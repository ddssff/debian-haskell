{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
-- |Figure out the dependency relation between debianized source
-- directories.  The code to actually solve these dependency relations
-- for a particular set of binary packages is in Debian.Repo.Dependency.
module Debian.GenBuildDeps 
    ( DepInfo(..)
    -- * Preparing dependency info
    , buildDependencies
    , RelaxInfo
    , relaxDeps
    , OldRelaxInfo(..)
    , oldRelaxDeps
    -- * Using dependency info
    , BuildableInfo(..)
    , ReadyTarget(..)
    , buildable
    , compareSource
    -- * Obsolete?
    , orderSource
    , genDeps
    , failPackage
    , getSourceOrder
    ) where

import		 Control.Applicative ((<$>), (<*>))
import		 Control.Monad (filterM)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either (EitherT, left, right, bimapEitherT)
import		 Data.Graph (Graph, Edge, buildG, topSort, reachable, transposeG, vertices, edges)
import		 Data.List
import qualified Data.Map as Map
import		 Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import		 Debian.Control (HasDebianControl, debianSourcePackageName, debianBinaryPackageNames, parseControlFromFile)
import		 Debian.Control.Policy (ControlFileError(..), debianBuildDeps, debianBuildDepsIndep)
import		 Debian.Relation
import		 Debian.Relation.Text ()
import		 System.Directory (getDirectoryContents, doesFileExist)

-- | This type describes the build dependencies of a source package.
data DepInfo = DepInfo {
      sourceName :: SrcPkgName		-- ^ source package name
    , relations :: Relations		-- ^ dependency relations
    , binaryNames :: [BinPkgName]	-- ^ binary dependency names (is this a function of relations?)
    }

-- |Return the dependency info for a source package with the given dependency relaxation.
-- |According to debian policy, only the first paragraph in debian\/control can be a source package
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-sourcecontrolfiles>
buildDependencies :: (Monad m, HasDebianControl control Text) => control -> EitherT ControlFileError m DepInfo
buildDependencies control = do
  (s, bd, bdi, bs) <- (,,,) <$> debianSourcePackageName control <*> debianBuildDeps control <*> debianBuildDepsIndep control <*> debianBinaryPackageNames control
  right $ DepInfo { sourceName = s, relations = concat [bd, bdi], binaryNames = bs}

-- |Specifies build dependencies that should be ignored during the build
-- decision.  If the pair is (BINARY, Nothing) it means the binary package
-- BINARY should always be ignored when deciding whether to build.  If the
-- pair is (BINARY, Just SOURCE) it means that binary package BINARY should
-- be ignored when deiciding whether to build package SOURCE.
newtype OldRelaxInfo = RelaxInfo [(BinPkgName, Maybe SrcPkgName)] deriving Show

-- | Given a source package name and a binary package name, return
-- False if the binary package should be ignored hwen deciding whether
-- to build the source package.  This is used to prevent build
-- dependency cycles from triggering unnecessary rebuilds.  (This is a
-- replacement for the RelaxInfo type, which we temporarily rename
-- OldRelaxInfo.)
type RelaxInfo = SrcPkgName -> BinPkgName -> Bool

_makeRelaxInfo :: OldRelaxInfo -> RelaxInfo
_makeRelaxInfo (RelaxInfo xs) srcPkgName binPkgName =
    Set.member binPkgName global || maybe False (Set.member binPkgName) (Map.lookup srcPkgName mp)
    where
      (global :: Set.Set BinPkgName, mp :: Map.Map SrcPkgName (Set.Set BinPkgName)) =
          foldr (\ entry (global', mp') ->
                     case entry of
                       (b, Just s) -> (global', Map.insertWith Set.union s (Set.singleton b) mp')
                       (b, Nothing) -> (Set.insert b global', mp')) (Set.empty, Map.empty) xs

-- |Remove any dependencies that are designated \"relaxed\" by relaxInfo.
relaxDeps :: RelaxInfo -> [DepInfo] -> [DepInfo]
relaxDeps relaxInfo deps =
    map relaxDep deps
    where
      relaxDep :: DepInfo -> DepInfo
      relaxDep info = info {relations = filteredDependencies}
          where
            -- Discard any dependencies not on the filtered package name list.  If
            -- this results in an empty list in an or-dep the entire dependency can
            -- be discarded.
            filteredDependencies :: Relations
            filteredDependencies = filter (/= []) (map (filter keepDep) (relations info))
            keepDep :: Relation -> Bool
            keepDep (Rel name _ _) = not (relaxInfo (sourceName info) name)

-- |Remove any dependencies that are designated \"relaxed\" by relaxInfo.
oldRelaxDeps :: OldRelaxInfo -> [DepInfo] -> [DepInfo]
oldRelaxDeps relaxInfo deps =
    map relaxDep deps
    where
      relaxDep :: DepInfo -> DepInfo
      relaxDep info = info {relations = filteredDependencies}
          where
            -- Discard any dependencies not on the filtered package name list.  If
            -- this results in an empty list in an or-dep the entire dependency can
            -- be discarded.
            filteredDependencies :: Relations
            filteredDependencies = filter (/= []) (map (filter keepDep) (relations info))
            keepDep :: Relation -> Bool
            keepDep (Rel name _ _) = not (elem name ignored)
            -- Binary packages to be ignored wrt this source package's build decision
            ignored = ignoredForSourcePackage (sourceName info) relaxInfo
            -- Return a list of binary packages which should be ignored for this
            -- source package.
            ignoredForSourcePackage :: SrcPkgName -> OldRelaxInfo -> [BinPkgName]
            ignoredForSourcePackage source (RelaxInfo pairs) =
                map fst . filter (maybe True (== source) . snd) $ pairs
                -- concat . map binaries . catMaybes . map snd . filter (\ (_, x) -> maybe True (== source) x) $ pairs

-- | 
data ReadyTarget a
    = ReadyTarget { ready :: a
                  -- ^ Some target whose build dependencies are all satisfied
                  , waiting :: [a]
                  -- ^ The targets that are waiting for the ready target
                  , other :: [a]
                  -- ^ The rest of the targets that need to be built
                  }

data BuildableInfo a
    = BuildableInfo
      { readyTargets :: [ReadyTarget a]
      , allBlocked :: [a] }
    | CycleInfo
      { depPairs :: [(a, a)] }

-- | Given an ordering function representing the dependencies on a
-- list of packages, return a ReadyTarget triple: One ready package,
-- the packages that depend on the ready package directly or
-- indirectly, and all the other packages.
buildable :: (a -> a -> Ordering) -> [a] -> BuildableInfo a
buildable cmp packages =
    -- Find all packages which can't reach any other packages in the
    -- graph of the "has build dependency" relation.
    case partition (\ x -> reachable hasDep x == [x]) verts of
      -- None of the packages are buildable, return information
      -- about how to break this build dependency cycle.
      ([], _) -> CycleInfo {depPairs = map ofEdge (cycleEdges hasDep)}
      -- We have some buildable packages, return them along with
      -- the list of packages each one directly blocks
      (allReady, blocked) ->
          BuildableInfo { readyTargets = map (makeReady blocked allReady) allReady
                        , allBlocked = map ofVertex blocked }
    where
      makeReady blocked ready thisReady =
          let otherReady = filter (/= thisReady) ready
              (directlyBlocked, otherBlocked) = partition (\ x -> elem x (reachable isDep thisReady)) blocked in
          ReadyTarget { ready = ofVertex thisReady
                      , waiting = map ofVertex directlyBlocked
                      , other = map ofVertex (otherReady ++ otherBlocked) }
      --allDeps x = (ofVertex x, map ofVertex (filter (/= x) (reachable hasDep x)))
      isDep = buildG (0, length packages - 1) edges'
      edges' = map (\ (a, b) -> (b, a)) edges''
      hasDep = buildG (0, length packages - 1) edges''
      edges'' :: [(Int, Int)]
      edges'' =
          nub (foldr f [] (tails vertPairs))
          where f [] es = es
                f (x : xs) es = catMaybes (map (toEdge x) xs) ++ es
                toEdge (xv, xa) (yv, ya) =
                    case cmp xa ya of
                      EQ -> Nothing
                      LT -> Just (yv, xv)
                      GT -> Just (xv, yv)
      ofEdge (a, b) = (ofVertex a, ofVertex b)
      ofVertex n = fromJust (Map.findWithDefault Nothing n (Map.fromList (zip [0..] (map Just packages))))
      verts :: [Int]
      verts = map fst vertPairs
      vertPairs = zip [0..] packages

cycleEdges :: Graph -> [Edge]
cycleEdges g =
    filter (`elem` (edges g))
               (Set.toList (Set.intersection
                            (Set.fromList (closure g))
                            (Set.fromList (closure (transposeG g)))))
    where
      closure g' = concat (map (\ v -> (map (\ u -> (v, u)) (reachable g' v))) (vertices g'))
      --self (a, b) = a == b
      --distrib = concat . map (\ (n, ms) -> map (\ m -> (n, m)) ms) 
      --swap (a, b) = (b, a)

-- | Remove any packages which can't be built given that a package has failed.
failPackage :: Eq a => (a -> a -> Ordering) -> a -> [a] -> ([a], [a])
failPackage cmp failed packages =
    let graph = buildGraph cmp packages in
    let root = elemIndex failed packages in
    let victims = maybe [] (map (fromJust . vertex) . reachable graph) root in
    partition (\ x -> not . elem x $ victims) packages
    where
      vertex n = Map.findWithDefault Nothing n vertexMap
      vertexMap = Map.fromList (zip [0..] (map Just packages))

-- | Given a list of packages, sort them according to their apparant
-- build dependencies so that the first element doesn't depend on any
-- of the other packages.
orderSource :: (a -> a -> Ordering) -> [a] -> [a]
orderSource cmp packages =
    map (fromJust . vertex) (topSort graph)
    where
      graph = buildGraph cmp packages
      vertex n = Map.findWithDefault Nothing n vertexMap
      vertexMap = Map.fromList (zip [0..] (map Just packages))

-- | Build a graph with the list of packages as its nodes and the
-- build dependencies as its edges.
buildGraph :: (a -> a -> Ordering) -> [a] -> Graph
buildGraph cmp packages =
    let es = someEdges (zip packages [0..]) in
    buildG (0, length packages - 1) es
    where
      someEdges [] = []
      someEdges (a : etc) = aEdges a etc ++ someEdges etc
      aEdges (ap, an) etc =
          concat (map (\ (bp, bn) ->
                           case cmp ap bp of
                             LT -> [(an, bn)]
                             GT -> [(bn, an)]
                             EQ -> []) etc)

-- |This is a nice start. It ignores circular build depends and takes
-- a pretty simplistic approach to 'or' build depends. However, I
-- think this should work pretty nicely in practice.
compareSource :: DepInfo -> DepInfo -> Ordering
compareSource (DepInfo {relations = depends1, binaryNames = bins1}) (DepInfo {relations = depends2, binaryNames = bins2})
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins2)) (concat depends1) = GT
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins1)) (concat depends2) = LT
    | otherwise = EQ
    where
      checkPackageNameReq :: Relation -> BinPkgName -> Bool
      checkPackageNameReq (Rel rPkgName _ _) bPkgName = rPkgName == bPkgName

-- |Return the dependency info for a list of control files.
genDeps :: [FilePath] -> EitherT ControlFileError IO [DepInfo]
genDeps controlFiles = do
  bimapEitherT id (orderSource compareSource) (mapM genDep' controlFiles)
    where
      -- Parse the control file and extract the build dependencies
      genDep' :: FilePath -> EitherT ControlFileError IO DepInfo
      genDep' controlPath = lift (parseControlFromFile controlPath) >>= either (left . ParseRelationsError) right >>= buildDependencies

-- |One example of how to tie the below functions together. In this
-- case 'fp' is the path to a directory that contains a bunch of
-- checked out source packages. The code will automatically look for
-- debian\/control. It returns a list with the packages in the
-- order they should be built.
getSourceOrder :: FilePath -> EitherT ControlFileError IO [SrcPkgName]
getSourceOrder fp =
    lift (findControlFiles fp) >>=
    bimapEitherT id (map sourceName . orderSource compareSource) . genDeps
    where
      -- Return a list of the files that look like debian\/control.
      findControlFiles :: FilePath -> IO [FilePath]
      findControlFiles root =
          getDirectoryContents root >>=
          mapM (\ x -> return $ root ++ "/" ++ x ++ "/debian/control") >>=
          filterM doesFileExist
