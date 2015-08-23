{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
-- |Figure out the dependency relation between debianized source
-- directories.  The code to actually solve these dependency relations
-- for a particular set of binary packages is in Debian.Repo.Dependency.
module Debian.GenBuildDeps
    ( DepInfo(..)
    , sourceName'
    , relations'
    , binaryNames'
    -- * Preparing dependency info
    , buildDependencies
    , RelaxInfo
    , relaxDeps
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

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>))
#endif
import           Control.Exception (throw)
import           Control.Monad (filterM, foldM)
import           Control.Monad.State (evalState, get, modify, State)
import           Data.Graph (Graph, Edge, Vertex, buildG, topSort, reachable, transposeG, edges, scc)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Tree as Tree (Tree(Node, rootLabel, subForest))
import           Debian.Control (parseControlFromFile)
import           Debian.Control.Policy (HasDebianControl, DebianControl, ControlFileError(..), validateDebianControl, debianSourcePackageName, debianBinaryPackageNames, debianBuildDeps, debianBuildDepsIndep)
import           Debian.Loc (__LOC__)
import           Debian.Relation
import           Debian.Relation.Text ()
import           System.Directory (getDirectoryContents, doesFileExist)

-- | This type describes the build dependencies of a source package.
data DepInfo = DepInfo {
      sourceName :: SrcPkgName          -- ^ source package name
    , relations :: Relations            -- ^ dependency relations
    , binaryNames :: [BinPkgName]       -- ^ binary dependency names (is this a function of relations?)
    } deriving Show

instance Eq DepInfo where
    a == b = (sourceName a == sourceName b) &&
             Set.fromList (map Set.fromList (relations a)) == Set.fromList (map Set.fromList (relations b)) &&
             Set.fromList (binaryNames a) == Set.fromList (binaryNames b)

-- |Return the dependency info for a source package with the given dependency relaxation.
-- |According to debian policy, only the first paragraph in debian\/control can be a source package
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-sourcecontrolfiles>
buildDependencies :: HasDebianControl control => control -> DepInfo
buildDependencies control = do
  DepInfo { sourceName = debianSourcePackageName control
          , relations = concat [fromMaybe [] (debianBuildDeps control),
                                fromMaybe [] (debianBuildDepsIndep control)]
          , binaryNames = debianBinaryPackageNames control }

-- | source package name
sourceName' :: HasDebianControl control => control -> SrcPkgName
sourceName' control = debianSourcePackageName control

-- | dependency relations
relations' :: HasDebianControl control => control -> Relations
relations' control = concat [fromMaybe [] (debianBuildDeps control),
                            fromMaybe [] (debianBuildDepsIndep control)]

-- | binary dependency names (is this a function of relations?)
binaryNames' :: HasDebianControl control => control -> [BinPkgName]
binaryNames' control = debianBinaryPackageNames control

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
buildable :: forall a. (a -> DepInfo) -> [a] -> BuildableInfo a
buildable relax packages =
    -- Find all packages which can't reach any other packages in the
    -- graph of the "has build dependency" relation.
    case partition (\ x -> reachable hasDep x == [x]) verts of
      -- None of the packages are buildable, return information
      -- about how to break this build dependency cycle.
      ([], _) -> CycleInfo {depPairs = map ofEdge $ head $ (allCycles hasDep)}
      -- We have some buildable packages, return them along with
      -- the list of packages each one directly blocks
      (allReady, blocked) ->
          BuildableInfo { readyTargets = map (makeReady blocked allReady) allReady
                        , allBlocked = map ofVertex blocked }
    where
      makeReady :: [Vertex] -> [Vertex] -> Vertex -> ReadyTarget a
      makeReady blocked ready thisReady =
          let otherReady = filter (/= thisReady) ready
              (directlyBlocked, otherBlocked) = partition (\ x -> elem x (reachable isDep thisReady)) blocked in
          ReadyTarget { ready = ofVertex thisReady
                      , waiting = map ofVertex directlyBlocked
                      , other = map ofVertex (otherReady ++ otherBlocked) }
      --allDeps x = (ofVertex x, map ofVertex (filter (/= x) (reachable hasDep x)))
      isDep :: Graph
      isDep = transposeG hasDep
      hasDep :: Graph
      hasDep = buildG (0, length packages - 1) hasDepEdges

      hasDepEdges :: [(Int, Int)]
      hasDepEdges =
#if 0
          nub (foldr f [] (tails vertPairs))
          where f :: [(Int, DepInfo)] -> [(Int, Int)] -> [(Int, Int)]
                f [] es = es
                f (x : xs) es = catMaybes (map (toEdge x) xs) ++ es
                toEdge :: (Int, DepInfo) -> (Int, DepInfo) -> Maybe Edge
                toEdge (xv, xa) (yv, ya) =
                    case compareSource xa ya of
                      EQ -> Nothing
                      LT -> Just (yv, xv)
                      GT -> Just (xv, yv)
#else
          nub (evalState (foldM f [] (tails vertPairs)) Map.empty)
          where f :: [(Int, Int)] -> [(Int, DepInfo)] -> State (Map.Map (Int, Int) Ordering) [(Int, Int)]
                f es [] = return es
                f es (x : xs) = mapM (toEdge x) xs >>= \es' -> return (catMaybes es' ++ es)
                toEdge :: (Int, DepInfo) -> (Int, DepInfo) -> State (Map.Map (Int, Int) Ordering) (Maybe Edge)
                toEdge (xv, xa) (yv, ya) = do
                  mp <- get
                  r <- case Map.lookup (xv, yv) mp of
                         Just r' -> return r'
                         Nothing -> do
                           let r' = compareSource xa ya
                           modify (Map.insert (xv, yv) r')
                           return r'
                  case r of
                    EQ -> return Nothing
                    LT -> {-trace ("compareSource " ++ show xv ++ " " ++ show yv ++ " -> LT") $-} return $ Just (yv, xv)
                    GT -> {-trace ("compareSource " ++ show xv ++ " " ++ show yv ++ " -> GT") $-} return $ Just (xv, yv)
#endif
      ofEdge :: Edge -> (a, a)
      ofEdge (a, b) = (ofVertex a, ofVertex b)
      ofVertex :: Int -> a
      ofVertex n = fromJust (Map.findWithDefault Nothing n (Map.fromList (zip [0..] (map Just packages))))
      verts :: [Int]
      verts = map fst vertPairs
      vertPairs :: [(Int, DepInfo)]
      vertPairs = zip [0..] $ map relax packages

-- | Find a cycle in a graph that involves
allCycles :: Graph -> [[Edge]]
allCycles g =
    -- Every cycle is confined to an SCC (strongly connected component).
    -- Every node in an SCC is part of some cycle.
    concatMap sccCycles (scc g)
    where
      -- Find all the cycles in an SCC
      sccCycles :: Tree Vertex -> [[Edge]]
      sccCycles t = mapMaybe addBackEdge (treePaths t)

      addBackEdge :: [Vertex] -> Maybe [Edge]
      addBackEdge path@(root : _) =
          let back = (last path, root) in
          if elem back (edges g) then Just (pathEdges (path ++ [root])) else Nothing

-- | All the paths from root to a leaf
treePaths :: Tree a -> [[a]]
treePaths (Node {rootLabel = r, subForest = []}) = [[r]]
treePaths (Node {rootLabel = r, subForest = ts}) = map (r :) (concatMap treePaths ts)

pathEdges :: [a] -> [(a, a)]
pathEdges (v1 : v2 : vs) = (v1, v2) : pathEdges (v2 : vs)
pathEdges _ = []

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

compareSource' :: HasDebianControl control => control -> control -> Ordering
compareSource' control1 control2
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins2)) (concat depends1) = GT
    | any (\rel -> isJust (find (checkPackageNameReq rel) bins1)) (concat depends2) = LT
    | otherwise = EQ
    where
      bins1 = binaryNames' control1
      bins2 = binaryNames' control2
      depends1 = relations' control1
      depends2 = relations' control2
      checkPackageNameReq :: Relation -> BinPkgName -> Bool
      checkPackageNameReq (Rel rPkgName _ _) bPkgName = rPkgName == bPkgName

-- |Return the dependency info for a list of control files.
genDeps :: [FilePath] -> IO [DebianControl]
genDeps controlFiles = do
  orderSource compareSource' <$> mapM genDep' controlFiles
    where
      -- Parse the control file and extract the build dependencies
      genDep' controlPath = parseControlFromFile controlPath >>=
                            either (\ x -> throw (ParseRelationsError [$__LOC__] x))
                                   (\ x -> validateDebianControl x {- `mapExn` (pushLoc $__LOC__) -} >>= either throw return)

-- pushLoc :: Loc -> ControlFileError -> ControlFileError
-- pushLoc loc e = e {locs = loc : locs e}

-- |One example of how to tie the below functions together. In this
-- case 'fp' is the path to a directory that contains a bunch of
-- checked out source packages. The code will automatically look for
-- debian\/control. It returns a list with the packages in the
-- order they should be built.
getSourceOrder :: FilePath -> IO [SrcPkgName]
getSourceOrder fp =
    findControlFiles fp >>= genDeps >>= return . map sourceName'
    where
      -- Return a list of the files that look like debian\/control.
      findControlFiles :: FilePath -> IO [FilePath]
      findControlFiles root =
          getDirectoryContents root >>=
          mapM (\ x -> return $ root ++ "/" ++ x ++ "/debian/control") >>=
          filterM doesFileExist
