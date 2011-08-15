module Test.VersionPolicy where

import Test.HUnit

import Debian.Version
import Debian.VersionPolicy

-- * Tag parsing

versionPolicyTests =
    map (\ (vendor, release, versionString) ->
             let version = (parseDebianVersion versionString) in
             TestCase (assertEqual versionString (rebuildTag vendor version) version)) versionStrings ++
    [ TestCase (assertEqual
                "setTag"
                (parseDebianVersion "1.2-3seereason4~feisty7")
                (either (error "setTag failed!") id
                            (setTag id "seereason" (Just "feisty") Nothing 
                             -- version currently uploaded to the build release
                             (Just (parseDebianVersion "1.2-3seereason4~feisty4"))
                             -- All the versions in the repository
                             [parseDebianVersion "1.2-3seereason4~feisty5",
                              parseDebianVersion "1.2-3seereason4~feisty6"] 
                             -- The version retrieved from the changelog
		             (parseDebianVersion "1.2-3seereason4"))))
    , TestCase (assertEqual
                "setTag2"
                (Right (parseDebianVersion "3000.0.2.1-2+6seereason1~feisty1"))
                (setTag id "seereason" (Just "feisty") Nothing
                        (Just (parseDebianVersion "3000.0.2.1-1+6seereason1~feisty3"))
                        [parseDebianVersion "3000.0.2.1-1+6seereason3",
                         parseDebianVersion "3000.0.2.1-1+6seereason1~feisty3",
                         parseDebianVersion "3000.0.2.1-1+6seereason1~feisty2",
                         parseDebianVersion "3000.0.2.1-1+6seereason1~feisty1",
                         parseDebianVersion "3000.0.2.1-1+6seereason0~gutsy4",
                         parseDebianVersion "3000.0.2.1-1+6seereason0~gutsy3",
                         parseDebianVersion "3000.0.2.1-1+6seereason0~gutsy2",
                         parseDebianVersion "3000.0.2-2+6"]
                        (parseDebianVersion "3000.0.2.1-2+6")))                 
    , TestCase (assertEqual
                "setTag"
                (parseDebianVersion "0.4.0.1")
                (fst (parseTag "seereason" (parseDebianVersion "0.4.0.1-0seereason1"))))
    , TestCase (assertEqual
                "appendTag (parseTag \"seereason\" (parseDebianVersion \"0.4.0.1-0seereason1\")) -> \"0.4.0.1-0seereason1\""
                (parseDebianVersion "0.4.0.1-0seereason1")
                (uncurry (appendTag id) (parseTag "seereason" (parseDebianVersion "0.4.0.1-0seereason1"))))
    , TestCase (assertEqual
                "setTag \"seereason\" (Just \"gutsy\") \"0.4.0.1-0seereason1\" -> \"0.4.0.1-0seereason1~gutsy1\""
                (parseDebianVersion "0.4.0.1-0seereason1~gutsy1")
                (either (error "setTag failed!") id
                            (setTag id "seereason" (Just "gutsy") Nothing 
                             -- version currently uploaded to the build release
                             Nothing
                             -- All the versions in the repository
                             [] 
                             -- The version retrieved from the changelog
		             (parseDebianVersion "0.4.0.1-0seereason1")))) ]

versionStrings = [ ("seereason", Just "feisty", "1.2-3seereason4~feisty5")
                 , ("seereason", Just "feisty", "1.2-3")
                 , ("seereason", Nothing, "1.2-3seereason4")
                 , ("seereason", Nothing, "1.2-0seereason4") ]

rebuildTag vendor version = 
    case parseTag vendor version of
      (version, Just tag) -> appendTag id version (Just tag)
      (version, Nothing) -> version
