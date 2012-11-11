{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}
module Test.Control where

import Test.HUnit
import Data.List (intercalate)
import Debian.Control
import Debian.Control.PrettyPrint (ppControl, ppParagraph)
import Debian.Control.String ({- Pretty instances -})
import Text.PrettyPrint.Class (pretty)

deriving instance Eq (Control' String)
deriving instance Show (Control' String)
deriving instance Show (Paragraph' String)
deriving instance Show (Field' String)

-- Additional tests of the results of parsing additional
-- inter-paragraph newlines, or missing terminating newlines, would be
-- good.
controlTests =
    [ TestCase (assertEqual "pretty1" control (either (error "parser failed") id (parseControl "debian/control" sample)))
    , TestCase (assertEqual "pretty2" sample (show (ppControl control)))
    , TestCase (assertEqual "pretty3" (head paragraphs ++ "\n") (show (ppParagraph (head (unControl control)))))
    -- The Pretty class instances are distinct implementations from
    -- those in Debian.Control.PrettyPrint.  Not sure why, there is a
    -- terse note about performance concerns.
    , TestCase (assertEqual "pretty4" sample (show (pretty control)))
    , TestCase (assertEqual "pretty5" (head paragraphs ++ "\n") (show (pretty (head (unControl control)))))  ]

-- | These paragraphs have no terminating newlines.  They are added
-- where appropriate to the expected test results.
paragraphs =
    [ "Source: haskell-debian\nSection: haskell\nPriority: extra\nMaintainer: Debian Haskell Group <pkg-haskell-maintainers@lists.alioth.debian.org>\nUploaders: Joachim Breitner <nomeata@debian.org>\nBuild-Depends: debhelper (>= 7)\n  , cdbs\n  , haskell-devscripts (>= 0.7)\n  , ghc\n  , ghc-prof\n  , libghc-hunit-dev\n  , libghc-hunit-prof\n  , libghc-mtl-dev\n  , libghc-mtl-prof\n  , libghc-parsec3-dev\n  , libghc-parsec3-prof\n  , libghc-pretty-class-dev\n  , libghc-pretty-class-prof\n  , libghc-process-extras-dev (>= 0.4)\n  , libghc-process-extras-prof (>= 0.4)\n  , libghc-regex-compat-dev\n  , libghc-regex-compat-prof\n  , libghc-regex-tdfa-dev (>= 1.1.3)\n  , libghc-regex-tdfa-prof\n  , libghc-bzlib-dev (>= 0.5.0.0-4)\n  , libghc-bzlib-prof\n  , libghc-haxml-prof (>= 1:1.20)\n  , libghc-unixutils-dev (>= 1.50)\n  , libghc-unixutils-prof (>= 1.50)\n  , libghc-zlib-dev\n  , libghc-zlib-prof\n  , libghc-network-dev (>= 2.4)\n  , libghc-network-prof (>= 2.4)\n  , libghc-utf8-string-dev\n  , libghc-utf8-string-prof,\n  , libcrypto++-dev\nBuild-Depends-Indep: ghc-doc\n  , libghc-hunit-doc\n  , libghc-mtl-doc\n  , libghc-parsec3-doc\n  , libghc-pretty-class-doc\n  , libghc-process-extras-doc (>= 0.4)\n  , libghc-regex-compat-doc\n  , libghc-regex-tdfa-doc\n  , libghc-bzlib-doc\n  , libghc-haxml-doc (>= 1:1.20)\n  , libghc-unixutils-doc (>= 1.50)\n  , libghc-zlib-doc\n  , libghc-network-doc (>= 2.4)\n  , libghc-utf8-string-doc\nStandards-Version: 3.9.2\nHomepage: http://hackage.haskell.org/package/debian\nVcs-Darcs: http://darcs.debian.org/pkg-haskell/haskell-debian\nVcs-Browser: http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-debian",
      "Package: libghc-debian-dev\nArchitecture: any\nDepends: ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nProvides: ${haskell:Provides}\nDescription: Haskell library for working with the Debian package system\n This package provides a library for the Haskell programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the libraries compiled for GHC 6.",
      "Package: libghc-debian-prof\nArchitecture: any\nDepends: ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nProvides: ${haskell:Provides}\nDescription: Profiling library for working with the Debian package system\n This package provides a library for the Haskell programming language,\n compiled for profiling.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the profiling libraries compiled for GHC 6.",
      "Package: libghc-debian-doc\nSection: doc\nArchitecture: all\nDepends: ${misc:Depends}, ${haskell:Depends}\nRecommends: ${haskell:Recommends}\nSuggests: ${haskell:Suggests}\nDescription: Documentation for Debian package system library\n This package provides the documentation for a library for the Haskell\n programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the library documentation.",
      "Package: haskell-debian-utils\nSection: devel\nArchitecture: any\nDepends: ghc, ${misc:Depends}, ${shlibs:Depends}\nRecommends: apt-file\nDescription: Various helpers to work with Debian packages\n This package contains tools shipped with the Haskell library \8220debian\8221:\n .\n   * fakechanges:\n     Sometimes you have the .debs, .dsc, .tar.gz, .diff.gz, etc from a package\n     build, but not the .changes file. This package lets you create a fake\n     .changes file in case you need one.\n .\n   * debian-report:\n     Analyze Debian repositories and generate reports about their contents and\n     relations. For example, a list of all packages in a distribution that are\n     trumped by another distribution.\n .\n   * cabal-debian:\n     Tool for creating debianizations of Haskell packages based on the .cabal\n     file.  If apt-file is installed it will use it to discover what is the\n     debian package name of a C library.\n .\n   * apt-get-build-depends:\n     Tool which will parse the Build-Depends{-Indep} lines from debian/control\n     and apt-get install the required packages" ]

sample :: String
sample = intercalate "\n\n" paragraphs ++ "\n"

-- | The expecte result of parsing the sample control file.
control =
    Control
      { unControl = [Paragraph [Field ("Source"," haskell-debian")
                               ,Field ("Section"," haskell")
                               ,Field ("Priority"," extra")
                               ,Field ("Maintainer"," Debian Haskell Group <pkg-haskell-maintainers@lists.alioth.debian.org>")
                               ,Field ("Uploaders"," Joachim Breitner <nomeata@debian.org>")
                               ,Field ("Build-Depends"," debhelper (>= 7)\n  , cdbs\n  , haskell-devscripts (>= 0.7)\n  , ghc\n  , ghc-prof\n  , libghc-hunit-dev\n  , libghc-hunit-prof\n  , libghc-mtl-dev\n  , libghc-mtl-prof\n  , libghc-parsec3-dev\n  , libghc-parsec3-prof\n  , libghc-pretty-class-dev\n  , libghc-pretty-class-prof\n  , libghc-process-extras-dev (>= 0.4)\n  , libghc-process-extras-prof (>= 0.4)\n  , libghc-regex-compat-dev\n  , libghc-regex-compat-prof\n  , libghc-regex-tdfa-dev (>= 1.1.3)\n  , libghc-regex-tdfa-prof\n  , libghc-bzlib-dev (>= 0.5.0.0-4)\n  , libghc-bzlib-prof\n  , libghc-haxml-prof (>= 1:1.20)\n  , libghc-unixutils-dev (>= 1.50)\n  , libghc-unixutils-prof (>= 1.50)\n  , libghc-zlib-dev\n  , libghc-zlib-prof\n  , libghc-network-dev (>= 2.4)\n  , libghc-network-prof (>= 2.4)\n  , libghc-utf8-string-dev\n  , libghc-utf8-string-prof,\n  , libcrypto++-dev")
                               ,Field ("Build-Depends-Indep"," ghc-doc\n  , libghc-hunit-doc\n  , libghc-mtl-doc\n  , libghc-parsec3-doc\n  , libghc-pretty-class-doc\n  , libghc-process-extras-doc (>= 0.4)\n  , libghc-regex-compat-doc\n  , libghc-regex-tdfa-doc\n  , libghc-bzlib-doc\n  , libghc-haxml-doc (>= 1:1.20)\n  , libghc-unixutils-doc (>= 1.50)\n  , libghc-zlib-doc\n  , libghc-network-doc (>= 2.4)\n  , libghc-utf8-string-doc")
                               ,Field ("Standards-Version"," 3.9.2")
                               ,Field ("Homepage"," http://hackage.haskell.org/package/debian")
                               ,Field ("Vcs-Darcs"," http://darcs.debian.org/pkg-haskell/haskell-debian")
                               ,Field ("Vcs-Browser"," http://darcs.debian.org/cgi-bin/darcsweb.cgi?r=pkg-haskell/haskell-debian")]
                    ,Paragraph [Field ("Package"," libghc-debian-dev")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Provides"," ${haskell:Provides}")
                               ,Field ("Description"," Haskell library for working with the Debian package system\n This package provides a library for the Haskell programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the libraries compiled for GHC 6.")]
                    ,Paragraph [Field ("Package"," libghc-debian-prof")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ${haskell:Depends}\n  , ${shlibs:Depends}\n  , ${misc:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Provides"," ${haskell:Provides}")
                               ,Field ("Description"," Profiling library for working with the Debian package system\n This package provides a library for the Haskell programming language,\n compiled for profiling.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the profiling libraries compiled for GHC 6.")],
                     Paragraph [Field ("Package"," libghc-debian-doc")
                               ,Field ("Section"," doc")
                               ,Field ("Architecture"," all")
                               ,Field ("Depends"," ${misc:Depends}, ${haskell:Depends}")
                               ,Field ("Recommends"," ${haskell:Recommends}")
                               ,Field ("Suggests"," ${haskell:Suggests}")
                               ,Field ("Description"," Documentation for Debian package system library\n This package provides the documentation for a library for the Haskell\n programming language.\n See http://www.haskell.org/ for more information on Haskell.\n .\n This library includes modules covering almost every aspect of the Debian\n packaging system, including low level data types such as version numbers\n and dependency relations, on up to the types necessary for computing and\n installing build dependencies, building source and binary packages,\n and inserting them into a repository.\n .\n This package contains the library documentation.")],
                     Paragraph [Field ("Package"," haskell-debian-utils")
                               ,Field ("Section"," devel")
                               ,Field ("Architecture"," any")
                               ,Field ("Depends"," ghc, ${misc:Depends}, ${shlibs:Depends}")
                               ,Field ("Recommends"," apt-file")
                               ,Field ("Description"," Various helpers to work with Debian packages\n This package contains tools shipped with the Haskell library \8220debian\8221:\n .\n   * fakechanges:\n     Sometimes you have the .debs, .dsc, .tar.gz, .diff.gz, etc from a package\n     build, but not the .changes file. This package lets you create a fake\n     .changes file in case you need one.\n .\n   * debian-report:\n     Analyze Debian repositories and generate reports about their contents and\n     relations. For example, a list of all packages in a distribution that are\n     trumped by another distribution.\n .\n   * cabal-debian:\n     Tool for creating debianizations of Haskell packages based on the .cabal\n     file.  If apt-file is installed it will use it to discover what is the\n     debian package name of a C library.\n .\n   * apt-get-build-depends:\n     Tool which will parse the Build-Depends{-Indep} lines from debian/control\n     and apt-get install the required packages")]]}
