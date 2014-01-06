-- | Count the packages in a control file, used for speed tests.
import Debian.Control.ByteString
import Data.Either
import Text.Printf
import System.Environment

main = put . count . rights =<< mapM parseControlFromFile =<< getArgs
  where count :: [Control] -> Int
        count = sum . map (length . unControl)
        put = putStrLn . printf "%d Packages"
