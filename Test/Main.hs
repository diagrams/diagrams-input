module Main where

import qualified Data.Text as T
import Diagrams.SVG.ReadSVG
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Image
import Filesystem.Path.CurrentOS (encodeString, decodeString, toText)
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath)
import System.Environment
import System.Directory
import Paths_diagramsInputTest

main = do
  path <- getDataFileName "svgs"
  fileNames <- getDirectoryContents path
  let resourceFiles = map ("svgs/" ++)  $ filter (\x -> x /= "." && x /= "..") fileNames
  files <- mapM getDataFileName resourceFiles
  putStr $ show files

  images <- mapM loadImageEmb (map decodeString files)
  mainWith (head (map img images))

img :: Either String (DImage SVG Double Embedded) -> Diagram SVG
img im = case im of Left err -> mempty
                    Right i -> image i

