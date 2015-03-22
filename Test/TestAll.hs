{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TestAll where

import qualified Data.Text as T
import Diagrams.SVG.ReadSVG
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Image
import Distribution.TestSuite
import Filesystem.Path.CurrentOS (encodeString, decodeString, toText)
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath)
import System.Environment
import System.Directory
import Paths_diagrams_input

tests :: IO [Test]
tests = do
  path <- getDataFileName "svgs"
  fileNames <- getDirectoryContents path
  let resourceFiles = map ("svgs/" ++)  $ filter (\x -> x /= "." && x /= "..") fileNames
  files <- mapM getDataFileName resourceFiles
--  putStr $ show files
--  images <- mapM loadImageEmb (map decodeString files)
--  mainWith (head (map img images))
--   return $ zipWith (imageTest "imageTest") files images
  return []

-- img :: V2 ~ V b => Either String (DImage SVG Double Embedded) -> Diagram SVG
img im = case im of Left err -> mempty
                    Right i -> image i


-- imageTest :: V b ~ V2 => String -> FilePath -> Either String (DImage SVG Double Embedded) -> Test
imageTest testName file image = Test $ TestInstance
    { run = do
        return $ Finished $ case image of
            Left err -> Fail ("Loading " ++ show file ++ ". Error: " ++ err)
            Right _ -> Pass
    , name = testName
    , tags = []
    , options = []
    , setOption = \_ _ -> Left "no options supported"
    }

