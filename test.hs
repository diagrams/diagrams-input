{-# LANGUAGE OverloadedStrings #-}

module Main where
import Diagrams.SVG.ReadSVG
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Environment
import Filesystem.Path.CurrentOS
import Diagrams.SVG.Attributes (PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..))

main = do
    let min = PAR (AlignXY PMid PMin) Meet
    let mid = PAR (AlignXY PMid PMid) Meet
    let max = PAR (AlignXY PMax PMax) Meet
    diagram0FromSVG <- readSVGFile "svgs/SVG_logo.svg" 100 100               min
    diagram1FromSVG <- readSVGFile "svgs/haskell.svg" 100 100                mid
    diagram2FromSVG <- readSVGFile "svgs/Hello_Kitty_logo.svg" 100 100       max
    diagram3FromSVG <- readSVGFile "svgs/Commons_logo_optimized.svg" 100 100 min
    diagram4FromSVG <- readSVGFile "svgs/Commons_logo_optimized.svg" 100 100 max
    diagram5FromSVG <- readSVGFile "svgs/RWTH_Logo_3.svg" 100 100            max
    diagram6FromSVG <- readSVGFile "svgs/RWTH_Logo_3.svg" 100 100            mid
    diagram7FromSVG <- readSVGFile "svgs/web.svg" 100 100                    min
    diagram8FromSVG <- readSVGFile "svgs/web.svg" 100 100                    mid

    mainWith $
      (diagram0FromSVG ||| diagram1FromSVG ||| diagram2FromSVG) ===
      ( (diagram3FromSVG === diagram4FromSVG) |||
        ((diagram5FromSVG ||| diagram6FromSVG) ===
         (diagram7FromSVG ||| diagram8FromSVG)) )

