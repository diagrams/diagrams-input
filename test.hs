{-# LANGUAGE OverloadedStrings #-}

module Main where
import Diagrams.SVG.ReadSVG
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Environment
import Filesystem.Path.CurrentOS
import Diagrams.SVG.Attributes (PreserveAR(..), AlignSVG(..), Place(..), MeetOrSlice(..))

main = do
    -- args <- getArgs
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

--    diagram3FromSVG <- readSVGFile "VD_11_Filled_00.svg" 100 100        max -- nearly works
--    diagram2FromSVG <- readSVGFile "star.svg" 100 100                   max -- works
--    diagram9FromSVG <- readSVGFile "ge.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- no fill
--    diagram10FromSVG <- readSVGFile "money-bag-3.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- no fill
--    diagram11FromSVG <- readSVGFile "KFC_logo.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- black rect in front
--    diagram12FromSVG <- readSVGFile "ubuntu.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- no black circle
--    diagram13FromSVG <- readSVGFile "peace.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- bug
--    diagram14FromSVG <- readSVGFile "Bundesliga-Logo.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- bug
    -- diagram8FromSVG <- readSVGFile "lhc.svg" 100 100 (PAR (AlignXY PMin PMin) Meet) -- (decodeString (head args))
    -- diagram9FromSVG <- readSVGFile "Wikipedia-logo.svg" -- parse bug
                                   -- "partition.svg" -- bug
                                   -- "money-bag-3.svg" -- empty
    mainWith $
      (diagram0FromSVG ||| diagram1FromSVG ||| diagram2FromSVG) ===
      ( (diagram3FromSVG === diagram4FromSVG) |||
        ((diagram5FromSVG ||| diagram6FromSVG) ===
         (diagram7FromSVG ||| diagram8FromSVG)) )

