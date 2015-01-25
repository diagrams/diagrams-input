svg-diagrams
============

A SVG parser for diagrams using xml-conduit and attoparsec. The goal is to support all tags and features that are commonly used and that are supported by diagrams. For another excellent svg-parser, that is currently not targeted at `diagrams` see: https://github.com/Twinside/svg-tree.


Main library function
======================
```xml
readSVGFile :: FilePath -> Double -> Double -> PreserveAR -> IO (Diagram B R2)
readSVGFile fp width height preserveAR =
```

To understand preserveAR, look at 
[SVG Spec: Preserve Aspect ratio](http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute)
and the following example:

```xml
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
    diagram0FromSVG <- readSVGFile "svgs/SVG_logo.svg" 100 100 min

    mainWith $ diagram0FromSVG
```
