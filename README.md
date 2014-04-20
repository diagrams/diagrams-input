svg-diagrams
============

Parsing SVG files with xml-conduit and attoparsec


Issues
=======

To load a SVG file into your diagram you currently have to delete the xmlns namespace in the SVG tag and delete all attributes that have prefixes. E.g. edit the following:

svg
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:cc="http://web.resource.org/cc/"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:svg="http://www.w3.org/2000/svg"
   xmlns="http://www.w3.org/2000/svg"
   xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
   xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
   width="300"
   height="300"
   id="svg2"
   sodipodi:version="0.32"
   inkscape:version="0.45.1"
   version="1.0"
   inkscape:output_extension="org.inkscape.output.svg.inkscape"

into:

svg
   width="300"
   height="300"
   id="svg2"
   version="1.0"

This will not be necessary with a new version of xml-conduit.
Another problem is that css is not parsed yet. Some SVGs use CSS-classses for colouring.
But then it is likely that it works.






Main library function:
======================

readSVGFile :: FilePath -> Double -> Double -> PreserveAR -> IO (Diagram B R2)
readSVGFile fp width height preserveAR =

Usage example:

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
