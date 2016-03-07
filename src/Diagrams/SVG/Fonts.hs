module Diagrams.SVG.Fonts
    (
      -- * Drawing text
      textSVG, textSVG', textSVG_

      -- * Options
    , TextOpts(..), Mode(..), Spacing(..)

      -- * Provided fonts
    , bit, lin, fab

      -- * Loading fonts
    , loadFont
    )
    where

import Diagrams.SVG.Fonts.Text
import Diagrams.SVG.Fonts.Fonts    (bit, lin, fab)
import Diagrams.SVG.Fonts.ReadFont (loadFont)
