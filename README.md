# diagrams-input

diagrams-input provides functions to parse several input formats for diagrams:
- Raster images: PNG, JPG, ...  (using JuicyPixels)
- SVG (using [xml-conduit](https://github.com/snoyberg/xml/tree/master/xml-conduit) and [attoparsec](https://github.com/bos/attoparsec))

In the future it would be nice to have:
- HTML + CSS (We need CSS anyway if we want to implement the SVG parser properly).
  HTML could be a good exercise for developing layouting functions.
- PDF
- Collada (3d)
- Obj (3d)

# Usage

See the [diagrams manual] (http://projects.haskell.org/diagrams/doc/manual.html#images).

# Development
The SVG parser evolved like maybe most others also did: By taking some SVG images, focussing on one image, getting it displayed correctly. See if the changes affect other images positively. Figuring out what is the most important thing to fix next. The SVG 1.1 spec was used.
For testing purposes [diagrams-input-test] (https://github.com/diagrams/diagrams-input-test) reads and writes all images in a directory.

## TODO
- [ ] Implement \<text\>-tag in the svg parser: Passing through without generating the outlines
- [ ] fill = "none" => path open
- [ ] Scale viewbox to width and height
- [ ] Bug in arc command
- [ ] Use patched xml-conduit instead of my patched version in the Text folder
- [ ] Transform exceptions into Left values in Image.hs (if monad transformers and conduit is easy for you please help me)
- [ ] inherit-attribute
- [ ] display-attribute

## A Walk through the Code
1. [Input.hs](https://github.com/diagrams/diagrams-input/blob/master/src/Diagrams/TwoD/Input.hs) contains the main functions [loadimageEmbedded](https://github.com/diagrams/diagrams-input/blob/21b58f8bfed86e0a96865848c680d465027a638e/src/Diagrams/TwoD/Input.hs#L34) and [loadImageExternal](https://github.com/diagrams/diagrams-input/blob/21b58f8bfed86e0a96865848c680d465027a638e/src/Diagrams/TwoD/Input.hs#L44). They call [readimage](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/TwoD/Image.hs#L59) from [JuicyPixels](https://github.com/Twinside/Juicy.Pixels) and [readSVGFile](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/ReadSVG.hs#L184-L186) from [ReadSVG.hs](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/ReadSVG.hs).
2. In [ReadSVG.hs](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/ReadSVG.hs) the xml file is [parsed](https://github.com/diagrams/diagrams-input/blob/d8e2d9ee91b0e23fa1fea69d892101395cd5f8e7/src/Diagrams/SVG/ReadSVG.hs#L210) and translated into a [tree](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/Tree.hs#L52-L84). That was necessary because there need to be at least two passes because of references with the \<use\>-tag. The tree has Constructors that take functions that expect data (like css) that is only known after the first pass.
3. All the [nodes](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/Tree.hs#L132-L157) of the tree are stored in a [key value storage](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/ReadSVG.hs#L195-L197). 
   Every node contains the whole subtree, but this is no problem because of lazy evaluation
4. Gradients that have nested references to other gradients are [flattened](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/Tree.hs#L193-L243) into one gradient.
5. References are [inserted](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/Tree.hs#L256-L281) using the key-value-storage and everything is combined into one [diagram](https://github.com/diagrams/diagrams-input/blob/afcb278dbbaee2d58deacb58d9294810bb7606c0/src/Diagrams/SVG/ReadSVG.hs#L191-L192).

## Other SVG Parsers
- Haskell: [svg-tree] (https://github.com/Twinside/svg-tree) (currently not targeted at `diagrams`)
- Java: [Processing] (https://github.com/processing/processing/blob/7f63dad6f21db722fe98d7a1f2afaa3de6c17b4c/core/src/processing/core/PShapeSVG.java)
- Java: [Apache Batik] (https://github.com/apache/batik)
- Javascript: [canvg] (https://github.com/gabelerner/canvg)
