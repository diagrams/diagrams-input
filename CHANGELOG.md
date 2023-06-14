# v0.1.2 (14 June 2023)

- Add new function `readSVGLBS` to read an SVG directly from a lazy
  `ByteString` (thanks to Igor Moreno)
- Fix parsing bug that was causing some paths to be read
  incorrectly ([#17](https://github.com/diagrams/diagrams-input/issues/17))
- Test with GHC 9.6

# v0.1.1 (8 Jan 2023)

- Fix example in documentation
- Fix `readSVGFile` function to return `Left` for any exception
  generated during parsing (thanks to Hans Roland Senn)

# v0.1 (25 March 2022)

- initial release
