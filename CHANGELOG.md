# v0.1.5 (3 April 2025)

- Update SVG parser to account for optional comma separation of values ([#21](https://github.com/diagrams/diagrams-input/pull/21), thanks to @Chobbes)
- Allow `diagrams-lib-1.5` and test on GHC 9.12

# v0.1.4 (5 Nov 2024)

- Require `data-default-0.8`

# v0.1.3.1 (24 Sep 2024)

- Test up to GHC 9.10
- Fix a few warnings

# v0.1.3 (20 June 2023)

- Add new function `readSVGLBS` to read an SVG directly from a lazy
  `ByteString` (thanks to Igor Moreno)

# v0.1.2 (14 June 2023)

- Fix parsing bug that was causing some paths to be read
  incorrectly ([#17](https://github.com/diagrams/diagrams-input/issues/17))
- Test with GHC 9.6

# v0.1.1 (8 Jan 2023)

- Fix example in documentation
- Fix `readSVGFile` function to return `Left` for any exception
  generated during parsing (thanks to Hans Roland Senn)

# v0.1 (25 March 2022)

- initial release
