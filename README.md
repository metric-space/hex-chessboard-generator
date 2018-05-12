# Hexagonal Chessboard Image Generator

Uses the power of the [Diagrams Library](https://archives.haskell.org/projects.haskell.org/diagrams/) to generate a hexagonal chessboard image (SVG) of configurable size (both image size and chessboard size)

## Example 

![alt text](https://github.com/metric-space/hex-chessboard-generator/raw/master/images/hex_chessboard.png "example of hex-chessboard")


## Usage

`stack build && stack exec diagramstutorial-exe` should by default give out a svg of image size 800x800 of a minl = 3, maxl = 10

`stack exec -- stack exec -- diagramstutorial-exe --help` for info on how to pass in info via flags


## Initial Inspiration
Inspired by [https://pritschet.me/wiki/python/example-scripts-python/2d-graphics-cairo/](https://pritschet.me/wiki/python/example-scripts-python/2d-graphics-cairo/).
Though approach to generate is completely different. 
