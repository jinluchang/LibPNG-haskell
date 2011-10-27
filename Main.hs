module Main where

import LibPNG

main :: IO ()
main = do
    let pixelss = replicate 40 $ map (\g -> colorPixel 255 255 g 0) [0..255]
                              ++ map (\r -> colorPixel 255 r 255 0) [254,253..1]
                              ++ map (\b -> colorPixel 255 0 255 b) [0..255]
                              ++ map (\g -> colorPixel 255 0 g 255) [254,253..0]
    image <- imageFromPixelss pixelss
    writePNGImage "spectrum.png" image
    pixelss' <- readPixelssFromPNG "spectrum.png"
    writePNGFromPixelss "spectrum-copy.png" pixelss'
