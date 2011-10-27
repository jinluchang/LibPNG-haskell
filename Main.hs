module Main where

import LibPNG

waterColor :: Int -> Int -> Pixel
waterColor x' y' = colorPixel 255 r 130 60 where
    r = round $ sin ( x * y / 3000 :: Double ) ^ (2::Int) * 255
    x = fromIntegral x'
    y = fromIntegral y'

main :: IO ()
main = do
    let pixelss = replicate 40 $ map (\g -> colorPixel 255 255 g 0) [0..255]
                              ++ map (\r -> colorPixel 255 r 255 0) [254,253..1]
                              ++ map (\b -> colorPixel 255 0 255 b) [0..255]
                              ++ map (\g -> colorPixel 255 0 g 255) [254,253..0]
    let water = map (\x -> map (\y -> waterColor x y) [-256..256]) [-256..256]
    writePNGFromPixelss "Water.png" water
    image <- imageFromPixelss pixelss
    writePNGImage "spectrum.png" image
    pixelss' <- readPixelssFromPNG "spectrum.png"
    writePNGFromPixelss "spectrum-copy.png" pixelss'
