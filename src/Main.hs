module Main where

import Codec.Image.LibPNG

spectrum :: [[Pixel]]
spectrum = replicate 40 $ map (\g -> colorPixel 255 255 g 0) [0..255]
                              ++ map (\r -> colorPixel 255 r 255 0) [254,253..1]
                              ++ map (\b -> colorPixel 255 0 255 b) [0..255]
                              ++ map (\g -> colorPixel 255 0 g 255) [254,253..0]

halfSize :: [[a]] -> [[a]]
halfSize = map half . half where
    half (x:_:xs) = x : half xs
    half xs = xs

waterColor :: Double -> Double -> Pixel
waterColor x y = colorPixel 255 r 130 60 where
    r = round $ sin ( x * y / 3000 :: Double ) ^ (2::Int) * 255

water :: [[Pixel]]
water = [ [ waterColor x y | x <- [-256..255] ] | y <- [-256..255] ]

waterLarge :: Double -> [[Pixel]]
waterLarge scale = [ [ waterColor (scale*x) (scale*y) | x <- [-1280..1279] ] | y <- [-512..511] ]

main :: IO ()
main = do
    writePNGFromPixelss "Water.png" water
    image <- imageFromPixelss spectrum
    writePNGImage "Spectrum.png" image
    pixelss <- readPixelssFromPNG "Water.png"
    writePNGFromPixelss "Water-small.png" $ halfSize pixelss
    writePNGFromPixelss "Water-large.png" $ waterLarge 0.5
