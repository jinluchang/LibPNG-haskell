module Main where

import System.Console.GetOpt
import System.Exit

import Verbosity
import Flags
import Codec.Image.LibPNG

waterColor :: Int -> Int -> Pixel
waterColor x' y' = colorPixel 255 r 130 60 where
    r = round $ sin ( x * y / 3000 :: Double ) ^ (2::Int) * 255
    x = fromIntegral x'
    y = fromIntegral y'

spectrum :: [[Pixel]]
spectrum = replicate 40 $ map (\g -> colorPixel 255 255 g 0) [0..255]
                              ++ map (\r -> colorPixel 255 r 255 0) [254,253..1]
                              ++ map (\b -> colorPixel 255 0 255 b) [0..255]
                              ++ map (\g -> colorPixel 255 0 g 255) [254,253..0]

halfSize :: [[a]] -> [[a]]
halfSize = map half . half where
    half (x:_:xs) = x : half xs
    half xs = xs

main :: IO ()
main = do
    whenLoud $ putStrLn "Hello world."
    (flags, _, _) <- processOptions
    if not $ flagHelp flags
        then return ()
        else do
            putStrLn $ flip usageInfo options $
                "Usage: program [options]\n" ++
                "It does some things.\n"
            exitSuccess
    if not $ flagTest flags
        then return ()
        else do
            putStrLn "Start testing."
            exitSuccess
    let water = map (\x -> map (\y -> waterColor x y) [-256..256]) [-256..256]
    writePNGFromPixelss "Water.png" water
    image <- imageFromPixelss spectrum
    writePNGImage "Spectrum.png" image
    pixelss <- readPixelssFromPNG "Water.png"
    writePNGFromPixelss "Water-small.png" $ halfSize pixelss
    whenLoud $ putStrLn "Goodbye world."
    return ()
