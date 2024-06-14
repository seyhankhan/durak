module Colour (module Colour) where

color :: Int -> String
color n = concat ["\ESC[", show n, "m"]

reset, red, green, yellow, blue, cyan :: String
reset = color 0
red = color 31
green = color 32
yellow = color 33
blue = color 34
cyan = color 36

bold :: String -> String
bold s = color 1 ++ s ++ reset

