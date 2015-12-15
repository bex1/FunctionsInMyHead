module Colors where

import Data.Bits
import Data.Word (Word8)
import Graphics.UI.SDL as SDL

red, green, blue, black, white, yellow, cyan, brown, orange :: SDL.Pixel
red = SDL.Pixel 0xFF0000
green = SDL.Pixel 0x00FF00
blue = SDL.Pixel 0x0000FF
black = SDL.Pixel 0x000000
white = SDL.Pixel 0xFFFFFF
yellow = SDL.Pixel 0xFFFF00
cyan = SDL.Pixel 0x00FFFF
brown = SDL.Pixel 0xA52A2A
orange = SDL.Pixel 0xFFA500

windowBackgroundColor :: SDL.Pixel
windowBackgroundColor = black

wellBackgroundColor :: SDL.Pixel
wellBackgroundColor = SDL.Pixel 0xDDDDDD

solidBlockColor :: SDL.Pixel
solidBlockColor = SDL.Pixel 0x333333

textColor :: SDL.Color
textColor = SDL.Color 255 255 255
