module Infsabot.Constants(
        colorOfEmptySpot, colorOfMaterialSpot,
        colorDefaultOf
    ) where

import Codec.Picture
import Infsabot.Base

-- The color of the empty spot
colorOfEmptySpot :: PixelRGB8
colorOfEmptySpot = PixelRGB8 255 255 255

-- The color of material
colorOfMaterialSpot :: PixelRGB8
colorOfMaterialSpot = PixelRGB8 128 128 128

colorDefaultOf :: Team -> PixelRGB8
colorDefaultOf A = PixelRGB8 255 0 0
colorDefaultOf B = PixelRGB8 0 0 255
