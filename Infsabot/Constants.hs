module Infsabot.Constants(
        colorOfEmptySpot, colorOfMaterialSpot
    ) where

import Codec.Picture

-- The color of the empty spot
colorOfEmptySpot :: PixelRGB8
colorOfEmptySpot = PixelRGB8 255 255 255

-- The color of material
colorOfMaterialSpot :: PixelRGB8
colorOfMaterialSpot = PixelRGB8 128 128 128
