module Colors where

import Graphics.Declarative.Form (Color)

-- Stolen from Elm


fromRGB :: (Double,Double,Double) -> Color
fromRGB (r,g,b) = (r/255, g/255, b/255)

lightRed = fromRGB (239,41,41)
red = fromRGB (204,0,0)
darkRed = fromRGB (164,0,0)

lightOrange = fromRGB (252,175,62)
orange = fromRGB (245,121,0)
darkOrange = fromRGB (206,92,0)

lightYellow = fromRGB (255,233,79)
yellow = fromRGB (237,212,0)
darkYellow = fromRGB (196,160,0)

lightGreen = fromRGB (138,226,52)
green = fromRGB (115,210,22)
darkGreen = fromRGB (78,154,6)

lightBlue = fromRGB (114,159,207)
blue = fromRGB (52,101,164)
darkBlue = fromRGB (32,74,135)

lightPurple = fromRGB (173,127,168)
purple = fromRGB (117,80,123)
darkPurple = fromRGB (92,53,102)

lightBrown = fromRGB (233,185,110)
brown = fromRGB (193,125,17)
darkBrown = fromRGB (143,89,2)

black, white :: Color
black = (0,0,0)
white = (1,1,1)

lightGrey = fromRGB (238,238,236)
grey = fromRGB (211,215,207)
darkGrey = fromRGB (186,189,182)

lightGray = fromRGB (238,238,236)
gray = fromRGB (211,215,207)
darkGray = fromRGB (186,189,182)

lightCharcoal = fromRGB (136,138,133)
charcoal = fromRGB (85,87,83)
darkCharcoal = fromRGB (46,52,54)


allColors =
  [lightRed
  ,red
  ,darkRed
  ,lightOrange
  ,orange
  ,darkOrange
  ,lightYellow
  ,yellow
  ,darkYellow
  ,lightGreen
  ,green
  ,darkGreen
  ,lightBlue
  ,blue
  ,darkBlue
  ,lightPurple
  ,purple
  ,darkPurple
  ,lightBrown
  ,brown
  ,darkBrown
  ,black
  ,white
  ,lightGrey
  ,grey
  ,darkGrey
  ,lightGray
  ,gray
  ,darkGray
  ,lightCharcoal
  ,charcoal
  ,darkCharcoal
  ]


allColorsRandomOrder =
  [charcoal
  ,red
  ,brown
  ,darkYellow
  ,darkGrey
  ,darkGreen
  ,lightPurple
  ,green
  ,lightGreen
  ,lightRed
  ,purple
  ,lightBrown
  ,lightOrange
  ,gray
  ,darkOrange
  ,blue
  ,darkPurple
  ,darkBrown
  ,darkBlue
  ,yellow
  ,darkGray
  ,orange
  ,grey
  ,darkRed
  ,darkCharcoal
  ]
