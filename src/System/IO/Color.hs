module System.IO.Color where

data Color
  = NoColor
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Ord, Eq)

data Attribute = Null | Bold | Underscore | Blink | Reverse | Concealed
  deriving (Show, Ord, Eq)

type FgColor = Color
type BgColor = Color
type Decoration = (FgColor, BgColor, Attribute)

decorate :: String -> Decoration -> String
decorate str dec = concat [escDec dec, str, esc 0]

esc :: Int -> String
esc n = concat ["\ESC[", show n, "m"]

escDec :: Decoration -> String
escDec (fg, bg, at) = concat [escFg fg, escBg bg, escAttribute at]

escFg :: FgColor -> String
escFg color = escColor color 0

escBg :: BgColor -> String
escBg color = escColor color 10

escColor :: Color -> Int -> String
escColor NoColor _ = ""
escColor Black offset = esc (30 + offset)
escColor Red offset = esc (31 + offset)
escColor Green offset = esc (32 + offset)
escColor Yellow offset = esc (33 + offset)
escColor Blue offset = esc (34 + offset)
escColor Magenta offset = esc (35 + offset)
escColor Cyan offset = esc (36 + offset)
escColor White offset = esc (37 + offset)

escAttribute :: Attribute -> String
escAttribute Null = ""
escAttribute Bold = esc 1
escAttribute Underscore = esc 4
escAttribute Blink = esc 5
escAttribute Reverse = esc 7
escAttribute Concealed = esc 8