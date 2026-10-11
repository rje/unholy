module Unholy.Theme where

import Data.Word (Word32)

-- Unity-inspired charcoal surfaces, blue selection, warm error text.
background, panel, border, ink, muted, accent :: Word32
background=0x1b1c1fff
panel=0x26272bff
border=0x393b41ff
ink=0xdadde5ff
muted=0x9599a5ff
accent=0x62a1f6ff
rectangleColor, textColor, buttonColor, buttonBorder :: Int -> Word32
rectangleColor s=case s of 1->panel;2->border;3->accent;_->background
textColor s=case s of 1->muted;2->accent;3->0xf49983ff;_->ink
buttonColor s=case s of 1->0x365782ff;2->0x523b40ff;3->0x354f70ff;_->panel
buttonBorder s=if s==1 then 0x5087caff else border
-- raygui ABI enum values; all style choices belong to the Haskell theme.
styles :: [(Int,Int,Word32)]
styles=[(0,16,15),(0,17,0),(0,12,1),(0,0,border),(0,1,panel),(0,2,ink)
  ,(0,3,accent),(0,4,0x363b46ff),(0,5,ink),(0,6,accent),(0,7,0x34567fff),(0,8,0xffffffff)
  ,(9,1,background),(9,13,7),(2,14,0),(2,13,10),(4,17,0)]
