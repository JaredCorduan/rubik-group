{-|
= Meffert's Gear Ball

 <<https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/The_Gear_Ball.jpg/277px-The_Gear_Ball.jpg>>
-}
module RubikGroup.GearBall where

import RubikGroup (mkRubik, IRubik, Rubik (illegal), Move (..))

-- |Gear Ball Forward Move, defined as
--
-- @
--   mkRubik [F, B]
-- @
gbF :: IRubik
gbF = illegal $ mkRubik [F, B]

-- |Gear Ball Up Move, defined as
--
-- @
--   mkRubik [U, D]
-- @
gbU :: IRubik
gbU = illegal $ mkRubik [U, D]

-- |Gear Ball Right Move, defined as
--
-- @
--   mkRubik [R, L]
-- @
gbR :: IRubik
gbR = illegal $ mkRubik [R, L]

-- |Move to swap edges.
--
-- @
--   corners
--   . . . . . . . .
--   0 1 2 3 4 5 6 7
--
--   edges
--    +  .  +  .  .  .  .  .  +  .  +  .
--    0  1  2  3  4  5  6  7  8  9 10 11
--    ↓     ↓     ↓  ↓  ↓  ↓  ↓     ↓
--    2     0     6  7  4  5 10     8
-- @
gbSwapEdges :: IRubik
gbSwapEdges = gbR <> gbR <> gbU <> gbF <> gbF <> gbU

-- |Move to solve the edge orientations.
--
-- @
--   corners
--   . . . . . . . .
--   0 1 2 3 4 5 6 7
--
--   edges
--    +  .  +  .  +  +  +  +  +  .  +  .
--    0  1  2  3  4  5  6  7  8  9 10 11
-- @
gbOrientEdges :: IRubik
gbOrientEdges = gbR <> gbF <> gbU <> gbR <> gbF <> gbU

