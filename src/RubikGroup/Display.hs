{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module RubikGroup.Display
  ( display
  ) where

import           RubikGroup

import           Data.Maybe                    (fromMaybe)
import           Data.Modular                  (Mod)
import           Data.Proxy                    (Proxy (..))
import           Data.Vector.Fixed             (Arity, VecList)
import qualified Data.Vector.Fixed             as DVF (toList)
import           GHC.TypeLits                  (KnownNat, natVal)


displayPermItem :: forall n. Arity n => Perm n -> Mod Int n -> (String, String, String)
displayPermItem p x =
  if px == x
    then (sh x, buffer <> " ", buffer <> " ")
    else (sh x, buffer <> "↓", sh px)
  where
    px = fromMaybe (error "index too big") (permuteOne p x )
    n' = fromIntegral (natVal (Proxy @n)) - 1 :: Int
    maxWidth = length (show n')
    sh j = replicate (maxWidth - length (show j)) ' ' <> show j
    buffer = replicate (maxWidth - 1) ' '

displayPermutation :: forall n. Arity n => Perm n -> (String, String, String)
displayPermutation p = (unwords row1, unwords row2, unwords row3)
  where
    n' = fromIntegral $ natVal (Proxy @n)
    (row1, row2, row3) = unzip3 $ fmap (displayPermItem p) [0..n'-1]

edgeOrientations :: Cyclic 2 -> String
edgeOrientations (Cyclic x)
  | x == 0 = "."
  | x == 1 = "+"
  | otherwise = error "invalid edge orientation"

cornerOrientations :: Cyclic 3 -> String
cornerOrientations (Cyclic x)
  | x == 0    = "."
  | x == 1    = "↶"
  | x == 2    = "↷"
  | otherwise = error "invalid corner orientation"

displayOrientations
  :: forall n m. (Arity n, KnownNat m)
  => (Cyclic m -> String)
  -> VecList n (Cyclic m)
  -> String
displayOrientations symbols vect = unwords $
  fmap (\v -> buffer <> symbols v) (DVF.toList vect)
  where
    n' = fromIntegral (natVal (Proxy @n)) - 1 :: Int
    maxWidth = length (show n')
    buffer = replicate (maxWidth - 1) ' '

-- | Visual display of a rubik group element.
--
-- +--------------+---+---+---+
-- | orientations | 0 | 1 | 2 |
-- +==============+===+===+===+
-- | corners      | . | ↶ | ↷ |
-- +--------------+---+---+---+
-- | edges        | . | + |   |
-- +--------------+---+---+---+
--
-- For example, the F move:
--
-- @
--   corners
--   ↶ ↷ . . ↷ ↶ . .
--   0 1 2 3 4 5 6 7
--   ↓ ↓     ↓ ↓
--   4 0     5 1
--
--   edges
--    +  .  .  .  .  .  .  .  +  .  .  .
--    0  1  2  3  4  5  6  7  8  9 10 11
--    ↓           ↓  ↓        ↓
--    4           8  0        5
--  @
display :: IRubik -> String
display g =
  "corners"
  <>"\n" <> displayOrientations cornerOrientations (getCO g)
  <>"\n" <> cp1
  <>"\n" <> cp2
  <>"\n" <> cp3
  <>"\n"
  <>"\nedges"
  <>"\n" <> displayOrientations edgeOrientations (getEO g)
  <>"\n" <> ep1
  <>"\n" <> ep2
  <>"\n" <> ep3
  <>"\n"
  where
    (cp1, cp2, cp3) = displayPermutation (getCP g)
    (ep1, ep2, ep3) = displayPermutation (getEP g)
