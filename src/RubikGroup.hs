{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : RubikGroup
Description : Implementation of the Rubik's Cube group
License     : GPL-3
Maintainer  : jaredcorduan@gmail.com
Stability   : experimental
Portability : POSIX

This is an Haskell implementation of the Rubik's Cube algebra described in
 [Joyner's wonderful book](https://www.maa.org/press/maa-reviews/adventures-in-group-theory)
 on the subject.

If you take the Rubik's Cube apart (without peeling off any stickers)

 <<https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/Disassembled-rubix-1.jpg/320px-Disassembled-rubix-1.jpg>>

and put it back together anyway that the pieces will fit,
you get a permutation of the stickers that may or may not be solveable anymore.
The collection of all such permutations is what Joyner calls the the
__illegal Rubik's Cube group__, and has been implemented as 'IRubik'.
Investigating this group provides insight into the actual Rubik's Cube group,
implemented here as 'Rubik'.

The illegal Rubik's Cube group is isomorphic to:

\[ (\mathsf{C}_3^8 \rtimes_\phi \mathsf{S_8})\times(\mathsf{C}_2^{12} \rtimes_\phi \mathsf{S_{12}}) \]

where \( \phi~:\mathsf{S_n}\to\mathsf{Aut}(\mathsf{C}_m^n) \) is given by
the action of \( \mathsf{S_n} \) on \( n \).

Alternatively, it can be written using wreath products:

\[ (\mathsf{C}_3 \wr \mathsf{S_8})\times(\mathsf{C}_2 \wr \mathsf{S_{12}}) \]

See Theorem 11.1.1 of Joyner.

The Rubik's Cube group is the following subgroup:

\[
   \Big\{
     (v,~r,~w,~s)
     ~\mid~
     \mathsf{sign}(r)=\mathsf{sign}(s),~v_1+\ldots v_8 \equiv 0,~w_1+\ldots w_{12} \equiv 0
   \Big\}
\]

See Theorem 11.2.1 of Joyner.
-}

module RubikGroup
  (
-- | The Illegal Rubik's Cube Group is defined as the product of two groups,
-- one for the corners and one for the edges. Each of these groups is
-- a semidirect product of a power of a cyclic group (corresponding to orientations)
-- with a permutation group.
-- The funtion 'mkIRubik' provides a convenient way of constructing instances.
    IRubik
-- | Get the corner orientations.
   , getCO
-- | Get the corner permutatinos.
   , getCP
-- | Get the edge orientations.
   , getEO
-- | Get the edge permutatinos.
   , getEP
-- | Transpositons used to build permutations.
-- These will be used to create permutations of size 8 and 12, but the transposition
-- will happily accept numbers of any size. When they are use to create specific
-- permutations of fixed width vectors, the numbers are taken modulo the size.
  , Trnsp(..)
-- | Used together with '~>' to create [cycles](https://en.wikipedia.org/wiki/Cyclic_permutation).
--
-- given numbers @x@ and @y@, @x ~~> y@ creates the transposition \( (x~y) \).
  , (~~>)
-- | Given the transposition @x ~~> y@, use @x ~~> y ~> z@ to create the
-- permutation \( (x~y)(x~z) = (x~y~z) \).
--
-- This can be extended indefinitely to create cycles, so that
-- @x ~~> y ~> z ~> w@ creates \( (x~y)(x~z)(x~w) = (x~y~z~w) \), etc.
--
-- This is intended for creating cyles, but nothing enforces this. For example,
-- @x ~~> y ~> z ~> y ~> w@ creates \( (x~y)(x~z)(x~y)(x~w) = (xw)(yz) \), etc.
  , (~>)
-- | Permutation
  , Perm
-- | Construct a permutation from a list of transpositions.
  , mkPerm
-- | Construct an element of the illegal Rubik's cube group.
-- It takes a list of integers for the corner orientations, a list of transpositions
-- to make the corner permutation, and again two more lists for the edges.
-- The orientations will be converted to numbers of the appropriate modulus,
-- and the both the orientations and the permutations will be converted to fixed-width vectors.
  , mkIRubik
-- | The 'Move' type is an abstraction of the usual moves/rotations of the Rubik's Cube.
-- We will translate them into the illegal Rubik's Cube, and for that we must
-- specify a labeling of the corners, edges, and all the orientations.
--
-- First we label the sides:
--
-- @
--    _ _ _
--   \/_\/_\/_\/\\
--  \/_\/U\/_\/\\/\\
-- \/_\/_\/_\/\\\/R\/\\
-- \\_\\_\\_\\\/\\\/\\\/
--  \\_\\F\\_\\\/\\\/
--   \\_\\_\\_\\\/
-- @
--
-- with
--
-- * __B__ack opposite __F__orward,
-- * __D__own opposite __U__p,
-- * __L__eft opposite __R__ight
--
-- We use the same label for the clockwise rotation of the corresponding side.
--
-- Given the standard "western" color scheme of:
--
-- W = __w__hite, O = __o__range, G = __g__reen, R = __r__ed, B = __b__lue, Y = __y__ellow
--
-- label the corners:
--
-- - 0 - RWB
-- - 1 - GWR
-- - 2 - OWG
-- - 3 - BWO
-- - 4 - RYB
-- - 5 - GYR
-- - 6 - OYG
-- - 7 - BYO
--
-- label the edges:
--
-- -  0 - RW
-- -  1 - GW
-- -  2 - OW
-- -  3 - BW
-- -  4 - RB
-- -  5 - GR
-- -  6 - OG
-- -  7 - BO
-- -  8 - RY
-- -  9 - GY
-- - 10 - OY
-- - 11 - BY
--
-- Orient the corners:
-- Mark each white and yellow square on the corners with a +
-- The orientation of a corner is the number of clockwise
-- rotations needed to get the plus sign oriented as in solved state.
--
-- Orient the edges:
-- Mark one square on each edge as follows:
--
-- - rw -> w
-- - gw -> g
-- - ow -> o
-- - bw -> w
-- - rb -> r
-- - gr -> g
-- - og -> o
-- - bo -> b
-- - ry -> r
-- - gy -> y
-- - oy -> y
-- - by -> b
--
-- The orientation of an edge is 0 if the plus matches the solved state and 1 otherwise.
  , Move(..)
-- | The (legal) Rubik's Cube group is defined as a sequence of the usual moves.
-- A sequence of moves can be translated to the illegal Rubik's Cube group with 'toIRubik'.
  , Rubik(..)
-- | Translate a sequence of moves to the (legal) Rubik's Cube group.
  , mkRubik
-- | Forward move
--
-- @
-- f = mkIRubik
--   [1, 2, 0, 0, 2, 1, 0, 0] (0 ~~> 4 ~> 5 ~> 1)
--   [1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] (0 ~~> 4 ~> 8 ~> 5)
-- @
  , f
-- | Up move
--
-- @
-- u = mkIRubik
--   [0, 0, 0, 0, 0, 0, 0, 0] (0 ~~> 1 ~> 2 ~> 3)
--   [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] (0 ~~> 1 ~> 2 ~> 3)
-- @
  , u
-- | Right move
--
-- @
-- r = mkIRubik
--   [2, 0, 0, 1, 1, 0, 0, 2] (0 ~~> 3 ~> 7 ~> 4)
--   [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1] (3 ~~> 7 ~> 11 ~> 4)
-- @
  , r
-- | Back move
--
-- @
-- b = mkIRubik
--   [0, 0, 1, 2, 0, 0, 2, 1] (2 ~~> 6 ~> 7 ~> 3)
--   [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0] (2 ~~> 5 ~> 10 ~> 7)
-- @
  , b
-- | Down move
--
-- @
-- d = mkIRubik
--   [0, 0, 0, 0, 0, 0, 0, 0] (4 ~~> 7 ~> 6 ~> 5)
--   [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1] (8 ~~> 11 ~> 10 ~> 9)
-- @
  , d
-- | Left move
--
-- @
-- l = mkIRubik
--   [0, 1, 2, 0, 0, 2, 1, 0] (1 ~~> 5 ~> 6 ~> 2)
--   [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0] (1 ~~> 5 ~> 9 ~> 6)
-- @
  , l
-- | Sum of all corner orientations
  , sumTwists
-- | Sum of all edge orientations
  , sumFlips
-- | Orbit of a permutation
  , orbits
-- | True if the permutation is even
  , isEvenPerm
  ) where

import           Data.Group                    (Group, invert)
import qualified Data.Map                      as Map
import           Data.Modular                  (Mod, toMod, unMod)
import           Data.Monoid.Action            (Action, act)
import           Data.Monoid.SemiDirectProduct (Semi, tag, unSemi)
import           Data.Proxy                    (Proxy (..))
import qualified Data.Set                      as Set
import           Data.Vector.Fixed             (Arity, VecList, generate,
                                                generateM, imap, (!))
import qualified Data.Vector.Fixed             as DVF
import           GHC.TypeLits                  (KnownNat, natVal)
import           Test.Tasty.QuickCheck         (Arbitrary, arbitrary,
                                                arbitraryBoundedEnum)

newtype Cyclic n = Cyclic (Mod Int n) deriving (Eq)

instance Show (Cyclic n) where
  show (Cyclic x) = show x

instance KnownNat n => Semigroup (Cyclic n) where
  (Cyclic x) <> (Cyclic y) = Cyclic $ x + y

instance KnownNat n => Monoid (Cyclic n) where
  mempty = Cyclic 0

instance KnownNat n => Group (Cyclic n) where
  invert (Cyclic x) = Cyclic $ negate x

data Trnsp n = Trnsp (Mod Int n) (Mod Int n) deriving (Show, Eq)

newtype Perm n = Perm (VecList n (Mod Int n))

instance Arity n => Eq (Perm n) where
  (Perm p) == (Perm q) = p == q

instance Arity n => Show (Perm n) where
  show (Perm p) = show p

instance Arity n => Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ DVF.map (q !) (DVF.map unMod p)

mkPerm :: forall n. Arity n => [Trnsp n] -> Perm n
mkPerm sws = Perm $ DVF.map (\z -> foldl evalTrnsp z sws) (generate toMod)
  where
    evalTrnsp z (Trnsp x y)
      | z == x = y
      | z == y = x
      | otherwise = z

instance Arity n => Monoid (Perm n) where
  mempty = mkPerm []

permToMap :: forall n. Arity n => Perm n -> Map.Map (Mod Int n) Int
permToMap (Perm p) = Map.fromList $ DVF.toList $ imap (\i x -> (x, i)) p

instance Arity n => Group (Perm n) where
  invert p = Perm $ DVF.map get (generate toMod)
    where
      get x = toMod $ Map.findWithDefault 0 x (permToMap p)

instance (Arity n, KnownNat m) => Group (VecList n (Cyclic m)) where
  invert v = DVF.fromList $ fmap invert (DVF.toList v)

instance (Arity n, KnownNat m) => Group (Semi (VecList n (Cyclic m)) (Perm n)) where
  invert g = tag (act p' (invert v)) p'
    where
      (v, p) = unSemi g
      p' = invert p

instance Arity n => Show (Semi (VecList n (Cyclic m)) (Perm n)) where
  show snm = show (o, p)
    where
      (o, Perm p) = unSemi snm

instance Arity n => Eq (Semi (VecList n (Cyclic m)) (Perm n)) where
  x == y = unSemi x == unSemi y

instance Arity n => Action (Perm n) (VecList n (Cyclic m)) where
  act (Perm p) v = DVF.map ((v !) . unMod) p

type Corners = Semi (VecList 8 (Cyclic 3)) (Perm 8)
type Edges = Semi (VecList 12 (Cyclic 2)) (Perm 12)

data IRubik = IRubik Corners Edges deriving (Show, Eq)

getCO :: IRubik -> VecList 8 (Cyclic 3)
getCO (IRubik c _) = fst . unSemi $ c

getCP :: IRubik -> Perm 8
getCP (IRubik c _) = snd . unSemi $ c

getEO :: IRubik -> VecList 12 (Cyclic 2)
getEO (IRubik _ e) = fst . unSemi $ e

getEP :: IRubik -> Perm 12
getEP (IRubik _ e) = snd . unSemi $ e

mkIRubik :: [Int] -> [Trnsp 8] -> [Int] -> [Trnsp 12] -> IRubik
mkIRubik co cp eo ep =
  IRubik
    (tag (DVF.fromList (map (Cyclic . toMod) co)) (mkPerm cp))
    (tag (DVF.fromList (map (Cyclic . toMod) eo)) (mkPerm ep))

instance Semigroup IRubik where
  (IRubik c1 e1) <> (IRubik c2 e2) = IRubik (c1 <> c2) (e1 <> e2)

instance Monoid IRubik where
  mempty = IRubik mempty mempty

instance Group IRubik where
  invert (IRubik c1 e1) = IRubik (invert c1) (invert e1)

(~~>) :: KnownNat n => Int -> Int -> [Trnsp n]
x ~~> y = [Trnsp (toMod x) (toMod y)]

(~>) :: KnownNat n => [Trnsp n] -> Int -> [Trnsp n]
[] ~> _ = []
sws ~> z = sws ++ [Trnsp x (toMod z)]
  where (Trnsp x _) = last sws

f :: IRubik
f = mkIRubik
  [1, 2, 0, 0, 2, 1, 0, 0] (0 ~~> 4 ~> 5 ~> 1)
  [1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0] (0 ~~> 4 ~> 8 ~> 5)

u :: IRubik
u = mkIRubik
  [0, 0, 0, 0, 0, 0, 0, 0] (0 ~~> 1 ~> 2 ~> 3)
  [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0] (0 ~~> 1 ~> 2 ~> 3)

r :: IRubik
r = mkIRubik
  [2, 0, 0, 1, 1, 0, 0, 2] (0 ~~> 3 ~> 7 ~> 4)
  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1] (3 ~~> 7 ~> 11 ~> 4)

b :: IRubik
b = mkIRubik
  [0, 0, 1, 2, 0, 0, 2, 1] (2 ~~> 6 ~> 7 ~> 3)
  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0] (2 ~~> 5 ~> 10 ~> 7)

d :: IRubik
d = mkIRubik
  [0, 0, 0, 0, 0, 0, 0, 0] (4 ~~> 7 ~> 6 ~> 5)
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1] (8 ~~> 11 ~> 10 ~> 9)

l :: IRubik
l = mkIRubik
  [0, 1, 2, 0, 0, 2, 1, 0] (1 ~~> 5 ~> 6 ~> 2)
  [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0] (1 ~~> 5 ~> 9 ~> 6)

data Move
  -- | __F__orward (clockwise)
  = F
  -- | __U__p (clockwise)
  | U
  -- | __R__ight (clockwise)
  | R
  -- | __B__ack (clockwise)
  | B
  -- | __D__own (clockwise)
  | D
  -- | __L__elf (clockwise)
  | L
  -- | __F__orward (counter-clockwise)
  | F'
  -- | __U__p (counter-clockwise)
  | U'
  -- | __R__ight (counter-clockwise)
  | R'
  -- | __B__ack (counter-clockwise)
  | B'
  -- | __D__own (counter-clockwise)
  | D'
  -- | __L__elf (counter-clockwise)
  | L'
  deriving (Show, Eq, Enum, Bounded)

newtype Rubik = Rubik { illegal :: IRubik } deriving (Show, Eq)

mkRubik :: [Move] -> Rubik
mkRubik = Rubik . foldr (\m a -> moveToIR m <> a) mempty
  where
    moveToIR F  = f
    moveToIR U  = u
    moveToIR R  = r
    moveToIR B  = b
    moveToIR D  = d
    moveToIR L  = l
    moveToIR F' = invert f
    moveToIR U' = invert u
    moveToIR R' = invert r
    moveToIR B' = invert b
    moveToIR D' = invert d
    moveToIR L' = invert l

sumTwists :: IRubik -> Cyclic 3
sumTwists (IRubik c _) = foldl (<>) mempty co
  where (co, _) = unSemi c

sumFlips :: IRubik -> Cyclic 2
sumFlips (IRubik _ e) = foldl (<>) mempty eo
  where (eo, _) = unSemi e


orbit :: forall n. Arity n => Perm n -> Mod Int n -> Set.Set (Mod Int n)
orbit p x = orbit' p x (Set.singleton x)
  where
    orbit' (Perm q) i current =
      if Set.member j current
        then current
        else orbit' (Perm q) j (Set.insert j current)
      where j = q ! unMod i

-- this is far from optimal, but for n=12 it's nbd
orbits :: forall n. Arity n => Perm n -> Set.Set (Set.Set (Mod Int n))
orbits p = foldl (\s i -> Set.insert (orbit p i) s) Set.empty $ map toMod [0..(m-1)]
  where m = fromIntegral $ natVal (Proxy @n)

isEvenPerm :: Arity n => Perm n -> Bool
isEvenPerm p = foldl (\a o -> a + Set.size o - 1) 0 (orbits p) `mod` 2 == 0

-- Arbitrary Instances --

instance KnownNat n => Arbitrary (Trnsp n) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Trnsp (toMod x) (toMod y)

instance Arity n => Arbitrary (Perm n) where
  arbitrary = mkPerm <$> arbitrary

instance KnownNat m => Arbitrary (Cyclic m) where
  arbitrary = (Cyclic . toMod) <$> arbitrary

instance (Arity n, KnownNat m) => Arbitrary (Semi (VecList n (Cyclic m)) (Perm n)) where
  arbitrary = do
    p <- arbitrary
    v <- generateM $ const arbitrary
    return $ tag v p

instance Arbitrary IRubik where
  arbitrary = do
    c <- arbitrary
    e <- arbitrary
    return $ IRubik c e

instance Arbitrary Move where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Rubik where
  arbitrary = mkRubik <$> arbitrary
