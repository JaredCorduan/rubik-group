{-# LANGUAGE DataKinds #-}

import           Data.Group            (invert)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           RubikGroup


irubikAsoc :: IRubik -> IRubik -> IRubik -> Bool
irubikAsoc x y z = x <> y <> z == x <> y <> z

irubikInv :: IRubik -> Bool
irubikInv x = x <> invert x == mempty

irubikIdl :: IRubik -> Bool
irubikIdl x = x <> mempty == x

irubikIdr :: IRubik -> Bool
irubikIdr x = mempty <> x == x

conservationOfTwists :: Rubik -> Bool
conservationOfTwists g = sumTwists (illegal g) == mempty

conservationOfFlips :: Rubik -> Bool
conservationOfFlips g = sumFlips (illegal g) == mempty

equalParity :: Rubik -> Bool
equalParity g = isEvenPerm (getCP . illegal $ g) == isEvenPerm (getEP . illegal $ g)

signTest :: [Trnsp 12] -> Bool
signTest ts = isEvenPerm (mkPerm ts' :: Perm 12) == (length ts' `mod` 2 == 0)
  where ts' = filter (\(Trnsp x y) -> x `mod` 12 /= y `mod` 12) ts

corner3cycle :: Bool
corner3cycle =
  illegal (mkRubik [U, R, U', L', U, R', U', L]) ==
  mkIRubik
    [0, 2, 2, 2, 0, 0, 0, 0] (1 ~~> 3 ~> 2)
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] []

edge3cycle :: Bool
edge3cycle =
  illegal (mkRubik [R, R, U, R, U, R', U', R', U', R', U, R']) ==
  mkIRubik
    [0, 0, 0, 0, 0, 0, 0, 0] []
    [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] (0 ~~> 1 ~> 3)

edgeSwapCornerSwap :: Bool
edgeSwapCornerSwap =
  illegal (mkRubik [R',U, L', U, U, R, U', R', U, U, R, L, U']) ==
  mkIRubik
    [0, 0, 0, 0, 0, 0, 0, 0] (2 ~~> 3)
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] (1 ~~> 2)

reorientCorners :: Bool
reorientCorners =
  illegal (mkRubik [R', D', R, D, R', D', R, D, U, D', R', D, R, D', R', D, R, U']) ==
  mkIRubik
    [1, 0, 0, 2, 0, 0, 0, 0] []
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] []

reorientEdges :: Bool
reorientEdges =
  illegal (mkRubik [F, R', F', R', F, F, L, D, R, D', L', R', F, F, R, R]) ==
  mkIRubik
    [0, 0, 0, 0, 0, 0, 0, 0] []
    [1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0] []


illegalLaws :: TestTree
illegalLaws = testGroup "Illegal Cube - Group Laws"
  [ testProperty "IRubik associative" irubikAsoc
  , testProperty "IRubik inverses" irubikInv
  , testProperty "IRubik identity left" irubikIdl
  , testProperty "IRubik identity right" irubikIdr
  ]

legalProps :: TestTree
legalProps = testGroup "Legal Cube - Properties"
  [ testProperty "Conservation of Twists" conservationOfTwists
  , testProperty "Conservation of Flips" conservationOfFlips
  , testProperty "Equal Parity" equalParity
  , testProperty "Correctness of isEvenPerm" signTest
  ]

exampleCombos :: TestTree
exampleCombos = testGroup "Examples - Common Algos"
  [ testProperty "Corner 3-Cycle" corner3cycle
  , testProperty "Edge 3-Cycle" edge3cycle
  , testProperty "Edge Swap and Corner Swap" edgeSwapCornerSwap
  , testProperty "Reorient Corners" reorientCorners
  , testProperty "Reorient Edges" reorientEdges
  ]

rubikTests :: TestTree
rubikTests = testGroup "Rubik's Cube Algebra" [illegalLaws, legalProps, exampleCombos]

main :: IO ()
main = defaultMain rubikTests
