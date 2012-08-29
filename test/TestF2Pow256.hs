{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- # Standard Library

-- # Site Packages

-- # LOCAL
import Data.FieldTypes
import Math.FiniteFields.F2Pow256

-- # HTF
import Test.Framework
import TestHelpers

-- # DEBUG
import Debug.Trace

type Element = F2Pow256

_MAX_NUM_ :: Element
_MAX_NUM_ = fromInteger $ 2^256-1

prop_elementEqualsElement :: Element -> Bool
prop_elementEqualsElement e = e == e

prop_elementElementPlusOneMinusOne :: Element -> Bool
prop_elementElementPlusOneMinusOne e = e == (e + 1) - 1

prop_elementEqualsElementTimesOne :: Element -> Bool
prop_elementEqualsElementTimesOne e = e == (e * 1)

prop_zeroEqualsElementTimesZero :: Element -> Bool
prop_zeroEqualsElementTimesZero e = 0 == (0 * e)

prop_additionCommutative :: Element -> Element -> Bool
prop_additionCommutative l r = l + r == r + l

prop_multiplicationCommutative :: Element -> Element -> Bool
prop_multiplicationCommutative l r = l * r == r * l

prop_elementNotEqualNextElement :: Element -> Bool
prop_elementNotEqualNextElement e = e /= (e + 1)

prop_equalsNotEqualsDifferent :: Element -> Element -> Bool
prop_equalsNotEqualsDifferent l r = (l == r) /= (l /= r)

prop_additionAssociative :: Element -> Element -> Element -> Bool
prop_additionAssociative a b c = (a + b) + c == a + (b + c)

prop_differenceAssociative :: Element -> Element -> Element -> Bool
prop_differenceAssociative a b c = (a - b) - c == a - (b - c)

prop_multiplicationAssociative :: Element -> Element -> Element -> Bool
prop_multiplicationAssociative a b c = (a * b) * c == a * (b * c)

prop_multiplicativeInverse :: Element -> Bool
prop_multiplicativeInverse e = e == 0 || e * (invert e) == 1

prop_additiveInverse :: Element -> Bool
prop_additiveInverse e = e - e == 0 && e + (-e) == 0

prop_elementPlusZeroEqualsElement :: Element -> Bool
prop_elementPlusZeroEqualsElement e = e == e + 0

prop_distributiveLaw :: Element -> Element -> Element -> Bool
prop_distributiveLaw a b c = a * (b + c) == a * b + a * c

prop_zeroIsNotOne :: Bool
prop_zeroIsNotOne = (0 :: Element) /= (1 :: Element)

test_representationZero :: IO ()
test_representationZero =
    do let expected = "[]"
           actual = show (0 :: Element)
       assertEqual expected actual

test_representationOne :: IO ()
test_representationOne =
    do let expected = "[1]"
           actual = show (1 :: Element)
       assertEqual expected actual

test_representationTwo :: IO ()
test_representationTwo =
    do let expected = "[0 1]"
           actual = show (2 :: Element)
       assertEqual expected actual

test_representationThree :: IO ()
test_representationThree =
    do let expected = "[1 1]"
           actual = show (3 :: Element)
       assertEqual expected actual

test_representation255 :: IO ()
test_representation255 =
    do let elem = 255 :: Element
           actual = show elem
           expected = "[1 1 1 1 1 1 1 1]"
       assertEqual expected actual

test_representation256 :: IO ()
test_representation256 =
    do let elem = 256 :: Element
           actual = show elem
           expected = "[0 0 0 0 0 0 0 0 1]"
       assertEqual expected actual

test_representation2Pow256Minus1 :: IO ()
test_representation2Pow256Minus1 =
    do let act = show _MAX_NUM_
           exp = "[1" ++ (concat $ replicate 254 " 1") ++ " 1]"
       assertEqual exp act

prop_readParsesShow :: Element -> Bool
prop_readParsesShow e = e == (f2Pow256FromString (show e))

main = htfMain htf_thisModulesTests
