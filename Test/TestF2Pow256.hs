{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- # Standard Library

-- # Site Packages

-- # LOCAL
import Data.FieldTypes
import Math.FiniteFields.F2Pow256
import Math.FiniteFields.Foreign.FFInterface

-- # HTF
import System.Environment ( getArgs )
import Test.Framework
import TestHelpers

-- # DEBUG
import Debug.Trace

type Element = F2Pow256

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

prop_multiplicationAssociative :: Element -> Element -> Element -> Bool
prop_multiplicationAssociative a b c = (a * b) * c == a * (b * c)

prop_multiplicativeInverse :: Element -> Bool
prop_multiplicativeInverse e = e == 0 || e * (invert e) == 1

prop_elementPlusZeroEqualsElement :: Element -> Bool
prop_elementPlusZeroEqualsElement e = e == e + 0

prop_distributiveLaw :: Element -> Element -> Element -> Bool
prop_distributiveLaw a b c = a * (b + c) == a * b + a * c

prop_zeroIsNotOne :: Bool
prop_zeroIsNotOne = (0 :: Element) /= (1 :: Element)

allTestSuites :: [TestSuite]
allTestSuites = [ allHTFTests ]

testSuite :: TestSuite
testSuite = makeAnonTestSuite (map testSuiteAsTest allTestSuites)

main = do ffInitializeInterface
          args <- getArgs
          runTestWithArgs args testSuite
