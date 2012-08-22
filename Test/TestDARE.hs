{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- # Standard Library
import Control.Monad (liftM)
import Data.Map (Map)
import qualified Data.Map as M

-- # Site Packages
import Control.Monad.CryptoRandom (CRandT, getCRandom, runCRandT)
import Crypto.Random (SystemRandom, GenError, CryptoRandomGen, newGenIO)
import Math.Algebra.Field.Base
import Math.Common.IntegerAsType (IntegerAsType)
import Math.FiniteFields.F2Pow256
import System.Random (Random(..), RandomGen(..))

-- # Local
import Codec.DARE
import Data.ExpressionTypes
import Data.FieldTypes
import Math.FiniteFields.Foreign.FFInterface (ffInitializeInterface)

-- # HTF
import System.Environment ( getArgs )
import Test.Framework

-- # DEBUG
import Debug.Trace

type Element = F2Pow256
_ALL_POS_NUMS_ :: [Element]
_ALL_POS_NUMS_ = map fromIntegral [1..(2^256-1)]

_VAR_X_ :: FieldElement el => Expr el
_VAR_X_ = Var "x"

_VAR_Y_ :: FieldElement el => Expr el
_VAR_Y_ = Var "y"

_VAR_Z_ :: FieldElement el => Expr el
_VAR_Z_ = Var "z"

_VAL_X_ :: FieldElement el => el
_VAL_X_ = 23

_VAL_Y_ :: FieldElement el => el
_VAL_Y_ = 42

_VAL_Z_ :: FieldElement el => el
_VAL_Z_ = 11

_TEST_VAR_MAP_ :: FieldElement el => VarMapping el
_TEST_VAR_MAP_ = M.fromList [("x", _VAL_X_), ("y", _VAL_Y_), ("z", _VAL_Z_)]

instance IntegerAsType n => Arbitrary (Fp n) where
    arbitrary = arbitrarySizedIntegral

instance Arbitrary F2Pow256 where
    arbitrary = arbitrarySizedIntegral

instance IntegerAsType n => Random (Fp n) where
    random g =
        let (rint, g') = random g
            rint' = rint :: Integer
            in trace ("rint="++show rint++", out="++show (fromIntegral rint')) (fromIntegral rint', g')
    randomR (lo, hi) g =
        let loint = (read . show) lo :: Integer
            hiint = (read . show) hi :: Integer
            (rint, g') = randomR (loint, hiint) g
            rint' = rint :: Integer
            rfp = fromIntegral rint'
            in trace ("lo="++show lo++", hi="++show hi++", rint="++show rint++", out="++show rfp) (rfp, g')

execDare :: FieldElement el => Expr el -> IO (Maybe el)
execDare expr =
    do g <- (newGenIO :: IO SystemRandom)
       let (_, dares) = exprToRP g expr
           (out, _) = runRP _TEST_VAR_MAP_ dares
       return out

test_simpleDARE =
    do act <- execDare $ sum (replicate 96 1)
       assertEqual act (Just (sum (replicate 96 1)) :: Maybe Element)

test_complexAddDARE =
    do actual <- execDare $ 1 + 17 + _VAR_X_ + (_VAR_X_ + 23)
       let expected :: Maybe Element
           expected = Just $ 1 + 17 + _VAL_X_ + (_VAL_X_ + 23)
       assertEqual actual expected

test_complexDARE1 =
    do actual <- execDare  $ 4 * _VAR_X_ + _VAR_Y_ + _VAR_X_ * _VAR_X_ * _VAR_X_
       let expected :: Maybe Element
           expected = Just $ 4 * _VAL_X_ + _VAL_Y_ + _VAL_X_ * _VAL_X_ * _VAL_X_
       assertEqual actual expected

test_complexDARE2 =
    do actual <- execDare  $ ( (  (4 * _VAR_X_ * _VAR_X_ + 2)
                                * (_VAR_X_ + _VAR_Y_ * (_VAR_X_ + _VAR_Y_))
                                * _VAR_X_ * _VAR_Y_ + 7)
                              * _VAR_X_
                             )
       let expected :: Maybe Element
           expected = Just $ ( (  (4 * _VAL_X_ * _VAL_X_ + 2)
                                * (_VAL_X_ + _VAL_Y_ * (_VAL_X_ + _VAL_Y_))
                                * _VAL_X_ * _VAL_Y_ + 7)
                              * _VAL_X_
                             )
       assertEqual actual expected

prop_dareAddDAREConstants :: Element -> Element -> Element -> Bool
prop_dareAddDAREConstants el1 el2 rnd =
    let el1DARE = dareEncodePrimaryExpr $ Constant el1
        el2DARE = dareEncodePrimaryExpr $ Constant el2
        el1el2DARE = dareEncodeDareAdd el1DARE el2DARE rnd
        act = dareDecode M.empty el1el2DARE
    in Just (el1 + el2) == act

prop_dareAddConstants :: Element
                      -> Element
                      -> Element
                      -> Bool
prop_dareAddConstants el1 el2 r =
    let el1pe = Constant el1
        el2pe = Constant el2
        outDARE = dareEncodeAdd el1pe el2pe r
        act = dareDecode M.empty outDARE
    in Just (el1 + el2) == act

prop_dareAddVarsAndConstants :: Element
                             -> Element
                             -> Element
                             -> Element
                             -> Element
                             -> Bool
prop_dareAddVarsAndConstants el1 el2 r1 r2 r3 =
    let el1pe = Constant el1
        el2pe = Constant el2
        v1pe = Variable "x"
        v2pe = Variable "y"
        dare1 = dareEncodeAdd el1pe v1pe r1
        dare2 = dareEncodeAdd el2pe v2pe r2
        outDARE = dareEncodeDareAdd dare1 dare2 r3
        act = dareDecode _TEST_VAR_MAP_ outDARE
    in Just ((el1 + _VAL_X_) + (el2 + _VAL_Y_)) == act

prop_dareAddAndMulVarsAndConstants :: Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Bool
prop_dareAddAndMulVarsAndConstants el1 el2 el3 el4 r1 r2 r3 r4 r5 r6 r7 r8 r9 =
    let el1pe = Constant el1
        el2pe = Constant el2
        el3pe = Constant el3
        el4pe = Constant el4
        v1pe = Variable "x"
        v2pe = Variable "y"
        dare1 = dareEncodeMul el1pe v1pe el3pe r1 r2 r3 r4
        dare2 = dareEncodeMul el2pe v2pe el4pe r5 r6 r7 r8
        outDARE = dareEncodeDareAdd dare1 dare2 r9
        act = dareDecode _TEST_VAR_MAP_ outDARE
    in Just ((el1 * _VAL_X_ + el3) + (el2 * _VAL_Y_ + el4)) == act

prop_dareAddAndMulVars :: Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Element
                       -> Bool
prop_dareAddAndMulVars el1 el2 el3 el4 r1 r2 r3 r4 r5 r6 r7 r8 r9 =
    let v1pe = Variable "x"
        v2pe = Variable "y"
        dare1 = dareEncodeMul v1pe v1pe v1pe r1 r2 r3 r4
        dare2 = dareEncodeMul v2pe v2pe v2pe r5 r6 r7 r8
        outDARE = dareEncodeDareAdd dare1 dare2 r9
        act = dareDecode _TEST_VAR_MAP_ outDARE
    in Just ((_VAL_X_ * _VAL_X_ + _VAL_X_)+(_VAL_Y_ * _VAL_Y_ + _VAL_Y_)) == act

prop_dareAddConstants2 :: Element
                       -> Element
                       -> Property
prop_dareAddConstants2 el1 el2 =
    let el1pe = Constant el1
        el2pe = Constant el2
        outDARE r = dareEncodeAdd el1pe el2pe r
        act r = dareDecode M.empty (outDARE r)
    in forAll arbitrarySizedIntegral $ \r -> Just (el1 + el2) == act r

prop_dareMulConstants :: Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Bool
prop_dareMulConstants el1 el2 el3 r1 r2 r3 r4 =
    let el1pe = Constant el1
        el2pe = Constant el2
        el3pe = Constant el3
        outDARE = dareEncodeMul el1pe el2pe el3pe r1 r2 r3 r4
        act = dareDecode M.empty outDARE
    in Just (el1 * el2 + el3) == act

prop_dareMulConstantsTimesElement :: Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Bool
prop_dareMulConstantsTimesElement el1 el2 el3 el4 r1 r2 r3 r4 =
    let el1pe = Constant el1
        el2pe = Constant el2
        el3pe = Constant el3
        dare = dareEncodeMul el1pe el2pe el3pe r1 r2 r3 r4
        outDARE = mulElementToDARE dare el4
        act = dareDecode M.empty outDARE
    in Just ((el1 * el2 + el3) * el4) == act

prop_dareMulVarsPlusElementTimesElement :: Element
                                        -> Element
                                        -> Element
                                        -> Element
                                        -> Element
                                        -> Element
                                        -> Bool
prop_dareMulVarsPlusElementTimesElement elAdd elMul r1 r2 r3 r4 =
    let el1pe = Variable "x"
        el2pe = Variable "y"
        el3pe = Variable "z"
        dare = dareEncodeMul el1pe el2pe el3pe r1 r2 r3 r4
        outDARE = mulElementToDARE (addToDARE dare elAdd) elMul
        act = dareDecode _TEST_VAR_MAP_ outDARE
    in Just ((_VAL_X_ * _VAL_Y_ + _VAL_Z_ + elAdd) * elMul) == act

prop_dareMulConstantsPlusElement :: Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Element
                                  -> Bool
prop_dareMulConstantsPlusElement el1 el2 el3 el4 r1 r2 r3 r4 =
    let el1pe = Constant el1
        el2pe = Constant el2
        el3pe = Constant el3
        dare = dareEncodeMul el1pe el2pe el3pe r1 r2 r3 r4
        outDARE = addToDARE dare el4
        act = dareDecode M.empty outDARE
    in Just ((el1 * el2 + el3) + el4) == act

prop_dareMulConstantsTimesElements :: Element
                                   -> Element
                                   -> Element
                                   -> [Element]
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Element
                                   -> Bool
prop_dareMulConstantsTimesElements el1 el2 el3 els r1 r2 r3 r4 =
    let el1pe = Constant el1
        el2pe = Constant el2
        el3pe = Constant el3
        dare = dareEncodeMul el1pe el2pe el3pe r1 r2 r3 r4
        outDARE = foldl mulElementToDARE dare els
        act = dareDecode M.empty outDARE
    in Just ((el1 * el2 + el3) * product els) == act

threePositiveElements =
    elements [(x, y, z) |
               x <- _ALL_POS_NUMS_, y <- _ALL_POS_NUMS_, z <- _ALL_POS_NUMS_
             ]

encryptedConstant :: FieldElement el
                  => el
                  -> (el, el)
                  -> (PrimaryExpression el, (el, el))
encryptedConstant el (ka, kb) = (Constant $ ka * (el + kb), (ka, kb))

prop_dareEncryptedMulConstants :: Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Element
                               -> Property
prop_dareEncryptedMulConstants el1 el2 el3 k1b k2b k3b r1 r2 r3 r4 =
    let el1pe kpos = encryptedConstant el1 (kpos, k1b)
        el2pe kpos = encryptedConstant el2 (kpos, k2b)
        el3pe kpos = encryptedConstant el3 (kpos, k3b)
        act :: Element -> Element -> Element -> DARE Element
        act k1a k2a k3a = dareEncodeEncMul (el1pe k1a)
                                           (el2pe k2a)
                                           (el3pe k3a)
                                           r1 r2 r3 r4
    in forAll threePositiveElements $
        \(kpos, kpos2, kpos3) ->
            Just (el1 * el2 + el3) == dareDecode M.empty (act kpos kpos2 kpos3)

--prop_inverse (NonZero x) = (invert x) * x == (1 :: Element)

allTestSuites :: [TestSuite]
allTestSuites = [ allHTFTests ]

testSuite :: TestSuite
testSuite = makeAnonTestSuite (map testSuiteAsTest allTestSuites)

main = do ffInitializeInterface
          args <- getArgs
          runTestWithArgs args testSuite
