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
import Data.DAREEvaluation
import Data.DARETypes
import Data.ExpressionTypes
import Data.FieldTypes

-- # HTF
import Test.Framework
import TestHelpers

-- # DEBUG
import Debug.Trace

type Element = F2Pow256
--type Element = F97
--instance IntegerAsType n => Field (Fp n) where
--    invert n =
--        case n of
--          0 -> error "0 is not invertible"
--          n' -> 1 / n'

_SOME_POS_NUMS_ :: [Element]
_SOME_POS_NUMS_ = map fromIntegral $ [1..100] ++
                                     [ 2^256-1, 2^42-1, 234345, 391238571]

_VAR_X_ :: Field el => Expr el
_VAR_X_ = Var "x"

_VAR_Y_ :: Field el => Expr el
_VAR_Y_ = Var "y"

_VAR_Z_ :: Field el => Expr el
_VAR_Z_ = Var "z"

_VAL_X_ :: Field el => el
_VAL_X_ = 23

_VAL_Y_ :: Field el => el
_VAL_Y_ = 42

_TEST_VAR_MAP_ :: Field el => VarMapping el
_TEST_VAR_MAP_ = M.fromList [ ("x", _VAL_X_)
                            , (leftVar "x", _VAL_X_)
                            , (rightVar "x", _VAL_X_)
                            , ("y", _VAL_Y_)
                            , (leftVar "y", _VAL_Y_)
                            , (rightVar "y", _VAL_Y_)
                            ]

deriveSkp :: Field el => el -> el -> (el, el)
deriveSkp l r = (if l == 0 then 23 else l, if r == 0 then 42 else r)

dareDecodeFull :: VarMapping Element -> DARE Element -> Maybe Element
dareDecodeFull varMap dare =
    let out = dareDecode varMap dare
        (DARE ((skL, skR), (dkL, dkR)) _ _) = dare
     in case out of
          Nothing -> trace "DECODE FAILED" Nothing
          Just (l, r) ->
              let finalL = (l-dkL) * invert skL
                  finalR = (r-dkR) * invert skR
               in if finalL == finalR
                     then Just finalL
                     else trace ("<"++show finalL++" vs. "++show finalR++">") $
                             Nothing

instance IntegerAsType n => Arbitrary (Fp n) where
    arbitrary = arbitrarySizedIntegral

instance IntegerAsType n => Random (Fp n) where
    random g =
        let (rint, g') = random g
            rint' = rint :: Integer
            in (fromIntegral rint', g')
    randomR (lo, hi) g =
        let loint = (read . show) lo :: Integer
            hiint = (read . show) hi :: Integer
            (rint, g') = randomR (loint, hiint) g
            rint' = rint :: Integer
            rfp = fromIntegral rint'
            in (rfp, g')

execDare :: Field el => Expr el -> IO (Maybe el)
execDare expr =
    do g <- (newGenIO :: IO SystemRandom)
       let (_, dares) = exprToRP g expr
           erp = prepareRPEvaluation dares
           (outDirect, _) = runRP _TEST_VAR_MAP_ dares
           outERP = case runERP erp _TEST_VAR_MAP_ of
                      Left err -> trace err Nothing
                      Right val -> Just val
           out = if outDirect == outERP
                    then outDirect
                    else trace "DIRECT != ERP EVAL" Nothing
       return out

test_simpleDARE =
    do act <- execDare $ sum (replicate 96 1)
       assertEqual (Just (sum (replicate 96 1)) :: Maybe Element) act

test_complexAddDARE =
    do actual <- execDare $ 1 + 17 + _VAR_X_ + (_VAR_X_ + 23)
       let expected :: Maybe Element
           expected = Just $ 1 + 17 + _VAL_X_ + (_VAL_X_ + 23)
       assertEqual expected actual

test_complexDARE1 =
    do actual <- execDare  $ 4 * _VAR_X_ + _VAR_Y_ + _VAR_X_ * _VAR_X_ * _VAR_X_
       let expected :: Maybe Element
           expected = Just $ 4 * _VAL_X_ + _VAL_Y_ + _VAL_X_ * _VAL_X_ * _VAL_X_
       assertEqual expected actual

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
       assertEqual expected actual

prop_dareAddDAREConstants :: Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Bool
prop_dareAddDAREConstants el1 el2 r1 r2 r3 r4 skpL skpR =
    let skp = deriveSkp skpL skpR
        el1DARE = dareEncodePrimaryExpr skp (BiConst el1) r1 r2
        el2DARE = dareEncodePrimaryExpr skp (BiConst el2) r3 r4
        el1el2DARE = dareEncodeDareAdd skp el1DARE el2DARE
        act = dareDecodeFull M.empty el1el2DARE
    in Just (el1 + el2) == act

prop_dareAddConstants :: Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Bool
prop_dareAddConstants el1 el2 r1 r2 r3 r4 skpL skpR =
    let skp = deriveSkp skpL skpR
        el1pe = BiConst el1
        el2pe = BiConst el2
        outDARE = dareEncodeAdd skp el1pe el2pe r1 r2 r3 r4
        act = dareDecodeFull M.empty outDARE
    in Just (el1 + el2) == act

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
                       -> Element
                       -> Element
                       -> Element
                       -> Bool
prop_dareAddAndMulVars r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 =
    let v1pe = BiVar (0,0) "x"
        v2pe = BiVar (0,0) "y"
        skp = (1,1)
        dare1 = dareEncodeMul skp v1pe v1pe r1 r2 r3 r4 r5 r6 r7 r8
        dare2 = dareEncodeMul skp v2pe v2pe r9 r10 r11 r12 r13 r14 r15 r16
        outDARE = dareEncodeDareAdd skp dare1 dare2
        act = dareDecodeFull _TEST_VAR_MAP_ outDARE
    in Just ((_VAL_X_ * _VAL_X_)+(_VAL_Y_ * _VAL_Y_)) == act

prop_dareAddConstants2 :: Element
                       -> Element
                       -> Property
prop_dareAddConstants2 el1 el2 =
    let el1pe = BiConst el1
        el2pe = BiConst el2
        outDARE r = dareEncodeAdd (deriveSkp r r) el1pe el2pe r r r r
        act r = dareDecodeFull M.empty (outDARE r)
    in forAll arbitrarySizedIntegral $ \r -> Just (el1 + el2) == act r

prop_dareMulConstants :: Element
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
prop_dareMulConstants skil skir el1 el2 el3 r1 r2 r3 r4 r5 r6 r7 r8 =
    let el1pe = BiConst el1
        el2pe = BiConst el2
        skp = deriveSkp skil skir
        outDARE = dareEncodeMul skp el1pe el2pe r1 r2 r3 r4 r5 r6 r7 r8
        act = dareDecodeFull M.empty outDARE
    in Just (el1 * el2) == act

threePositiveElements =
    elements [(x, y, z) |
              x <- _SOME_POS_NUMS_, y <- _SOME_POS_NUMS_, z <- _SOME_POS_NUMS_
             ]

encryptedConstant :: Field el
                  => el
                  -> (el, el)
                  -> (PrimaryExpression el, (el, el))
encryptedConstant el (ka, kb) = (Constant $ ka * (el + kb), (ka, kb))

main = htfMain htf_thisModulesTests
