{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- # Standard Library
import qualified Data.Map as M

-- # Site Packages
import Crypto.Random (SystemRandom, newGenIO)
import Control.Monad.CryptoRandom (CRandom(..))

-- # Local
import Data.RAE.Encoder
import Data.RAE.Encoder.Internal.DRAC
import Data.RAE.Evaluation
import Data.RAE.Decoder
import Data.RAE.Types
import Data.ExpressionTypes
import Data.FieldTypes
import qualified Math.Polynomials as P
import qualified Functionality.AllInOne as AIO

-- # HTF
import Test.Framework

-- # DEBUG
import Debug.Trace

import Math.FiniteFields.F2Pow256
type Element = F2Pow256

--import Math.FiniteFields.F97
--type Element = F97

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

_TEST_VAR_MAP_CLEAN_ :: Field el => VarMapping el
_TEST_VAR_MAP_CLEAN_ = M.fromList [ ("x", _VAL_X_)
                                  , ("y", _VAL_Y_)
                                  , ("z", _VAL_Y_)
                                  ]
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

draeDecodeFull :: VarMapping Element -> DRAE Element -> Maybe Element
draeDecodeFull varMap drae =
    let out = decodeDRAE varMap drae
        (DRAE ((skL, skR), (dkL, dkR)) _ _) = drae
     in case out of
          Nothing -> trace "DECODE FAILED" Nothing
          Just (l, r) ->
              let finalL = (l-dkL) * invert skL
                  finalR = (r-dkR) * invert skR
               in if finalL == finalR
                     then Just finalL
                     else trace ("<"++show finalL++" vs. "++show finalR++">") $
                             Nothing

execExpr :: (Show el, CRandom el, Field el) => Expr el -> IO (Maybe el)
execExpr expr =
    do g <- (newGenIO :: IO SystemRandom)
       let varMap = _TEST_VAR_MAP_CLEAN_
           (_, drac) = exprToDRAC g expr
           (rac, oac) = singularizeDRAC drac
           (outDirect, _) = runDRAC varMap drac
           outRAC = case runRAC rac oac varMap of
                      Left err -> trace err Nothing
                      Right val -> Just val
           out = if outDirect == outRAC
                    then outDirect
                    else trace "DIRECT != RAC EVAL" Nothing
       funcOutM <- AIO.evaluateExpr varMap expr (\_ -> return ())
       let finalOut = if out == funcOutM
                         then out
                         else trace "DIRECT != FUNC" Nothing
       return finalOut

test_simpleDRAE =
    do act <- execExpr $ sum (replicate 96 1)
       assertEqual (Just (sum (replicate 96 1)) :: Maybe Element) act

test_complexAddDRAE =
    do actual <- execExpr $ 1 + 17 + _VAR_X_ + (_VAR_X_ + 23)
       let expected :: Maybe Element
           expected = Just $ 1 + 17 + _VAL_X_ + (_VAL_X_ + 23)
       assertEqual expected actual

test_complexDRAE1 =
    do actual <- execExpr  $ 4 * _VAR_X_ + _VAR_Y_ + _VAR_X_ * _VAR_X_ * _VAR_X_
       let expected :: Maybe Element
           expected = Just $ 4 * _VAL_X_ + _VAL_Y_ + _VAL_X_ * _VAL_X_ * _VAL_X_
       assertEqual expected actual

test_complexDRAE2 =
    do actual <- execExpr  $ ( (  (4 * _VAR_X_ * _VAR_X_ + 2)
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

poly1toN :: Integer -> (forall a. Num a => a -> [a] -> a) -> IO ()
poly1toN n buildPoly =
    do let polyCoeffs :: [Element]
           polyCoeffs = map fromInteger [1..n]
           poly :: Expr Element
           poly = buildPoly _VAR_X_ (map Literal polyCoeffs)
           expected = Just $ buildPoly _VAL_X_ polyCoeffs
       actual <- execExpr poly
       assertEqual expected actual

test_hornerPoly1to100 = poly1toN 100 P.horner

test_monomialPoly1to100 = poly1toN 10 P.monomial

prop_draeAddDRAEConstants :: Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Element
                          -> Bool
prop_draeAddDRAEConstants el1 el2 r1 r2 r3 r4 skpL skpR =
    let skp = deriveSkp skpL skpR
        el1DRAE = draeEncodePrimaryExpr skp (DualConst el1) r1 r2
        el2DRAE = draeEncodePrimaryExpr skp (DualConst el2) r3 r4
        el1el2DRAE = draeEncodeDRAEAdd skp el1DRAE el2DRAE
        act = draeDecodeFull M.empty el1el2DRAE
    in Just (el1 + el2) == act

prop_draeAddConstants :: Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Element
                      -> Bool
prop_draeAddConstants el1 el2 r1 r2 r3 r4 skpL skpR =
    let skp = deriveSkp skpL skpR
        el1pe = DualConst el1
        el2pe = DualConst el2
        outDRAE = draeEncodeAdd skp el1pe el2pe r1 r2 r3 r4
        act = draeDecodeFull M.empty outDRAE
    in Just (el1 + el2) == act

prop_draeAddAndMulVars :: Element
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
prop_draeAddAndMulVars r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 =
    let v1pe = DualVar (0,0) "x"
        v2pe = DualVar (0,0) "y"
        skp = (1,1)
        drae1 = draeEncodeMul skp v1pe v1pe r1 r2 r3 r4 r5 r6 r7 r8
        drae2 = draeEncodeMul skp v2pe v2pe r9 r10 r11 r12 r13 r14 r15 r16
        outDRAE = draeEncodeDRAEAdd skp drae1 drae2
        act = draeDecodeFull _TEST_VAR_MAP_ outDRAE
    in Just ((_VAL_X_ * _VAL_X_)+(_VAL_Y_ * _VAL_Y_)) == act

prop_draeAddConstants2 :: Element
                       -> Element
                       -> Property
prop_draeAddConstants2 el1 el2 =
    let el1pe = DualConst el1
        el2pe = DualConst el2
        outDRAE r = draeEncodeAdd (deriveSkp r r) el1pe el2pe r r r r
        act r = draeDecodeFull M.empty (outDRAE r)
    in forAll arbitrarySizedIntegral $ \r -> Just (el1 + el2) == act r

prop_draeMulConstants :: Element
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
prop_draeMulConstants skil skir el1 el2 el3 r1 r2 r3 r4 r5 r6 r7 r8 =
    let el1pe = DualConst el1
        el2pe = DualConst el2
        skp = deriveSkp skil skir
        outDRAE = draeEncodeMul skp el1pe el2pe r1 r2 r3 r4 r5 r6 r7 r8
        act = draeDecodeFull M.empty outDRAE
    in Just (el1 * el2) == act

main = htfMain htf_thisModulesTests
