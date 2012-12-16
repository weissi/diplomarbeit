--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.RAE.Encoder.Internal.DRAC
    ( -- * Public API
      exprToDRAC
      -- * Internal, Interesting API
      -- ** Real, Pure 'DRAE' Encoding
    , draeEncodeMul
    , draeEncodeAdd
    , draeEncodeDRAEAdd
    , draeEncodePrimaryExpr
      -- * Internal, Uninteresting API
      -- ** 'DRAE' Encoding automatically getting random numbers
    , draeEncodeMulRnd
    , draeEncodeAddRnd
    , draeEncodeDRAEAddRnd
      -- ** Internal Data Types
    , DualEncPrimExpr(..)
    ) where

-- # STANDARD LIBRARY
import Data.List (nub)
import Data.Map (Map)
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Text as T

-- # SITE PACKAGES
import Control.Monad.CryptoRandom (CRandT, getCRandom, runCRandT, CRandom)
import Crypto.Random (GenError, CryptoRandomGen)

-- # LOCAL
import Data.ExpressionTypes (Expr(..), Operator(..))
import Data.FieldTypes (Field(..))
import Data.LinearExpression (LinearExpr(..))
import Data.RAE.Evaluation ( _SPECIAL_DUAL_VAR_OUT_
                           , _SPECIAL_VAR_ADDED_PRE_OUT_
                           , _SPECIAL_DUAL_VAR_PRE_OUT_
                           )
import Data.RAE.Types ( VariableName, DRAE(..), DRAC
                      , PrimaryExpression(..)
                      , Key, DualKey, DualKeyPair
                      , DualVarName(..)
                      , genDualVarName
                      , DualLinearRadicals(..)
                      , RadicalTuple(..)
                      , MulTermRadicals(..)
                      )
import qualified Data.LinearExpression as LE

-- | A dual encrypted primary expression.
--
-- Either an unencrypted constant or an encrypted variable along with the dual
-- key (only the dynamic keys) needed to decrypt it.
data DualEncPrimExpr el = DualConst el
                        | DualVar (DualKey el) DualVarName
                        deriving Show

getRandomElement :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => CRandT g GenError m el
getRandomElement =
    do rint <- getCRandom
       return $! rint

getRandomInvertibleElement :: forall m g el.
                              (Monad m, CryptoRandomGen g, Field el, CRandom el)
                           => CRandT g GenError m el
getRandomInvertibleElement =
    loop
    where loop =
              do rel <- getRandomElement
                 if rel /= zero
                   then return $! rel
                   else loop

-- |'DRAE' for a multiplication getting randoms from generator
draeEncodeMulRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => DualKey el                    -- ^ The static dual key
                 -> DualEncPrimExpr el            -- ^ /x1/
                 -> DualEncPrimExpr el            -- ^ /x2/
                 -> CRandT g GenError m (DRAE el) -- ^ DRAE encoding /x1 * x2/
draeEncodeMulRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       r5 <- getRandomElement
       r6 <- getRandomElement
       r7 <- getRandomElement
       r8 <- getRandomElement
       rk1 <- getRandomElement
       rk2 <- getRandomElement
       rk3 <- getRandomElement
       rk4 <- getRandomElement
       rk5 <- getRandomElement
       rk6 <- getRandomElement
       rk7 <- getRandomElement
       rk8 <- getRandomElement
       rg1 <- getRandomElement
       rg2 <- getRandomElement
       rg3 <- getRandomElement
       rg4 <- getRandomElement
       rg5 <- getRandomElement
       rg6 <- getRandomElement
       rg7 <- getRandomElement
       rg8 <- getRandomElement
       return $! draeEncodeMul skp x1 x2 r1 r2 r3 r4 r5 r6 r7 r8
                                   rk1 rg1 rk2 rg2 rk3 rg3 rk4 rg4
                                   rk5 rg5 rk6 rg6 rk7 rg7 rk8 rg8

-- |'DRAE' for a multiplication /f(x1, x2) = x1 * x2/
draeEncodeMul :: forall el. Field el
              => DualKey el          -- ^ The static dual key
              -> DualEncPrimExpr el  -- ^ /x1/
              -> DualEncPrimExpr el  -- ^ /x2/
              -> el                  -- ^ /r1/
              -> el                  -- ^ /r2/
              -> el                  -- ^ /r3/
              -> el                  -- ^ /r4/
              -> el                  -- ^ /r5/
              -> el                  -- ^ /r6/
              -> el                  -- ^ /r7/
              -> el                  -- ^ /r8/
              -> el                  -- ^ /kappa1/
              -> el                  -- ^ /gamma1/
              -> el                  -- ^ /kappa2/
              -> el                  -- ^ /gamma2/
              -> el                  -- ^ /kappa3/
              -> el                  -- ^ /gamma3/
              -> el                  -- ^ /kappa4/
              -> el                  -- ^ /gamma4/
              -> el                  -- ^ /kappa5/
              -> el                  -- ^ /gamma5/
              -> el                  -- ^ /kappa6/
              -> el                  -- ^ /gamma6/
              -> el                  -- ^ /kappa7/
              -> el                  -- ^ /gamma7/
              -> el                  -- ^ /kappa8/
              -> el                  -- ^ /gamma8/
              -> DRAE el             -- ^ The DRAE encoding /x1 * x2/
draeEncodeMul skp@(!skL, !skR) !x1 !x2 !r1 !r2 !r3 !r4 !r5 !r6 !r7 !r8
                               !rk1 !rg1 !rk2 !rg2 !rk3 !rg3 !rk4 !rg4
                               !rk5 !rg5 !rk6 !rg6 !rk7 !rg7 !rk8 !rg8 =
    let !le1Ldom=decodeAndApplyL skp ((one-rk1)*skL)    x1 ((one-rk1)*(-r1)+rg1)
        !le1Lrez=decodeAndApplyR skp (rk1*skL)          x1 (rk1*(-r1)-rg1)
        !lr1L   = RT (le1Ldom, le1Lrez)
        --
        !le2Ldom=decodeAndApplyR skp ((one-rk2)*skL*r2) x1 ((one-rk2)*r3+rg2)
        !le2Lrez=decodeAndApplyL skp (rk2*skL*r2)       x1 (rk2*r3-rg2)
        --
        !le3Ldom=decodeAndApplyL skp (one-rk3)          x2 ((one-rk3)*(-r2)+rg3)
        !le3Lrez=decodeAndApplyR skp rk3                x2 (rk3*(-r2)-rg3)
        !lr3L   = RT (le3Ldom, le3Lrez)
        --
        !le4Ldom=decodeAndApplyR skp ((one-rk4)*r1)     x2 ((one-rk4)*r4+rg4)
        !le4Lrez=decodeAndApplyL skp (rk4*r1)           x2 (rk4*r4-rg4)
        --
        !le1Rdom=decodeAndApplyR skp ((one-rk5)*skR)    x1 ((one-rk5)*(-r5)+rg5)
        !le1Rrez=decodeAndApplyL skp (rk5*skR)          x1 (rk5*(-r5)-rg5)
        !lr1R   = RT (le1Rdom, le1Rrez)
        --
        !le2Rdom=decodeAndApplyL skp ((one-rk6)*skR*r6) x1 ((one-rk6)*r7+rg6)
        !le2Rrez=decodeAndApplyR skp (rk6*skR*r6)       x1 (rk6*r7-rg6)
        --
        !le3Rdom=decodeAndApplyR skp (one-rk7)          x2 ((one-rk7)*(-r6)+rg7)
        !le3Rrez=decodeAndApplyL skp rk7                x2 (rk7*(-r6)-rg7)
        !lr3R   = RT (le3Rdom, le3Rrez)
        --
        !le4Rdom=decodeAndApplyL skp ((one-rk8)*r5)     x2 ((one-rk8)*r8+rg8)
        !le4Rrez=decodeAndApplyR skp (rk8*r5)           x2 (rk8*r8-rg8)
        --
        !dkpL = r1*r2+r3+r4
        !dkpR = r5*r6+r7+r8
        bkp :: DualKeyPair el
        !bkp = (skp, (dkpL, dkpR))
        in DRAE bkp
                (DL.singleton $ MulTermRadicals { mtrLeft  = DLR (lr1L, lr3L)
                                                , mtrRight = DLR (lr1R, lr3R)
                                                })
                (DL.fromList [ (le2Ldom, le2Rdom)
                             , (le2Lrez, le2Rrez)
                             , (le4Ldom, le4Rdom)
                             , (le4Lrez, le4Rrez)
                             ]
                )

-- |'DRAE' for an addition (/f(x1, x2) = x1 + x2/) getting randoms from
-- generator.
draeEncodeAddRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => DualKey el         -- ^ The static dual key
                 -> DualEncPrimExpr el -- ^ /x1/
                 -> DualEncPrimExpr el -- ^ /x2/
                 -> CRandT g GenError m (DRAE el)
draeEncodeAddRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $! draeEncodeAdd skp x1 x2 r1 r2 r3 r4

-- |'DRAE' for an addition /f(x1, x2) = x1 + x2/
draeEncodeAdd :: forall el. (Field el)
              => DualKey el         -- ^ The static dual key
              -> DualEncPrimExpr el -- ^ /x1/
              -> DualEncPrimExpr el -- ^ /x2/
              -> el                 -- ^ /r1/
              -> el                 -- ^ /r2/
              -> el                 -- ^ /r3/
              -> el                 -- ^ /r4/
              -> DRAE el            -- The DRAE encoding /x1 + x2/
draeEncodeAdd skp@(!skL, !skR) !x1 !x2 !r1 !r2 !r3 !r4 =
    let !dkpL = r1 + r3
        !dkpR = r2 + r4
        bkp :: DualKeyPair el
        !bkp = (skp, (dkpL, dkpR))
        !le1L = decodeAndApplyL skp skL x1 r1
        !le1R = decodeAndApplyR skp skR x1 r2
        !le2L = decodeAndApplyL skp skL x2 r3
        !le2R = decodeAndApplyR skp skR x2 r4
     in DRAE bkp
             DL.empty
             (DL.fromList [ (le1L, le1R)
                          , (le2L, le2R)
                          ]
             )

decodeAndApply :: Field el
               => (DualVarName -> VariableName)
               -> (DualKey el -> Key el)
               -> DualKey el
               -> el
               -> DualEncPrimExpr el
               -> el
               -> LinearExpr el
decodeAndApply prjVar prjKey skp slope x =
    LE.apply slope (decodeDualEncPrimExpr skp prjVar prjKey x)

decodeAndApplyL :: Field el
                => DualKey el
                -> el
                -> DualEncPrimExpr el
                -> el
                -> LinearExpr el
decodeAndApplyL = decodeAndApply dvnLeftVarName fst

decodeAndApplyR :: Field el
                => DualKey el
                -> el
                -> DualEncPrimExpr el
                -> el
                -> LinearExpr el
decodeAndApplyR = decodeAndApply dvnRightVarName snd

decodeDualEncPrimExpr :: forall el. Field el
                    => DualKey el
                    -> (DualVarName -> VariableName)
                    -> (DualKey el -> Key el)
                    -> DualEncPrimExpr el
                    -> LinearExpr el
decodeDualEncPrimExpr skp prjVar prjKey depe =
    case depe of
      DualConst el -> ConstLinearExpr el
      DualVar dkp var ->
          let skInv :: Key el  -- the inverted static key
              skInv = (invert . prjKey) skp
           in LinearExpr skInv (prjVar var) (-prjKey dkp * skInv)

-- |'DRAE' for an addition of two 'DRAE's.
draeEncodeDRAEAdd :: Field el
                  => DualKey el -- ^ The static dual key.
                  -> DRAE el    -- ^ /D1/
                  -> DRAE el    -- ^ /D2/
                  -> DRAE el    -- ^ The DRAE encoding /D1 + D2/
draeEncodeDRAEAdd skp
                  (DRAE (dlSkp, dlDkp) dlMuls dlAdds)
                  (DRAE (drSkp, drDkp) drMuls drAdds) =
    let (!dlDkpL, !dlDkpR) = dlDkp
        (!drDkpL, !drDkpR) = drDkp
        !dkp = (dlDkpL + drDkpL, dlDkpR + drDkpR)
     in if (skp /= dlSkp) || (skp /= drSkp)
           then error "skps differ"
           else DRAE (skp, dkp)
                     (dlMuls `DL.append` drMuls)
                     (dlAdds `DL.append` drAdds)

-- |'DRAE' for a 'DRAE' addition getting randoms from generator.
draeEncodeDRAEAddRnd :: (Monad m, CryptoRandomGen g, Field el)
                     => DualKey el -- ^ The static dual key.
                     -> DRAE el    -- ^ /D1/
                     -> DRAE el    -- ^ /D2/
                     -> CRandT g GenError m (DRAE el)
draeEncodeDRAEAddRnd skp dl dr =
    return $! draeEncodeDRAEAdd skp dl dr

draeEncodePrimaryExprRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                         => DualKey el
                         -> DualEncPrimExpr el
                         -> CRandT g GenError m (DRAE el)
draeEncodePrimaryExprRnd skp e =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       return $! draeEncodePrimaryExpr skp e r1 r2

-- |'DRAE' encode primary expressions
draeEncodePrimaryExpr :: Field el
                      => DualKey el          -- ^ The static dual key
                      -> DualEncPrimExpr el  -- ^ The primary expression
                      -> el                  -- ^ /r1/
                      -> el                  -- ^ /r2/
                      -> DRAE el
draeEncodePrimaryExpr skp@(!skpL, !skpR) e !r1 !r2 =
    case e of
      DualConst c ->
          DRAE (skp, (r1, r2))
               DL.empty
               (DL.singleton ( ConstLinearExpr (skpL * c + r1)
                             , ConstLinearExpr (skpR * c + r2)
                             )
               )
      DualVar dkp v ->
          DRAE (skp, dkp)
               DL.empty
               (DL.singleton ( LinearExpr one (dvnLeftVarName v) zero
                             , LinearExpr one (dvnRightVarName v) zero
                             )
               )

type DRACGenMonad g el = CRandT g GenError (StateT Int (Writer (DRAC el)))

_CONST_0_ :: Field e => PrimaryExpression e
_CONST_0_ = Constant 0

priExFromExpr :: Field el
              => Map VariableName (DualKey el)
              -> Expr el
              -> Maybe (DualEncPrimExpr el)
priExFromExpr dkps expr =
    case expr of
      Op {} -> Nothing
      Var v -> Just $ varToDualVar dkps $ T.pack v
      Literal l -> Just $ DualConst l

varToDualVar :: Field el
           => Map VariableName (DualKey el)
           -> VariableName
           -> DualEncPrimExpr el
varToDualVar dkps v =
    case M.lookup v dkps of
      Just dkp -> DualVar dkp (genDualVarName v)
      Nothing -> error $ "lookup in initial dkps failed for " ++ T.unpack v

freshVar :: (CryptoRandomGen g, Field el)
         => (DRACGenMonad g el) DualVarName
freshVar =
    do n <- lift get
       lift $ put (n+1)
       return $! genDualVarName ("_t" `T.append` T.pack (show n))

exprToDRAE :: (CryptoRandomGen g, Field el, CRandom el)
           => DualKey el
           -> Map VariableName (DualKey el)
           -> Expr el
           -> (DRACGenMonad g el) (DRAE el)
exprToDRAE skp initDkps expr =
    case expr of
      Op op exprL exprR ->
          case op of
            Plus ->
                case ( priExFromExpr initDkps exprL
                     , priExFromExpr initDkps exprR
                     ) of
                  (Just peL, Just peR) ->
                      draeEncodeAddRnd skp peL peR
                  _ -> do l <- exprToDRAE skp initDkps exprL
                          r <- exprToDRAE skp initDkps exprR
                          draeEncodeDRAEAddRnd skp l r
            Minus -> error "DRAE compiler: minus not implemented"
            Times ->
                case ( priExFromExpr initDkps exprL
                     , priExFromExpr initDkps exprR
                     ) of
                  (Just peL, Just peR) ->
                      draeEncodeMulRnd skp peL peR
                  _ -> do l <- exprToDRAE skp initDkps exprL
                          r <- exprToDRAE skp initDkps exprR
                          varL <- freshVar
                          varR <- freshVar
                          encVarL <- putDRAE l varL
                          encVarR <- putDRAE r varR
                          draeEncodeMulRnd skp encVarL encVarR
      Var v -> draeEncodePrimaryExprRnd skp $ varToDualVar initDkps $ T.pack v
      Literal l -> draeEncodePrimaryExprRnd skp $ DualConst l
      where putDRAE :: (CryptoRandomGen g, Field el)
                    => DRAE el
                    -> DualVarName
                    -> (DRACGenMonad g el) (DualEncPrimExpr el)
            putDRAE drae@(DRAE (_, !dkp) _ _) !varName =
               do lift $ tell $! DL.singleton (varName, drae)
                  return $! DualVar dkp varName

finalDRAE :: forall el. forall g. (CryptoRandomGen g, Field el)
          => DualKey el
          -> DualKey el
          -> (DRACGenMonad g el) (DRAE el)
finalDRAE (skpL, skpR) (dkpL, dkpR) =
    do let finalSK = skpL + skpR
           finalDK = dkpL + dkpR
           finalSKinv = invert finalSK
           decodedLE = LinearExpr finalSKinv
                                  _SPECIAL_VAR_ADDED_PRE_OUT_
                                  (-finalDK * finalSKinv)
       return $ DRAE ((one, one), (zero, zero))
                     DL.empty
                     (DL.singleton (decodedLE, decodedLE))

collectInitialVariables :: forall el. Field el => Expr el -> [VariableName]
collectInitialVariables expr =
    let collect' =
            case expr of
              Literal _ -> []
              Var v -> [T.pack v]
              Op _ el er -> collectInitialVariables el ++
                            collectInitialVariables er
     in nub collect'

genDynamicKey :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
              => VariableName
              -> CRandT g GenError m (VariableName, (el, el))
genDynamicKey v =
    do dkL <- getRandomElement
       dkR <- getRandomElement
       return (v, (dkL, dkR))

initialVarDRAE :: (CryptoRandomGen g, Field el, CRandom el)
               => DualKey el
               -> (VariableName, DualKey el)
               -> (DRACGenMonad g el) ()
initialVarDRAE skp@(skL, skR) (v, dkp@(dkL, dkR)) =
    let drae = DRAE (skp, dkp)
                    DL.empty
                    (DL.singleton (LinearExpr skL v dkL,  LinearExpr skR v dkR))
     in lift $ tell $! DL.singleton (genDualVarName v, drae)

genSkp :: (CryptoRandomGen g, Field el, CRandom el)
       => (DRACGenMonad g el) (DualKey el)
genSkp =
    do skpL <- getRandomInvertibleElement
       skpR <- getRandomInvertibleElement
       if skpL + skpR == zero -- the sum has to be invertible, too
          then genSkp
          else return (skpL, skpR)

-- | Encode an 'Expr' as a 'DRAC'.
exprToDRAC :: forall g. forall el. (CryptoRandomGen g, Field el, CRandom el)
         => g       -- ^ The random number generator
         -> Expr el -- ^ The arithmetic expression to encode
         -> (Either GenError g, DRAC el) -- ^ The 'DRAC' encoding the 'Expr'
exprToDRAC g expr =
   let act :: (DRACGenMonad g el) (DRAE el)
       act = do skp <- genSkp
                let initVars = collectInitialVariables expr
                dkps <- mapM genDynamicKey initVars
                let initialKeys = M.fromList dkps
                mapM_ (initialVarDRAE skp) dkps
                zDRAE <- exprToDRAE skp initialKeys expr
                lift $ tell $! DL.singleton (_SPECIAL_DUAL_VAR_PRE_OUT_, zDRAE)
                let (DRAE (_, dkp) _ _) = zDRAE
                finalDRAE skp dkp
       (ge, draes) = runWriter (evalStateT (runCRandT act g) 0)
       lastDRAE =
           case ge of
             Left _ -> DL.empty
             Right (ld, _) -> DL.singleton (_SPECIAL_DUAL_VAR_OUT_, ld)
       errOrGen =
           case ge of
             Left err -> Left err
             Right (_, gen) -> Right gen
   in (errOrGen, draes `DL.append` lastDRAE)
