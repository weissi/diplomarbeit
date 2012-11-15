{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.RAE.Encoder.Internal.DRAC
    ( -- * Public API
      exprToDRAC
      -- * Internal, Interesting API
      -- ** Real, Pure @DRAE@ Encoding
    , draeEncodeMul
    , draeEncodeAdd
    , draeEncodeDRAEAdd
    , draeEncodePrimaryExpr
      -- * Internal, Uninteresting API
      -- ** @DRAE@ Encoding automatically getting random numbers
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
import Data.RAE.Evaluation ( _SPECIAL_VAR_OUT_, _SPECIAL_VAR_ADDED_PRE_OUT_
                           , _SPECIAL_VAR_PRE_OUT_
                           )
import Data.RAE.Types ( VariableName, DRAE(..), DRAC
                      , PrimaryExpression(..)
                      , Key, DualKey, DualKeyPair
                      , leftVar, rightVar
                      )
import qualified Data.LinearExpression as LE

-- | A dual encrypted primary expression.
--
-- Either an unencrypted constant or an encrypted variable along with the dual
-- key (only the dynamic keys) needed to decrypt it.
data DualEncPrimExpr el = DualConst el
                        | DualVar (DualKey el) VariableName
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

-- |@DRAE@ for a multiplication getting randoms from generator
draeEncodeMulRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => DualKey el
                 -> DualEncPrimExpr el
                 -> DualEncPrimExpr el
                 -> CRandT g GenError m (DRAE el)
draeEncodeMulRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       r5 <- getRandomElement
       r6 <- getRandomElement
       r7 <- getRandomElement
       r8 <- getRandomElement
       return $! draeEncodeMul skp x1 x2 r1 r2 r3 r4 r5 r6 r7 r8

-- |@DRAE@ for a multiplication /f(x1, x2) = x1 * x2/
-- modeled after How to Garble Arithmetic Circuits, p. 13
draeEncodeMul :: forall el. Field el
              => DualKey el
              -> DualEncPrimExpr el
              -> DualEncPrimExpr el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> DRAE el
draeEncodeMul skp@(!skL, !skR) !x1 !x2 !r1 !r2 !r3 !r4 !r5 !r6 !r7 !r8 =
    let !le1L = decodeAndApplyL skp skL      x1 (-r1)
        !le2L = decodeAndApplyR skp (skL*r2) x1 r3
        !le3L = decodeAndApplyL skp one      x2 (-r2)
        !le4L = decodeAndApplyR skp r1       x2 r4
        !le1R = decodeAndApplyR skp skR      x1 (-r5)
        !le2R = decodeAndApplyL skp (skR*r6) x1 r7
        !le3R = decodeAndApplyR skp one      x2 (-r6)
        !le4R = decodeAndApplyL skp r5       x2 r8
        !dkpL = r1*r2+r3+r4
        !dkpR = r5*r6+r7+r8
        bkp :: DualKeyPair el
        !bkp = (skp, (dkpL, dkpR))
        in DRAE bkp                               -- < the DualKeyPair
                (DL.fromList [ ((le1L, le1R)      -- / the multiplcative
                             , (le3L, le3R))      -- \   terms
                             ]
                )
                (DL.fromList [ (le2L, le2R)       -- / the additive
                             , (le4L, le4R)       -- \    terms
                             ]
                )

-- |@DRAE@ for an addition getting randoms from generator
draeEncodeAddRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => DualKey el
                 -> DualEncPrimExpr el
                 -> DualEncPrimExpr el
                 -> CRandT g GenError m (DRAE el)
draeEncodeAddRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $! draeEncodeAdd skp x1 x2 r1 r2 r3 r4

-- |@DRAE@ for an addition /f(x1, x2) = x1 + x2/
draeEncodeAdd :: forall el. (Field el)
              => DualKey el
              -> DualEncPrimExpr el
              -> DualEncPrimExpr el
              -> el
              -> el
              -> el
              -> el
              -> DRAE el
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
               => (VariableName -> VariableName)
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
decodeAndApplyL = decodeAndApply leftVar fst

decodeAndApplyR :: Field el
                => DualKey el
                -> el
                -> DualEncPrimExpr el
                -> el
                -> LinearExpr el
decodeAndApplyR = decodeAndApply rightVar snd

decodeDualEncPrimExpr :: forall el. Field el
                    => DualKey el
                    -> (VariableName -> VariableName)
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

-- |@DRAE@ for an addition of two @DRAE@s
draeEncodeDRAEAdd :: Field el
                  => DualKey el
                  -> DRAE el
                  -> DRAE el
                  -> DRAE el
draeEncodeDRAEAdd skp
                  (DRAE (dlSkp, dlDkp) dlMuls dlAdds)
                  (DRAE (drSkp, drDkp) drMuls drAdds) =
    let (dlDkpL, dlDkpR) = dlDkp
        (drDkpL, drDkpR) = drDkp
        dkp = (dlDkpL + drDkpL, dlDkpR + drDkpR)
        _ = if (skp /= dlSkp) || (skp /= drSkp) then error "skps differ" else ()
     in DRAE (skp, dkp) (dlMuls `DL.append` drMuls) (dlAdds `DL.append` drAdds)

-- |@DRAE@ for a @DRAE@ addition getting randoms from generator
draeEncodeDRAEAddRnd :: (Monad m, CryptoRandomGen g, Field el)
                     => DualKey el
                     -> DRAE el
                     -> DRAE el
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

-- |@DRAE@ encode primary expressions
draeEncodePrimaryExpr :: Field el
                      => DualKey el
                      -> DualEncPrimExpr el
                      -> el
                      -> el
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
               (DL.singleton ( LinearExpr one (leftVar v) zero
                             , LinearExpr one (rightVar v) zero
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
      Just dkp -> DualVar dkp v
      Nothing -> error $ "lookup in initial dkps failed for " ++ T.unpack v

freshVar :: (CryptoRandomGen g, Field el)
         => (DRACGenMonad g el) VariableName
freshVar =
    do n <- lift get
       lift $ put (n+1)
       return $ "_t" `T.append` T.pack (show n)

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
                    -> VariableName
                    -> (DRACGenMonad g el) (DualEncPrimExpr el)
            putDRAE drae@(DRAE (_, dkp) _ _) varName =
               do lift $ tell $ DL.singleton (varName, drae)
                  return $ DualVar dkp varName

finalDRAE :: forall el. forall g. (CryptoRandomGen g, Field el)
          => DualKey el
          -> DualKey el
          -> VariableName
          -> (DRACGenMonad g el) (DRAE el)
finalDRAE (skpL, skpR) (dkpL, dkpR) varName =
    do let finalSK = skpL + skpR
           finalDK = dkpL + dkpR
           finalSKinv = invert finalSK
           decodedLE = LinearExpr finalSKinv varName (-finalDK * finalSKinv)
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
                    (DL.singleton (LinearExpr skL v dkL, LinearExpr skR v dkR))
     in lift $ tell $ DL.singleton (v, drae)

genSkp :: (CryptoRandomGen g, Field el, CRandom el)
       => (DRACGenMonad g el) (DualKey el)
genSkp =
    do skpL <- getRandomInvertibleElement
       skpR <- getRandomInvertibleElement
       if skpL + skpR == zero -- the sum has to be invertible, too
          then genSkp
          else return (skpL, skpR)

-- | Encode an @Expr@ as a @DRAC@.
exprToDRAC :: forall g. forall el. (CryptoRandomGen g, Field el, CRandom el)
         => g
         -> Expr el
         -> (Either GenError g, DRAC el)
exprToDRAC g expr =
   let act :: (DRACGenMonad g el) (DRAE el)
       act = do skp <- genSkp
                let initVars = collectInitialVariables expr
                dkps <- mapM genDynamicKey initVars
                let initialKeys = M.fromList dkps
                mapM_ (initialVarDRAE skp) dkps
                zDRAE <- exprToDRAE skp initialKeys expr
                lift $ tell $ DL.singleton (_SPECIAL_VAR_PRE_OUT_, zDRAE)
                let (DRAE (_, dkp) _ _) = zDRAE
                finalDRAE skp dkp _SPECIAL_VAR_ADDED_PRE_OUT_
       (ge, draes) = runWriter (evalStateT (runCRandT act g) 0)
       lastDRAE =
           case ge of
             Left _ -> DL.empty
             Right (ld, _) -> DL.singleton (_SPECIAL_VAR_OUT_, ld)
       errOrGen =
           case ge of
             Left err -> Left err
             Right (_, gen) -> Right gen
   in (errOrGen, draes `DL.append` lastDRAE)
