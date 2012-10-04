{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.DARE ( dareEncodeMulRnd
                  , dareEncodeAddRnd
                  , dareEncodeDareAddRnd
                  , dareEncodePrimaryExpr
                  , dareDecode
                  , exprToRP
                  -- Just for the tests
                  , dareEncodeMul
                  , dareEncodeAdd
                  , dareEncodeDareAdd
                  ) where

-- # STANDARD LIBRARY
import Data.List (foldl', nub)
import Data.Map (Map)
import Control.Monad (liftM2)
import Control.Monad.Writer.Lazy (WriterT, tell, runWriterT)
import Control.Monad.State.Strict (State, evalState, get, put)
import Control.Monad.Trans (lift)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Text as T

-- # SITE PACKAGES
import Control.Monad.CryptoRandom (CRandT, getCRandom, runCRandT, CRandom)
import Crypto.Random (GenError, CryptoRandomGen)

-- # LOCAL
import Data.DARETypes ( VariableName, VarMapping, DARE(..), RP
                      , PrimaryExpression(..)
                      , Key, KeyPair, BiKeyPair, BiEncPrimExpr(..)
                      , leftVar, rightVar
                      , _SPECIAL_VAR_OUT_, _SPECIAL_VAR_ADDED_PRE_OUT_
                      , _SPECIAL_VAR_PRE_OUT_
                      )
import Data.ExpressionTypes (Expr(..), Operator(..))
import Data.FieldTypes (Field(..))
import Data.LinearExpression (LinearExpr(..))
import qualified Data.LinearExpression as LE

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

-- |DARE for a multiplication getting randoms from generator
dareEncodeMulRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => KeyPair el
                 -> BiEncPrimExpr el
                 -> BiEncPrimExpr el
                 -> CRandT g GenError m (DARE el)
dareEncodeMulRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       r5 <- getRandomElement
       r6 <- getRandomElement
       r7 <- getRandomElement
       r8 <- getRandomElement
       return $! dareEncodeMul skp x1 x2 r1 r2 r3 r4 r5 r6 r7 r8

-- |DARE for a multiplication f(x1, x2) = x1 * x2
-- modeled after How to Garble Arithmetic Circuits, p. 13
dareEncodeMul :: forall el. Field el
              => KeyPair el
              -> BiEncPrimExpr el
              -> BiEncPrimExpr el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> el
              -> DARE el
dareEncodeMul skp@(!skL, !skR) !x1 !x2 !r1 !r2 !r3 !r4 !r5 !r6 !r7 !r8 =
    let !le1L = decodeAndApplyL skp skL      x1 (-r1)
        !le2L = decodeAndApplyR skp (skL*r2) x1 r3
        !le3L = decodeAndApplyL skp 1        x2 (-r2)
        !le4L = decodeAndApplyR skp r1       x2 r4
        !le1R = decodeAndApplyR skp skR      x1 (-r5)
        !le2R = decodeAndApplyL skp (skR*r6) x1 r7
        !le3R = decodeAndApplyR skp 1        x2 (-r6)
        !le4R = decodeAndApplyL skp r5       x2 r8
        !dkpL = r1*r2+r3+r4
        !dkpR = r5*r6+r7+r8
        bkp :: BiKeyPair el
        !bkp = (skp, (dkpL, dkpR))
        in DARE bkp                               -- < the BiKeyPair
                (DL.fromList [ ((le1L, le1R)      -- / the multiplcative
                             , (le3L, le3R))      -- \   terms
                             ]
                )
                (DL.fromList [ (le2L, le2R)       -- / the additive
                             , (le4L, le4R)       -- \    terms
                             ]
                )

-- |DARE for an addition getting randoms from generator
dareEncodeAddRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                 => KeyPair el
                 -> BiEncPrimExpr el
                 -> BiEncPrimExpr el
                 -> CRandT g GenError m (DARE el)
dareEncodeAddRnd skp x1 x2 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $! dareEncodeAdd skp x1 x2 r1 r2 r3 r4

-- |DARE for an addition f(x1, x2) = x1 + x2
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeAdd :: forall el. (Field el)
              => KeyPair el
              -> BiEncPrimExpr el
              -> BiEncPrimExpr el
              -> el
              -> el
              -> el
              -> el
              -> DARE el
dareEncodeAdd skp@(!skL, !skR) !x1 !x2 !r1 !r2 !r3 !r4 =
    let !dkpL = r1 + r3
        !dkpR = r2 + r4
        bkp :: BiKeyPair el
        !bkp = (skp, (dkpL, dkpR))
        !le1L = decodeAndApplyL skp skL x1 r1
        !le1R = decodeAndApplyR skp skR x1 r2
        !le2L = decodeAndApplyL skp skL x2 r3
        !le2R = decodeAndApplyR skp skR x2 r4
     in DARE bkp
             DL.empty
             (DL.fromList [ (le1L, le1R)
                          , (le2L, le2R)
                          ]
             )

decodeAndApply :: Field el
               => (VariableName -> VariableName)
               -> (KeyPair el -> Key el)
               -> KeyPair el
               -> el
               -> BiEncPrimExpr el
               -> el
               -> LinearExpr el
decodeAndApply prjVar prjKey skp slope x intercept =
    LE.apply slope (decodeBiEncPrimExpr skp prjVar prjKey x) intercept

decodeAndApplyL :: Field el
                => KeyPair el
                -> el
                -> BiEncPrimExpr el
                -> el
                -> LinearExpr el
decodeAndApplyL = decodeAndApply leftVar fst

decodeAndApplyR :: Field el
                => KeyPair el
                -> el
                -> BiEncPrimExpr el
                -> el
                -> LinearExpr el
decodeAndApplyR = decodeAndApply rightVar snd

decodeBiEncPrimExpr :: forall el. Field el
                    => KeyPair el
                    -> (VariableName -> VariableName)
                    -> (KeyPair el -> Key el)
                    -> BiEncPrimExpr el
                    -> LinearExpr el
decodeBiEncPrimExpr skp prjVar prjKey bepc =
    case bepc of
      BiConst el -> ConstLinearExpr el
      BiVar dkp var ->
          let skInv :: Key el  -- the inverted static key
              skInv = (invert . prjKey) skp
           in LinearExpr skInv (prjVar var) (-prjKey dkp * skInv)

-- |DARE for an addition of two DAREs
dareEncodeDareAdd :: Field el
                  => KeyPair el
                  -> DARE el
                  -> DARE el
                  -> DARE el
dareEncodeDareAdd skp
                  (DARE (dlSkp, dlDkp) dlMuls dlAdds)
                  (DARE (drSkp, drDkp) drMuls drAdds) =
    let (dlDkpL, dlDkpR) = dlDkp
        (drDkpL, drDkpR) = drDkp
        dkp = (dlDkpL + drDkpL, dlDkpR + drDkpR)
        _ = if (skp /= dlSkp) || (skp /= drSkp) then error "skps differ" else ()
     in DARE (skp, dkp) (dlMuls `DL.append` drMuls) (dlAdds `DL.append` drAdds)

-- |DARE for a DARE addition getting randoms from generator
dareEncodeDareAddRnd :: (Monad m, CryptoRandomGen g, Field el)
                     => KeyPair el
                     -> DARE el
                     -> DARE el
                     -> CRandT g GenError m (DARE el)
dareEncodeDareAddRnd skp dl dr =
    do return $! dareEncodeDareAdd skp dl dr

dareEncodePrimaryExprRnd :: (Monad m, CryptoRandomGen g, Field el, CRandom el)
                         => KeyPair el
                         -> BiEncPrimExpr el
                         -> CRandT g GenError m (DARE el)
dareEncodePrimaryExprRnd skp e =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       return $! dareEncodePrimaryExpr skp e r1 r2

-- |DARE encode primary expressions
dareEncodePrimaryExpr :: Field el
                      => KeyPair el
                      -> BiEncPrimExpr el
                      -> el
                      -> el
                      -> DARE el
dareEncodePrimaryExpr skp@(!skpL, !skpR) e !r1 !r2 =
    case e of
      BiConst c ->
          DARE (skp, (r1, r2))
               DL.empty
               (DL.singleton ( ConstLinearExpr (skpL * c + r1)
                             , ConstLinearExpr (skpR * c + r2)
                             )
               )
      BiVar dkp v ->
          DARE (skp, dkp)
               DL.empty
               (DL.singleton ( LinearExpr 1 (leftVar v) 0
                             , LinearExpr 1 (rightVar v) 0
                             )
               )

-- |DARE decoder
dareDecode :: forall el. Field el
           => VarMapping el
           -> DARE el
           -> Maybe (el, el)
dareDecode varMap (DARE _ muls adds) =
    do l <- outL
       r <- outR
       return (l, r)
    where addValsM :: ((LinearExpr el, LinearExpr el) -> LinearExpr el)
                   -> [Maybe el]
          addValsM prj = map (LE.evaluate varMap) (map prj $ DL.toList adds)
          mulValsM :: ((LinearExpr el, LinearExpr el) -> LinearExpr el)
                   -> [Maybe el]
          mulValsM prj = map (doMul prj) $ DL.toList muls
          doMul prj (lel, ler) = liftM2 (*) (LE.evaluate varMap (prj lel))
                                        (LE.evaluate varMap (prj ler))
          outL = case sequence (addValsM fst ++ mulValsM fst) of
                   Nothing -> Nothing
                   Just vals -> Just $ (foldl' (+) 0 vals)
          outR = case sequence (addValsM snd ++ mulValsM snd) of
                   Nothing -> Nothing
                   Just vals -> Just $ (foldl' (+) 0 vals)

type RPGenMonad g el = CRandT g GenError (WriterT (RP el) (State Int))

_CONST_0_ :: Field e => PrimaryExpression e
_CONST_0_ = Constant 0

priExFromExpr :: Field el
              => Map VariableName (KeyPair el)
              -> Expr el
              -> Maybe (BiEncPrimExpr el)
priExFromExpr dkps expr =
    case expr of
      Op {} -> Nothing
      Var v -> Just $ varToBiVar dkps $ T.pack v
      Literal l -> Just $ BiConst l

varToBiVar :: Field el
           => Map VariableName (KeyPair el)
           -> VariableName
           -> BiEncPrimExpr el
varToBiVar dkps v =
    case M.lookup v dkps of
      Just dkp -> BiVar dkp v
      Nothing -> error $ "lookup in initial dkps failed for "++(T.unpack v)

freshVar :: (CryptoRandomGen g, Field el)
         => (RPGenMonad g el) VariableName
freshVar =
    do n <- lift $ get
       lift $ put (n+1)
       return $ "t" `T.append` (T.pack $ show n)

exprToRP' :: (CryptoRandomGen g, Field el, CRandom el)
          => KeyPair el
          -> Map VariableName (KeyPair el)
          -> Expr el
          -> (RPGenMonad g el) (DARE el)
exprToRP' skp initDkps expr =
    case expr of
      Op op exprL exprR ->
          case op of
            Plus ->
                case ( priExFromExpr initDkps exprL
                     , priExFromExpr initDkps exprR
                     ) of
                  (Just peL, Just peR) ->
                      dareEncodeAddRnd skp peL peR
                  _ -> do l <- exprToRP' skp initDkps exprL
                          r <- exprToRP' skp initDkps exprR
                          dareEncodeDareAddRnd skp l r
            Minus -> error "DARE compiler: minus not implemented"
            Times ->
                case ( priExFromExpr initDkps exprL
                     , priExFromExpr initDkps exprR
                     ) of
                  (Just peL, Just peR) ->
                      dareEncodeMulRnd skp peL peR
                  _ -> do l <- exprToRP' skp initDkps exprL
                          r <- exprToRP' skp initDkps exprR
                          varL <- freshVar
                          varR <- freshVar
                          encVarL <- putDare l varL
                          encVarR <- putDare r varR
                          dareEncodeMulRnd skp encVarL encVarR
      Var v -> dareEncodePrimaryExprRnd skp $ varToBiVar initDkps $ T.pack v
      Literal l -> dareEncodePrimaryExprRnd skp $ BiConst l
      where putDare :: (CryptoRandomGen g, Field el)
                    => DARE el
                    -> VariableName
                    -> (RPGenMonad g el) (BiEncPrimExpr el)
            putDare dare@(DARE (_, dkp) _ _) varName =
               do lift $ tell $ DL.singleton (varName, dare)
                  return $ BiVar dkp varName

finalDARE :: forall el. forall g. (CryptoRandomGen g, Field el)
          => KeyPair el
          -> KeyPair el
          -> VariableName
          -> (RPGenMonad g el) (DARE el)
finalDARE (skpL, skpR) (dkpL, dkpR) varName =
    do let finalSK = skpL + skpR
           finalDK = dkpL + dkpR
           finalSKinv = invert finalSK
           decodedLE = LinearExpr finalSKinv varName (-finalDK * finalSKinv)
       return $ DARE ((1,1), (0,0))
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

initialVarDARE :: (CryptoRandomGen g, Field el, CRandom el)
               => KeyPair el
               -> (VariableName, KeyPair el)
               -> (RPGenMonad g el) ()
initialVarDARE skp@(skL, skR) (v, dkp@(dkL, dkR)) =
    let dare = DARE (skp, dkp)
                    DL.empty
                    (DL.singleton (LinearExpr skL v dkL, LinearExpr skR v dkR))
     in lift $ tell $ DL.singleton (v, dare)

genSkp :: (CryptoRandomGen g, Field el, CRandom el)
       => (RPGenMonad g el) (KeyPair el)
genSkp =
    do skpL <- getRandomInvertibleElement
       skpR <- getRandomInvertibleElement
       if skpL + skpR == 0 -- the sum has to be invertible, too
          then genSkp
          else return (skpL, skpR)

exprToRP :: forall g. forall el. (CryptoRandomGen g, Field el, CRandom el)
         => g
         -> Expr el
         -> (Either GenError g, RP el)
exprToRP g expr =
   let act :: (RPGenMonad g el) (DARE el)
       act = do skp <- genSkp
                let initVars = collectInitialVariables expr
                dkps <- mapM genDynamicKey initVars
                let initialKeys = M.fromList dkps
                mapM_ (initialVarDARE skp) dkps
                zDare <- exprToRP' skp
                                   initialKeys
                                   expr
                lift $ tell $ DL.singleton (_SPECIAL_VAR_PRE_OUT_, zDare)
                let (DARE (_, dkp) _ _) = zDare
                finalDARE skp dkp _SPECIAL_VAR_ADDED_PRE_OUT_
       (ge, dares) = evalState (runWriterT (runCRandT act g)) 0
       lastDare =
           case ge of
             Left _ -> DL.empty
             Right (ld, _) -> DL.singleton (_SPECIAL_VAR_OUT_, ld)
       errOrGen =
           case ge of
             Left err -> Left err
             Right (_, gen) -> Right gen
   in (errOrGen, dares `DL.append` lastDare)
