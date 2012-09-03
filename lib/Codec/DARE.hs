{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Codec.DARE ( LinearExpr(..)
                  , DARE
                  , PrimaryExpression (..)
                  , VarMapping
                  , dareEncodeMulRnd
                  , dareEncodeAddRnd
                  , dareEncodeDareAddRnd
                  , dareEncodeEncMulRnd
                  , dareEncodePrimaryExpr
                  , dareDecode
                  , exprToRP
                  , runRP
                  -- Just for the tests
                  , dareEncodeMul
                  , dareEncodeAdd
                  , dareEncodeDareAdd
                  , dareEncodeEncMul
                  , addToDARE
                  , mulElementToDARE
                  ) where

-- # STANDARD LIBRARY
import Data.DList (DList)
import Data.List (foldl')
import Data.Map (Map)
import Control.Monad (liftM2)
import Control.Monad.State.Strict (State, get, put, execState)
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import Control.Monad.Trans (lift)
import qualified Data.DList as DL
import qualified Data.Map as M

-- # SITE PACKAGES
import Control.Monad.CryptoRandom (CRandT, getCRandom, runCRandT)
import Crypto.Random (GenError, CryptoRandomGen)
--import Math.Algebra.Field.Base (Fp)
--import Math.Common.IntegerAsType (IntegerAsType)

-- # LOCAL
import Data.ExpressionTypes (Expr(..), Operator(..))
import Data.FieldTypes (FieldElement(..))

--instance IntegerAsType n => FieldElement (Fp n) where
--    invert n =
--        case n of
--          0 -> error "0 is not invertible"
--          n' -> 1 / n'

type VariableName = String
type VarMapping el = Map VariableName el

data PrimaryExpression el = Variable VariableName
                          | Constant el

data LinearExpr el = LinearExpr { leSlope     :: el
                                , leVariable  :: VariableName
                                , leIntercept :: el
                                }
                   | ConstLinearExpr el

data DARE el = DARE [(LinearExpr el, LinearExpr el)]
                    [LinearExpr el]
                    deriving Show

type PrimaryExpressionEncryptionKey el = (el, el)

instance (Show el, FieldElement el) => Show (LinearExpr el) where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle

getRandomElement :: forall m g el. (Monad m, CryptoRandomGen g, FieldElement el)
                 => CRandT g GenError m el
getRandomElement =
    do rint <- getCRandom :: CRandT g GenError m Int
       return $ fromIntegral rint

getRandomInvertibleElement :: forall m g el.
                              (Monad m, CryptoRandomGen g, FieldElement el)
                           => CRandT g GenError m el
getRandomInvertibleElement =
    loop
    where loop =
              do rint <- getCRandom :: CRandT g GenError m Int
                 let rel = fromIntegral rint
                 if rel /= 0
                   then return rel
                   else loop

genLinearExpr :: FieldElement el
              => el
              -> PrimaryExpression el
              -> el
              -> LinearExpr el
genLinearExpr a b c =
    case b of
      Variable b' -> LinearExpr { leSlope = a
                                , leVariable = b'
                                , leIntercept = c
                                }
      Constant b' -> ConstLinearExpr (a * b' + c)

calcLinearExpr :: FieldElement el => VarMapping el -> LinearExpr el -> Maybe el
calcLinearExpr varMap le =
    case le of
      ConstLinearExpr cle -> Just cle
      LinearExpr s v i ->
          do vVal <- M.lookup v varMap
             return $ s * vVal + i

-- |DARE for a multiplication getting randoms from generator
dareEncodeMulRnd :: (Monad m, CryptoRandomGen g, FieldElement el)
                 => PrimaryExpression el
                 -> PrimaryExpression el
                 -> PrimaryExpression el
                 -> CRandT g GenError m (DARE el)
dareEncodeMulRnd x1 x2 x3 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $ dareEncodeMul x1 x2 x3 r1 r2 r3 r4

-- |DARE for an encrypted multiplication getting randoms from generator
dareEncodeEncMulRnd :: (Monad m, CryptoRandomGen g, FieldElement el)
                    => (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                    -> (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                    -> (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                    -> CRandT g GenError m (DARE el)
dareEncodeEncMulRnd x1 x2 x3 =
    do r1 <- getRandomElement
       r2 <- getRandomElement
       r3 <- getRandomElement
       r4 <- getRandomElement
       return $ dareEncodeEncMul x1 x2 x3 r1 r2 r3 r4

-- |DARE for an addition getting randoms from generator
dareEncodeAddRnd :: (Monad m, CryptoRandomGen g, FieldElement el)
                 => PrimaryExpression el
                 -> PrimaryExpression el
                 -> CRandT g GenError m (DARE el)
dareEncodeAddRnd x1 x2 =
    do r <- getRandomElement
       return $ dareEncodeAdd x1 x2 r

-- |DARE for an addition f(x1, x2) = x1 + x2
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeAdd :: FieldElement el
              => PrimaryExpression el
              -> PrimaryExpression el
              -> el
              -> DARE el
dareEncodeAdd x1 x2 r =
    let dareL = dareEncodePrimaryExpr x1
        dareR = dareEncodePrimaryExpr x2
        in dareEncodeDareAdd dareL dareR r

-- |DARE for a multiplication (+ addition) f(x1, x2, x3) = x1 * x2 + x3
-- see How to Garble Arithmetic Circuits, p. 13
dareEncodeMul :: FieldElement el
              => PrimaryExpression el
              -> PrimaryExpression el
              -> PrimaryExpression el
              -> el
              -> el
              -> el
              -> el
              -> DARE el
dareEncodeMul x1 x2 x3 r1 r2 r3 r4 =
    let le1 = genLinearExpr 1 x1 (-r1)
        le2 = genLinearExpr r2 x1 (-r1*r2 + r3)
        le3 = genLinearExpr 1 x2 (-r2)
        le4 = genLinearExpr r1 x2 r4
        le5 = genLinearExpr 1 x3 (-r3-r4)
        in DARE [(le1, le3)] [le2, le4, le5]

-- |DARE for an encrypted multiplication
dareEncodeEncMul :: FieldElement el
                 => (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                 -> (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                 -> (PrimaryExpression el, PrimaryExpressionEncryptionKey el)
                 -> el
                 -> el
                 -> el
                 -> el
                 -> DARE el
dareEncodeEncMul (x1, (k1a,k1b)) (x2, (k2a,k2b)) (x3, (k3a,k3b)) r1 r2 r3 r4 =
    let le1 = genLinearExpr (invert k1a) x1 (-k1b - r1) -- r1
        le2 = genLinearExpr (r2 * invert k1a) x1 (-r2*k1b - r1*r2 + r3) -- r1
        le3 = genLinearExpr (invert k2a) x2 (-k2b - r2) -- r2
        le4 = genLinearExpr (r1 * invert k2a) x2 (-r1*k2b + r4) -- r2
        le5 = genLinearExpr (invert k3a) x3 (-k3b -r3-r4) -- r3
        in DARE [(le1, le3)] [le2, le4, le5]

addToLinearExpression :: FieldElement el
                      => LinearExpr el
                      -> el
                      -> LinearExpr el
addToLinearExpression le el =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c + el)
      LinearExpr s v i -> LinearExpr s v (i + el)

mulElementToLinearExpression :: FieldElement el
                             => LinearExpr el
                             -> el
                             -> LinearExpr el
mulElementToLinearExpression le el =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c * el)
      LinearExpr s v i -> LinearExpr (s * el) v (i * el)

addToDARE :: FieldElement el
          => DARE el
          -> el
          -> DARE el
addToDARE (DARE muls adds) el =
    case adds of
      [] -> DARE muls [ConstLinearExpr el]
      (x:xs) -> DARE muls (addToLinearExpression x el : xs)

mulElementToDARE :: forall el. FieldElement el
                 => DARE el
                 -> el
                 -> DARE el
mulElementToDARE (DARE muls adds) el =
    let mulElementToLinearExpressions :: FieldElement el
                                      => (LinearExpr el, LinearExpr el)
                                      -> el
                                      -> (LinearExpr el, LinearExpr el)
        mulElementToLinearExpressions (le1, le2) e =
            ( mulElementToLinearExpression le1 e
            , le2
            )
        muls' :: [(LinearExpr el, LinearExpr el)]
        muls' = map (`mulElementToLinearExpressions` el) muls
        adds' :: [LinearExpr el]
        adds' = map (`mulElementToLinearExpression` el) adds
     in DARE muls' adds'

-- |DARE for an addition of two DAREs
dareEncodeDareAdd :: FieldElement el
                  => DARE el
                  -> DARE el
                  -> el
                  -> DARE el
dareEncodeDareAdd dl dr r =
    DARE (dlMuls' ++ drMuls') (dlAdds' ++ drAdds')
    where DARE dlMuls' dlAdds' = addToDARE dl (-r)
          DARE drMuls' drAdds' = addToDARE dr r

-- |DARE for a DARE addition getting randoms from generator
dareEncodeDareAddRnd :: (Monad m, CryptoRandomGen g, FieldElement el)
                     => DARE el
                     -> DARE el
                     -> CRandT g GenError m (DARE el)
dareEncodeDareAddRnd dl dr =
    do r <- getRandomElement
       return (dareEncodeDareAdd dl dr r)

-- |DARE encode constatnt
dareEncodePrimaryExpr :: FieldElement el => PrimaryExpression el -> DARE el
dareEncodePrimaryExpr c = DARE [] [genLinearExpr 1 c 0]

-- |DARE decoder
dareDecode :: forall el. FieldElement el => VarMapping el -> DARE el -> Maybe el
dareDecode varMap (DARE muls adds) =
    case sequence (addValsM ++ mulValsM) of
      Nothing -> Nothing
      Just vals -> Just $ foldl' (+) 0 vals
    where addValsM :: [Maybe el]
          addValsM = map (calcLinearExpr varMap) adds
          mulValsM :: [Maybe el]
          mulValsM = map doMul muls
          doMul :: (LinearExpr el, LinearExpr el) -> Maybe el
          doMul (lel, ler) = liftM2 (*) (calcLinearExpr varMap lel)
                                        (calcLinearExpr varMap ler)

type RPGenMonad g el = CRandT g GenError (Writer (RP el))
type RPStmt el = (VariableName, DARE el)
type RP el = DList (RPStmt el)

_CONST_0_ :: FieldElement e => PrimaryExpression e
_CONST_0_ = Constant 0

_NO_DARE_ENCRYPTION_ :: FieldElement el => (el, el)
_NO_DARE_ENCRYPTION_ = (1, 0)

priExFromExpr :: FieldElement el => Expr el -> Maybe (PrimaryExpression el)
priExFromExpr expr =
    case expr of
      Op {} -> Nothing
      Var v -> Just $ Variable v
      Literal l -> Just $ Constant l

exprToRP' :: (CryptoRandomGen g, FieldElement el)
          => Expr el
          -> VariableName
          -> (RPGenMonad g el) (DARE el)
exprToRP' expr outVar =
    case expr of
      Op op exprL exprR ->
          case op of
            Plus ->
                case (priExFromExpr exprL, priExFromExpr exprR) of
                  (Just peL, Just peR) ->
                      dareEncodeAddRnd peL peR
                  _ -> do l <- exprToRP' exprL ('L':outVar)
                          r <- exprToRP' exprR ('R':outVar)
                          dareEncodeDareAddRnd l r
            Minus -> error "DARE compiler: minus not implemented"
            Times ->
                case (priExFromExpr exprL, priExFromExpr exprR) of
                  (Just peL, Just peR) ->
                      dareEncodeMulRnd peL peR _CONST_0_
                  _ -> do l <- exprToRP' exprL ('L':outVar)
                          r <- exprToRP' exprR ('R':outVar)
                          encVarL <- putDare l ('l':outVar)
                          encVarR <- putDare r ('r':outVar)
                          dareEncodeEncMulRnd encVarL
                                              encVarR
                                              (_CONST_0_, _NO_DARE_ENCRYPTION_)
      Var v -> return $ dareEncodePrimaryExpr $ Variable v
      Literal l -> return $ dareEncodePrimaryExpr $ Constant l
      where putDare :: (CryptoRandomGen g, FieldElement el)
                    => DARE el
                    -> VariableName
                    -> (RPGenMonad g el) ( PrimaryExpression el
                                         , PrimaryExpressionEncryptionKey el
                                         )
            putDare dare varName =
               do r1 <- getRandomInvertibleElement
                  r2 <- getRandomElement
                  let dare' = mulElementToDARE (addToDARE dare r2) r1
                  lift $ tell $ DL.singleton (varName, dare')
                  return (Variable varName, (r1, r2))

exprToRP :: (CryptoRandomGen g, FieldElement el)
         => g
         -> Expr el
         -> (Either GenError g, RP el)
exprToRP g expr =
   let (ge, dares) = runWriter (runCRandT (exprToRP' expr "z") g)
       lastDare =
           case ge of
             Left _ -> DL.empty
             Right (ld, _) -> DL.singleton ("z", ld)
       errOrGen =
           case ge of
             Left err -> Left err
             Right (_, gen) -> Right gen
       in (errOrGen, dares `DL.append` lastDare)

type RunRPStateMonad el = State (VarMapping el)

execRPStmt :: FieldElement el => RPStmt el -> (RunRPStateMonad el) (Maybe el)
execRPStmt (outVar, dare) =
    do varMap <- get
       let !valM = dareDecode varMap dare
           varMap' =
              case valM of
               Just val -> M.insert outVar val varMap
               Nothing -> varMap
       put varMap'
       return valM

runRP :: forall el. FieldElement el
      => VarMapping el
      -> RP el
      -> (Maybe el, VarMapping el)
runRP initialVarMap rp =
  let outVarMap :: VarMapping el
      outVarMap = execState (mapM_ execRPStmt (DL.toList rp))
                            initialVarMap
    in (M.lookup "z" outVarMap, outVarMap)
