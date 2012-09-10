{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.DAREEvaluation ( ERP
                           , EDARE (..)
                           , OAFEReference(..)
                           , prepareRPEvaluation
                           , evaluateERP
                           , runERP
                           , runRP
                           ) where

-- # STANDARD LIBRARY
import Data.Map (Map)
import Data.List (foldl')
import Control.Monad (foldM)
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict ( State, StateT, get, put
                                  , execStateT, execState
                                  )
import qualified Data.DList as DL
import qualified Data.Map as M

-- # SITE PACKAGES
import Control.Failure (Failure, failure)

-- # LOCAL
import Data.DARETypes ( DARE(..), RP, RPStmt
                      , leftVar, rightVar
                      )
import Data.FieldTypes (Field(..))
import Data.LinearExpression ( LinearExpr(..), VariableName, VarMapping
                             , scalarMul
                             )
import Codec.DARE (dareDecode)

type OAFEConfiguration el = Map VariableName [(el, el)]
type OAFEEvaluation el    = Map VariableName [el]

data OAFEReference = OAFERef VariableName Int deriving Show

data EDARE val el =
    EDARE { edMulTerms   :: [(val, val)]
          , edAddTerms   :: [val]
          , edConst      :: el
          }

instance (Show val, Show el) => Show (EDARE val el) where
    show = prettyPrintEDARE

prettyPrintEDARE :: (Show val, Show el) => EDARE val el -> String
prettyPrintEDARE (EDARE muls adds c) =
    let showSubList :: Show a => String -> [a] -> String
        showSubList b as =
            case as of
              [] -> ""
              (a:as') -> "\t" ++ b ++ show a ++ "\n" ++ showSubList b as'
     in "EDARE\n" ++
            showSubList "M: " muls ++
            showSubList "A: " adds ++
            show c ++ "\n"

data ERP el =
    EvaluatableRP { erpEDares     :: [(VariableName, EDARE OAFEReference el)]
                  , erpOAFEConfig :: OAFEConfiguration el
                  } deriving Show

_EMPTY_EDARE_ :: Field el => EDARE OAFEReference el
_EMPTY_EDARE_ = EDARE [] [] 0

edModifyConst :: Field el
              => EDARE OAFEReference el
              -> (el -> el -> el)
              -> el
              -> EDARE OAFEReference el
edModifyConst edare op el =
    edare { edConst = el `seq` (edConst edare) `op` el }

edAddAddTerm :: Field el
             => EDARE OAFEReference el
             -> OAFEReference
             -> EDARE OAFEReference el
edAddAddTerm edare oaref =
    edare { edAddTerms = oaref : edAddTerms edare }

edAddMulTerm :: Field el
             => EDARE OAFEReference el
             -> (OAFEReference, OAFEReference)
             -> EDARE OAFEReference el
edAddMulTerm edare oarefs =
    edare { edMulTerms = oarefs : edMulTerms edare }

prepareRPEvaluation :: forall el. Field el
                    => RP el
                    -> ERP el
prepareRPEvaluation rp =
    let rpList :: [RPStmt el]
        rpList = DL.toList rp
        rpVars :: [VariableName]
        rpVars = concat $ map (\v -> [leftVar v, rightVar v]) $ map  fst rpList
        rpDares :: [DARE el]
        rpDares = map snd rpList
        toEdares :: [DARE el]
                 -> [EDARE OAFEReference el]
                 -> (OAFEConfiguration el)
                 -> ([EDARE OAFEReference el], OAFEConfiguration el)
        toEdares dares edares oac =
            case dares of
              [] -> (edares, oac)
              (dare:dares') ->
                  let (edareL, edareR, oac') = prepareDAREEvaluation dare oac
                   in toEdares dares' (edares ++ [edareL, edareR]) oac'
        erps :: ([EDARE OAFEReference el], OAFEConfiguration el)
        erps = toEdares rpDares [] M.empty
        finalEDares :: [EDARE OAFEReference el]
        finalEDares = fst erps
        finalOAC :: OAFEConfiguration el
        finalOAC = snd erps
     in EvaluatableRP { erpEDares = zip rpVars finalEDares
                      , erpOAFEConfig = finalOAC
                      }

prepareDAREEvaluation :: forall el. Field el
                      => DARE el
                      -> OAFEConfiguration el
                      -> ( EDARE OAFEReference el
                         , EDARE OAFEReference el
                         , OAFEConfiguration el
                         )
prepareDAREEvaluation (DARE _ dareMuls dareAdds) oafeCfg =
    let oafeReference :: Field el
                      => OAFEConfiguration el
                      -> LinearExpr el
                      -> (OAFEReference, OAFEConfiguration el)
        oafeReference oac le =
            case le of
              LinearExpr !slope !var !intercept ->
                  let oac' :: OAFEConfiguration el
                      oac' = M.insertWith (flip (++))
                                          var
                                          [(slope, intercept)]
                                          oac
                   in ( OAFERef var
                                ((length $ M.findWithDefault [] var oac')-1)
                      , oac'
                      )
              ConstLinearExpr _ ->
                  error $ "prepareDAREEvaluation: constant expressions "
                          ++ "don't need OAFEs"
        processAdds :: Field el
                    => (EDARE OAFEReference el, OAFEConfiguration el)
                    -> LinearExpr el
                    -> (EDARE OAFEReference el, OAFEConfiguration el)
        processAdds (edare, oac) le =
            case le of
              LinearExpr _ _ _ ->
                  let (oaref, oac') = oafeReference oac le
                   in (edAddAddTerm edare oaref, oac')
              ConstLinearExpr cle ->
                  (edModifyConst edare (+) cle, oac)
        processMuls :: Field el
                    => (EDARE OAFEReference el, OAFEConfiguration el)
                    -> (LinearExpr el, LinearExpr el)
                    -> (EDARE OAFEReference el, OAFEConfiguration el)
        processMuls (edare, oac) les =
            case les of
              (ConstLinearExpr clel, ConstLinearExpr cler) ->
                  processAdds (edare, oac) (ConstLinearExpr (clel * cler))
              (ConstLinearExpr clel, ler@(LinearExpr _ _ _)) ->
                  let ler' = scalarMul ler clel
                      (oaref, oac') = oafeReference oac ler'
                   in (edAddAddTerm edare oaref, oac')
              (lel@(LinearExpr _ _ _), ConstLinearExpr cler) ->
                  let lel' = scalarMul lel cler
                      (oaref, oac') = oafeReference oac lel'
                   in (edAddAddTerm edare oaref, oac')
              (lel, ler) ->
                  let (oarefl, oac')  = oafeReference oac  lel
                      (oarefr, oac'') = oafeReference oac' ler
                   in (edAddMulTerm edare (oarefl, oarefr), oac'')
        dareMulsList = DL.toList dareMuls
        dareAddsList = DL.toList dareAdds
        fstMul :: ((a, b), (c, d)) -> (a, c)
        fstMul (l, r) = (fst l, fst r)
        sndMul :: ((a, b), (c, d)) -> (b, d)
        sndMul (l, r) = (snd l, snd r)
        edareAfterMulsL :: EDARE OAFEReference el
        (edareAfterMulsL, oafeCfg') =
            foldl' processMuls
                   (_EMPTY_EDARE_, oafeCfg)
                   (map fstMul dareMulsList)
        edareAfterAddsL :: EDARE OAFEReference el
        (edareAfterAddsL, oafeCfg'') =
            foldl' processAdds
                   (edareAfterMulsL, oafeCfg')
                   (map fst dareAddsList)
        edareAfterMulsR :: EDARE OAFEReference el
        (edareAfterMulsR, oafeCfg''') =
            foldl' processMuls
                   (_EMPTY_EDARE_, oafeCfg'')
                   (map sndMul dareMulsList)
        edareAfterAddsR :: EDARE OAFEReference el
        (edareAfterAddsR, oafeCfg'''') =
            foldl' processAdds
                   (edareAfterMulsR, oafeCfg''')
                   (map snd dareAddsList)
     in (edareAfterAddsL, edareAfterAddsR, oafeCfg'''')

data DAREEvaluationFailure = UnknownFailure String String
                           | UnknownVariable String String
                           | IndexOutOfBounds String Int String
                           | NonWellFormedResult String
                           deriving Show

evaluateOAFE :: (Failure DAREEvaluationFailure f, Field el)
             => OAFEConfiguration el
             -> OAFEEvaluation el
             -> (String, el)
             -> f (OAFEEvaluation el)
evaluateOAFE oac curEval (var, val) =
    case M.lookup var oac of
      Nothing -> return $ curEval
          {-
          failure $
              UnknownFailure ("Variable `"++var++"' not found in OAFE config")
                             "evaluateOAFE"
          -}
      Just xs ->
          return $ M.insert var (map (\(s, i) -> s * val + i) xs) curEval

initiallyEvaluateOAFE :: (Failure DAREEvaluationFailure f, Field el)
                      => OAFEConfiguration el
                      -> VarMapping el
                      -> f (OAFEEvaluation el)
initiallyEvaluateOAFE oac vars =
    foldM (evaluateOAFE oac) M.empty $ M.toList vars

type RunERPStateMonad m el = StateT (OAFEEvaluation el) m

execERPStmt :: (Failure DAREEvaluationFailure f, Field el)
            => OAFEConfiguration el
            -> (VariableName, EDARE OAFEReference el)
            -> (RunERPStateMonad f el) el
execERPStmt oac (outVar, edare) =
    do varMap <- get
       val <- lift $ evaluateDARE edare varMap
       oae <- lift $ evaluateOAFE oac varMap (outVar, val)
       put oae
       return val

runERP :: forall el. (Field el)
       => ERP el
       -> VarMapping el
       -> Either String el
runERP (EvaluatableRP edares oac) vars =
    case doIt of
      Left err -> Left $ show (err :: DAREEvaluationFailure)
      Right val -> Right $ head val
    where doIt =
              do let oac' = M.insert "enc_OUT~_1" [(1,0)] oac
                     oac'' = M.insert "enc_OUT~_2" [(1,0)] oac'
                 oae <- initiallyEvaluateOAFE oac'' vars
                 out <- execStateT (mapM_ (execERPStmt oac'') edares) oae
                 case (M.lookup "enc_OUT~_1" out, M.lookup "enc_OUT~_2" out) of
                   (Just valL, Just valR) ->
                      if valL == valR
                         then return valL
                         else failure $ NonWellFormedResult "runERP"
                   (Just _, Nothing) ->
                       failure$UnknownFailure "output R not calculated" "runERP"
                   (Nothing, Just _) ->
                       failure$UnknownFailure "output L not calculated" "runERP"
                   (Nothing, Nothing) ->
                       failure $ UnknownFailure "output not calculated" "runERP"

evaluateDARE' :: forall f. forall el.
                 (Failure DAREEvaluationFailure f, Field el)
              => EDARE OAFEReference el
              -> OAFEEvaluation el
              -> f (EDARE el el)
evaluateDARE' (EDARE muls adds c) oafeVals =
    do adds' <- mapM resolve adds
       muls' <- sequence $ map resolveTuple muls
       return $ EDARE muls' adds' c
    where resolveTuple :: (OAFEReference, OAFEReference)
                       -> f (el, el)
          resolveTuple (oarefl, oarefr) =
              do l <- resolve oarefl
                 r <- resolve oarefr
                 return (l, r)
          resolve :: OAFEReference
                  -> f el
          resolve (OAFERef var idx) =
              do case M.lookup var oafeVals of
                   Nothing -> failure $ UnknownVariable var "evaluateDARE'"
                   Just vals ->
                       if length vals <= idx
                          then failure $
                                   IndexOutOfBounds var idx "evaluateDARE'"
                          else return $ vals!!idx

evaluateDARE :: forall f. forall el.
                (Failure DAREEvaluationFailure f, Field el)
             => EDARE OAFEReference el
             -> OAFEEvaluation el
             -> f el
evaluateDARE edare vals =
   do edare' <- evaluateDARE' edare vals
      let (EDARE muls adds c) = edare'
          muls' :: [el]
          muls' = map (uncurry (*)) muls
          rMuls :: el
          rMuls = foldl' (+) 0 muls'
          rAdds :: el
          rAdds = foldl' (+) 0 adds
      return $ rMuls + rAdds + c

evaluateERP :: Field el
            => ERP el
            -> OAFEEvaluation el
            -> Either String el
evaluateERP _ vals =
   case evaluateDARE undefined vals of
     Left err -> Left $ show (err :: DAREEvaluationFailure)
     Right e -> Right $ e

type RunRPStateMonad el = State (VarMapping el)

execRPStmt :: (Field el)
           => RPStmt el -> (RunRPStateMonad el) (Maybe (el, el))
execRPStmt (outVar, dare) =
    do varMap <- get
       let !valsM = dareDecode varMap dare
           varMap'' =
              case valsM of
               Just (valL, valR) ->
                  let varMap' = M.insert (leftVar outVar) valL varMap
                   in M.insert (rightVar outVar) valR varMap'
               Nothing -> varMap
       put varMap''
       return valsM

runRP :: forall el. (Field el)
      => VarMapping el
      -> RP el
      -> (Maybe el, VarMapping el)
runRP initialVarMap rp =
  let outVarMap :: VarMapping el
      outVarMap = execState (mapM_ execRPStmt (DL.toList rp))
                            initialVarMap
      out =
          case ( M.lookup (leftVar "OUT") outVarMap
               , M.lookup (rightVar "OUT") outVarMap
               ) of
            (Just l, Just r) ->
               if l == r then Just l else error "the impossible happened"
            _ -> Nothing
   in (out, outVarMap)
