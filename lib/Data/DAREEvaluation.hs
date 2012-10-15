{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.DAREEvaluation ( ERP(..)
                           , EDARE (..)
                           , OAFEConfiguration
                           , OAFEReference(..)
                           , OAFEEvaluation
                           , OAFEEvaluationRequest
                           , OAFEEvaluationResponse
                           , VariableName, VarMapping
                           , processOAFEEvaluationRequest
                           , prepareRPEvaluation
                           , evaluateERP
                           , runERP
                           , runRP
                           , evalDARE
                           ) where

-- # STANDARD LIBRARY
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Control.Monad (foldM, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict ( State, StateT, get, put
                                  , execStateT, execState
                                  )
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- # SITE PACKAGES
import Control.Failure (Failure, failure)
import Data.DList (DList)
import Data.Vector (Vector)

-- # LOCAL
import Data.DARETypes ( DARE(..), RP, RPStmt
                      , leftVar, rightVar
                      , _SPECIAL_VAR_OUT_
                      , _SPECIAL_VAR_PRE_OUT_
                      , _SPECIAL_VAR_ADDED_PRE_OUT_
                      )
import Data.FieldTypes (Field(..))
import Data.LinearExpression ( LinearExpr(..), VariableName, VarMapping
                             , scalarMul
                             )
import Codec.DARE (dareDecode)

instance Show a => Show (DList a) where
    show dl = (show . DL.toList) dl

type OAFEConfiguration el = HashMap VariableName (Vector (el, el))
type OAFEConfigGen el = HashMap VariableName (Int, DList (el, el))
type OAFEEvaluation el    = HashMap VariableName (Vector el)
type OAFEEvaluationRequest el = (VariableName, el)
type OAFEEvaluationResponse el = (VariableName, (Vector el))

data OAFEReference = OAFERef !VariableName !Int deriving Show

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
_EMPTY_EDARE_ = EDARE [] [] zero

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
    let rpVars :: [VariableName]
        rpVars = concatMap (\v -> [leftVar v, rightVar v]) rpVarNames
        rpDares :: [DARE el]
        rpVarNames :: [VariableName]
        (rpVarNames, rpDares) = unzip $ DL.toList rp
        toEdares :: [DARE el]
                 -> (OAFEConfigGen el)
                 -> Writer (DList (EDARE OAFEReference el)) (OAFEConfigGen el)
        toEdares dares oac =
            case dares of
              [] -> return $! oac
              (dare:dares') ->
                  do let (edareL, edareR, oac') = prepareDAREEvaluation dare oac
                     tell $! DL.singleton edareL
                     tell $! DL.singleton edareR
                     toEdares dares' oac'
        erps :: (OAFEConfigGen el, DList (EDARE OAFEReference el))
        erps = runWriter $ toEdares rpDares HM.empty
        finalEDares :: DList (EDARE OAFEReference el)
        finalEDares = snd erps
        finalOAC :: OAFEConfigGen el
        finalOAC = (fst erps) `HM.union`
                   HM.fromList [ (leftVar _SPECIAL_VAR_OUT_, (0,oneZeroDL))
                               , (rightVar _SPECIAL_VAR_OUT_, (0,oneZeroDL))
                               , (leftVar _SPECIAL_VAR_PRE_OUT_, (0,oneZeroDL))
                               , (rightVar _SPECIAL_VAR_PRE_OUT_, (0,oneZeroDL))
                               ]
     in EvaluatableRP { erpEDares = zip rpVars $ DL.toList finalEDares
                      , erpOAFEConfig = HM.map (V.fromList . DL.toList . snd)
                                               finalOAC
                      }

prepareDAREEvaluation :: forall el. Field el
                      => DARE el
                      -> OAFEConfigGen el
                      -> ( EDARE OAFEReference el
                         , EDARE OAFEReference el
                         , OAFEConfigGen el
                         )
prepareDAREEvaluation (DARE _ dareMuls dareAdds) oafeCfg =
    let oafeReference :: Field el
                      => OAFEConfigGen el
                      -> LinearExpr el
                      -> (OAFEReference, OAFEConfigGen el)
        oafeReference oac le =
            case le of
              LinearExpr !slope !var !intercept ->
                  let (varIdx, les) = HM.lookupDefault (-1, DL.empty) var oac
                      les' :: DList (el, el)
                      !les' = les `DL.snoc` (slope, intercept)
                      varIdx' :: Int
                      !varIdx' = varIdx + 1
                      oac' :: OAFEConfigGen el
                      !oac' = HM.insert var (varIdx', les') oac
                   in (OAFERef var varIdx', oac')
              ConstLinearExpr _ ->
                  error $ "prepareDAREEvaluation: constant expressions "
                          ++ "don't need OAFEs"
        processAdds :: Field el
                    => (EDARE OAFEReference el, OAFEConfigGen el)
                    -> LinearExpr el
                    -> (EDARE OAFEReference el, OAFEConfigGen el)
        processAdds (edare, oac) !le =
            case le of
              LinearExpr _ _ _ ->
                  let (oaref, oac') = oafeReference oac le
                   in (edAddAddTerm edare oaref, oac')
              ConstLinearExpr cle ->
                  (edModifyConst edare (+) cle, oac)
        processMuls :: Field el
                    => (EDARE OAFEReference el, OAFEConfigGen el)
                    -> (LinearExpr el, LinearExpr el)
                    -> (EDARE OAFEReference el, OAFEConfigGen el)
        processMuls (edare, oac) !les =
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
                           | UnknownVariable VariableName String
                           | IndexOutOfBounds VariableName Int String
                           deriving Show

evaluateOAFE :: (Failure DAREEvaluationFailure f, Field el)
             => OAFEConfiguration el
             -> OAFEEvaluation el
             -> (VariableName, el)
             -> f (OAFEEvaluation el)
evaluateOAFE oac curEval (var, val) =
    case HM.lookup var oac of
      Nothing -> return $ curEval
          {-
          failure $
              UnknownFailure ("Variable `"++var++"' not found in OAFE config")
                             "evaluateOAFE"
          -}
      Just xs ->
          return $ HM.insert var (V.map (\(s, i) -> s * val + i) xs) curEval

processOAFEEvaluationRequest :: Field el
                             => OAFEConfiguration el
                             -> OAFEEvaluationRequest el
                             -> OAFEEvaluationResponse el
processOAFEEvaluationRequest oac (var, val) =
    case HM.lookup var oac of
      Nothing -> (var, V.empty)
      Just xs -> (var, V.map (\(s, i) -> s * val + i) xs)

initiallyEvaluateOAFE :: (Failure DAREEvaluationFailure f, Field el)
                      => OAFEConfiguration el
                      -> VarMapping el
                      -> f (OAFEEvaluation el)
initiallyEvaluateOAFE oac vars =
    foldM (evaluateOAFE oac) HM.empty $ M.toList vars

type RunERPStateMonad m el = StateT (OAFEEvaluation el) m

execERPStmt :: (Failure DAREEvaluationFailure f, Field el)
            => OAFEConfiguration el
            -> (VariableName, EDARE OAFEReference el)
            -> (RunERPStateMonad f el) el
execERPStmt oac (outVar, edare) =
    do varMap <- get
       val <- lift $ evaluateDARE edare varMap
       oae <- lift $ evaluateOAFE oac varMap (outVar, val)
       oae' <- lift $ possiblyCalculateAddedPreOutVar oae
       put oae'
       return val
    where
      possiblyCalculateAddedPreOutVar oae =
          let svl = leftVar _SPECIAL_VAR_PRE_OUT_
              svr = rightVar _SPECIAL_VAR_PRE_OUT_
              svalM = if outVar == svl || outVar == svr
                         then case ( liftM V.toList $ HM.lookup svl oae
                                   , liftM V.toList $ HM.lookup svr oae
                                   ) of
                                (Just [valL], Just [valR]) ->
                                    return $! valL + valR
                                _ -> Nothing
                         else Nothing
           in case svalM of
                Just sval ->
                    evaluateOAFE oac oae (_SPECIAL_VAR_ADDED_PRE_OUT_, sval)
                Nothing -> return oae

runERP :: forall el. (Field el)
       => ERP el
       -> VarMapping el
       -> Either String el
runERP (EvaluatableRP edares oac) vars =
    case doIt of
      Left err -> Left $ show (err :: DAREEvaluationFailure)
      Right val -> Right $ V.head val
    where doIt =
              do let oac' = oac `HM.union`
                            HM.fromList
                                [ (leftVar _SPECIAL_VAR_OUT_, oneZeroV)
                                , (rightVar _SPECIAL_VAR_OUT_, oneZeroV)
                                , (leftVar _SPECIAL_VAR_PRE_OUT_, oneZeroV)
                                , (rightVar _SPECIAL_VAR_PRE_OUT_, oneZeroV)
                                ]
                 oae <- initiallyEvaluateOAFE oac' vars
                 out <- execStateT (mapM_ (execERPStmt oac') edares) oae
                 case HM.lookup (leftVar _SPECIAL_VAR_OUT_) out of
                   Just val -> return val
                   Nothing ->
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
              do case HM.lookup var oafeVals of
                   Nothing -> failure $ UnknownVariable var "evaluateDARE'"
                   Just vals ->
                       if V.length vals <= idx
                          then failure $
                                   IndexOutOfBounds var idx "evaluateDARE'"
                          else return $ vals V.! idx

evalDARE :: Field el
         => EDARE OAFEReference el
         -> OAFEEvaluation el
         -> Either DAREEvaluationFailure el
evalDARE = evaluateDARE

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
          rMuls = foldl' (+) zero muls'
          rAdds :: el
          rAdds = foldl' (+) zero adds
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
           varMap''' =
              case valsM of
               Just (valL, valR) ->
                  let varMap' = M.insert (leftVar outVar) valL varMap
                      varMap'' = if outVar == _SPECIAL_VAR_PRE_OUT_
                                    then M.insert _SPECIAL_VAR_ADDED_PRE_OUT_
                                                  (valL + valR)
                                                  varMap'
                                    else varMap'
                   in M.insert (rightVar outVar) valR varMap''
               Nothing -> varMap
       put varMap'''
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
          case ( M.lookup (leftVar _SPECIAL_VAR_OUT_) outVarMap
               , M.lookup (rightVar _SPECIAL_VAR_OUT_) outVarMap
               ) of
            (Just l, Just r) ->
               if l == r then Just l else error "the impossible happened"
            _ -> Nothing
   in (out, outVarMap)

oneZeroDL :: Field el => DList (el, el)
oneZeroDL = DL.singleton (one, zero)

oneZeroV :: Field el => Vector (el, el)
oneZeroV = V.singleton (one, zero)
