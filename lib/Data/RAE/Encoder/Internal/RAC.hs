{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Data.RAE.Encoder.Internal.RAC ( singularizeDRAC ) where

-- # STANDARD LIBRARY
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Control.Monad.Writer.Lazy (Writer, tell, runWriter)
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- # SITE PACKAGES
import Data.DList (DList)

-- # LOCAL
import Data.FieldTypes (Field(..))
import Data.LinearExpression (LinearExpr(..), VariableName, scalarMul)
import Data.OAFE (OAFEConfiguration, OAFEReference(..))
import Data.RAE.Evaluation ( _SPECIAL_VAR_OUT_
                           , _SPECIAL_VAR_PRE_OUT_
                           )
import Data.RAE.Types (DRAE(..), DRAC, RAE(..), RAC, leftVar, rightVar)

instance Show a => Show (DList a) where
    show = show . DL.toList

type OAFEConfigGen el = HashMap VariableName (Int, DList (el, el))

_EMPTY_RAE_ :: Field el => RAE OAFEReference el
_EMPTY_RAE_ = RAE [] [] zero

raeModifyConst :: Field el
               => RAE OAFEReference el
               -> (el -> el -> el)
               -> el
               -> RAE OAFEReference el
raeModifyConst rae op el =
    rae { raeConst = el `seq` raeConst rae `op` el }

raeAddAddTerm :: Field el
              => RAE OAFEReference el
              -> OAFEReference
              -> RAE OAFEReference el
raeAddAddTerm rae oaref =
    rae { raeAddTerms = oaref : raeAddTerms rae }

raeAddMulTerm :: Field el
              => RAE OAFEReference el
              -> (OAFEReference, OAFEReference)
              -> RAE OAFEReference el
raeAddMulTerm rae oarefs =
    rae { raeMulTerms = oarefs : raeMulTerms rae }

-- | Transform a @DRAC@ to a @RAC@.
singularizeDRAC :: forall el. Field el
                => DRAC el
                -> (RAC el, OAFEConfiguration el)
singularizeDRAC drac =
    let dracVars :: [VariableName]
        dracVars = concatMap (\v -> [leftVar v, rightVar v]) varNames
        origDRAEs :: [DRAE el]
        varNames :: [VariableName]
        (varNames, origDRAEs) = unzip $ DL.toList drac
        toRAEs :: [DRAE el]
               -> OAFEConfigGen el
               -> Writer (DList (RAE OAFEReference el)) (OAFEConfigGen el)
        toRAEs draes !oac =
            case draes of
              [] -> return $! oac
              (drae:draes') ->
                  do let (raeL, raeR, !oac')=singularizeDRAE drae oac
                     tell $! DL.singleton raeL
                     tell $! DL.singleton raeR
                     toRAEs draes' oac'
        raes :: (OAFEConfigGen el, DList (RAE OAFEReference el))
        raes = runWriter $ toRAEs origDRAEs HM.empty
        finalRAEs :: DList (RAE OAFEReference el)
        finalRAEs = snd raes
        finalOAC :: OAFEConfigGen el
        finalOAC = fst raes `HM.union`
                   HM.fromList [ (leftVar _SPECIAL_VAR_OUT_, (0,oneZeroDL))
                               , (rightVar _SPECIAL_VAR_OUT_, (0,oneZeroDL))
                               , (leftVar _SPECIAL_VAR_PRE_OUT_, (0,oneZeroDL))
                               , (rightVar _SPECIAL_VAR_PRE_OUT_, (0,oneZeroDL))
                               ]
     in ( zip dracVars $ DL.toList finalRAEs
        , HM.map (V.fromList . DL.toList . snd) finalOAC
        )

singularizeDRAE :: forall el. Field el
                => DRAE el
                -> OAFEConfigGen el
                -> ( RAE OAFEReference el
                   , RAE OAFEReference el
                   , OAFEConfigGen el
                   )
singularizeDRAE (DRAE _ draeMuls draeAdds) !oafeCfg =
    let oafeReference :: Field el
                      => OAFEConfigGen el
                      -> LinearExpr el
                      -> (OAFEReference, OAFEConfigGen el)
        oafeReference !oac le =
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
                  error $ "singularizeDRAE: constant expressions "
                          ++ "don't need OAFEs"
        processAdds :: Field el
                    => (RAE OAFEReference el, OAFEConfigGen el)
                    -> LinearExpr el
                    -> (RAE OAFEReference el, OAFEConfigGen el)
        processAdds (rae, oac) !le =
            case le of
              LinearExpr {} ->
                  let (oaref, !oac') = oafeReference oac le
                   in (raeAddAddTerm rae oaref, oac')
              ConstLinearExpr cle ->
                  (raeModifyConst rae (+) cle, oac)
        processMuls :: Field el
                    => (RAE OAFEReference el, OAFEConfigGen el)
                    -> (LinearExpr el, LinearExpr el)
                    -> (RAE OAFEReference el, OAFEConfigGen el)
        processMuls (rae, oac) !les =
            let -- | Turn a multiplication involving a scalar to an addition
                -- because that's an ordinary linear expression.
                processScalarMul :: Field el
                                 => LinearExpr el
                                 -> el
                                 -> (RAE OAFEReference el, OAFEConfigGen el)
                processScalarMul le el =
                    let le' = scalarMul le el
                        (oaref, oac') = oafeReference oac le'
                     in (raeAddAddTerm rae oaref, oac')
             in case les of
                  (ConstLinearExpr clel, ConstLinearExpr cler) ->
                      processAdds (rae, oac) (ConstLinearExpr (clel * cler))
                  (ConstLinearExpr clel, ler@(LinearExpr {})) ->
                      processScalarMul ler clel
                  (lel@(LinearExpr {}), ConstLinearExpr cler) ->
                      processScalarMul lel cler
                  (lel, ler) ->
                      let (oarefl, oac')  = oafeReference oac  lel
                          (oarefr, oac'') = oafeReference oac' ler
                       in (raeAddMulTerm rae (oarefl, oarefr), oac'')
        draeMulsList = DL.toList draeMuls
        draeAddsList = DL.toList draeAdds
        fstMul :: ((a, b), (c, d)) -> (a, c)
        fstMul (l, r) = (fst l, fst r)
        sndMul :: ((a, b), (c, d)) -> (b, d)
        sndMul (l, r) = (snd l, snd r)
        raeAfterMulsL :: RAE OAFEReference el
        (raeAfterMulsL, !oafeCfg') =
            foldl' processMuls
                   (_EMPTY_RAE_, oafeCfg)
                   (map fstMul draeMulsList)
        raeAfterAddsL :: RAE OAFEReference el
        (raeAfterAddsL, !oafeCfg'') =
            foldl' processAdds
                   (raeAfterMulsL, oafeCfg')
                   (map fst draeAddsList)
        raeAfterMulsR :: RAE OAFEReference el
        (raeAfterMulsR, !oafeCfg''') =
            foldl' processMuls
                   (_EMPTY_RAE_, oafeCfg'')
                   (map sndMul draeMulsList)
        raeAfterAddsR :: RAE OAFEReference el
        (raeAfterAddsR, !oafeCfg'''') =
            foldl' processAdds
                   (raeAfterMulsR, oafeCfg''')
                   (map snd draeAddsList)
     in (raeAfterAddsL, raeAfterAddsR, oafeCfg'''')

oneZeroDL :: Field el => DList (el, el)
oneZeroDL = DL.singleton (one, zero)
