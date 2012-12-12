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
import Data.RAE.Evaluation ( _SPECIAL_VAR_OUT_LEFT_, _SPECIAL_VAR_OUT_RIGHT_
                           , _SPECIAL_VAR_PRE_OUT_LEFT_
                           , _SPECIAL_VAR_PRE_OUT_RIGHT_
                           )
import Data.RAE.Types ( DRAE(..), DRAC, RAE(..), RAC
                      , DualVarName(..)
                      , MulTermRadicals(..)
                      , Radicals(..)
                      , LinearRadicals(..)
                      , DualLinearRadicals(..)
                      )

instance Show a => Show (DList a) where
    show = show . DL.toList

type OAFEConfigGen el = HashMap VariableName (Int, DList (el, el))

_EMPTY_RAE_ :: Field el => RAE Radicals OAFEReference el
_EMPTY_RAE_ = RAE [] [] zero

raeModifyConst :: Field el
               => RAE Radicals OAFEReference el
               -> (el -> el -> el)
               -> el
               -> RAE Radicals OAFEReference el
raeModifyConst rae op el =
    rae { raeConst = el `seq` raeConst rae `op` el }

raeAddAddTerm :: Field el
              => RAE Radicals OAFEReference el
              -> OAFEReference
              -> RAE Radicals OAFEReference el
raeAddAddTerm rae oaref =
    rae { raeAddTerms = oaref : raeAddTerms rae }

raeAddMulTerm :: Field el
              => RAE Radicals OAFEReference el
              -> (Radicals OAFEReference, Radicals OAFEReference)
              -> RAE Radicals OAFEReference el
raeAddMulTerm rae oarefs =
    rae { raeMulTerms = oarefs : raeMulTerms rae }

-- | Transform a @DRAC@ to a @RAC@.
singularizeDRAC :: forall el. Field el
                => DRAC el
                -> (RAC el, OAFEConfiguration el)
singularizeDRAC drac =
    let dracVars :: [VariableName]
        dracVars = concatMap (\v -> [dvnLeftVarName v, dvnRightVarName v])
                             varNames
        origDRAEs :: [DRAE el]
        varNames :: [DualVarName]
        (varNames, origDRAEs) = unzip $ DL.toList drac
        toRAEs :: [DRAE el]
               -> OAFEConfigGen el
               -> Writer (DList (RAE Radicals OAFEReference el))
                         (OAFEConfigGen el)
        toRAEs draes !oac =
            case draes of
              [] -> return $! oac
              (drae:draes') ->
                  do let (raeL, raeR, !oac') = singularizeDRAE drae oac
                     tell $! DL.singleton raeL
                     tell $! DL.singleton raeR
                     toRAEs draes' oac'
        raes :: (OAFEConfigGen el, DList (RAE Radicals OAFEReference el))
        raes = runWriter $ toRAEs origDRAEs HM.empty
        finalRAEs :: DList (RAE Radicals OAFEReference el)
        finalRAEs = snd raes
        finalOAC :: OAFEConfigGen el
        finalOAC = fst raes `HM.union`
                   HM.fromList [ (_SPECIAL_VAR_OUT_LEFT_, (0,oneZeroDL))
                               , (_SPECIAL_VAR_OUT_RIGHT_, (0,oneZeroDL))
                               , (_SPECIAL_VAR_PRE_OUT_LEFT_, (0,oneZeroDL))
                               , (_SPECIAL_VAR_PRE_OUT_RIGHT_, (0,oneZeroDL))
                               ]
     in ( zip dracVars $ DL.toList finalRAEs
        , HM.map (V.fromList . DL.toList . snd) finalOAC
        )

singularizeDRAE :: forall el. Field el
                => DRAE el
                -> OAFEConfigGen el
                -> ( RAE Radicals OAFEReference el
                   , RAE Radicals OAFEReference el
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
                      !oar = OAFERef var varIdx'
                   in (oar, oac')
              ConstLinearExpr _ ->
                  error $ "singularizeDRAE: constant expressions "
                          ++ "don't need OAFEs"
        processAdds :: Field el
                    => (RAE Radicals OAFEReference el, OAFEConfigGen el)
                    -> LinearExpr el
                    -> (RAE Radicals OAFEReference el, OAFEConfigGen el)
        processAdds (!rae, !oac) !le =
            case le of
              LinearExpr {} ->
                  let (oaref, !oac') = oafeReference oac le
                   in (raeAddAddTerm rae oaref, oac')
              ConstLinearExpr cle ->
                  (raeModifyConst rae (+) cle, oac)
        processMuls :: Field el
                    => (RAE Radicals OAFEReference el, OAFEConfigGen el)
                    -> DualLinearRadicals el
                    -> (RAE Radicals OAFEReference el, OAFEConfigGen el)
        processMuls (!rae, !oac) !dlr =
            let -- | Turn a multiplication involving a scalar to an addition
                -- because that's an ordinary linear expression.
                processScalarMul :: Field el
                                 => LinearExpr el
                                 -> LinearExpr el
                                 -> el
                                 -> el
                                 -> ( RAE Radicals OAFEReference el
                                    , OAFEConfigGen el
                                    )
                processScalarMul le1 le2 el1 el2 =
                    let !elsAdd = el1 + el2
                        le1' = scalarMul le1 elsAdd
                        le2' = scalarMul le2 elsAdd
                        (!oaref1, !oac') = oafeReference oac le1'
                        (!oaref2, !oac'') = oafeReference oac' le2'
                        pre = raeAddAddTerm rae oaref1
                     in (raeAddAddTerm pre oaref2, oac'')
                (DLR (dlrl, dlrr)) = dlr
                (LinearRadicals (dlrl1, dlrl2)) = dlrl
                (LinearRadicals (dlrr1, dlrr2)) = dlrr
                les = (dlrl1, dlrl2, dlrr1, dlrr2)
             in case les of
                  (   ConstLinearExpr clel1, ConstLinearExpr clel2
                    , ConstLinearExpr cler1, ConstLinearExpr cler2) ->
                      processAdds (rae, oac)
                                  (ConstLinearExpr ((clel1 + clel2) *
                                                    (cler1 + cler2)))
                  (   ConstLinearExpr clel1 , ConstLinearExpr clel2
                    , ler1@(LinearExpr {}) , ler2@(LinearExpr {})) ->
                      processScalarMul ler1 ler2 clel1 clel2
                  (   lel1@(LinearExpr {}), lel2@(LinearExpr{})
                    , ConstLinearExpr cler1, ConstLinearExpr cler2) ->
                      processScalarMul lel1 lel2 cler1 cler2
                  (   lel1@(LinearExpr {}), lel2@(LinearExpr {})
                    , ler1@(LinearExpr {}), ler2@(LinearExpr {})) ->
                      let (oarefl1, oac')  = oafeReference oac  lel1
                          (oarefl2, oac'')  = oafeReference oac'  lel2
                          (oarefr1, oac''') = oafeReference oac'' ler1
                          (oarefr2, oac'''') = oafeReference oac''' ler2
                       in (raeAddMulTerm rae ( ORR (oarefl1, oarefl2)
                                             , ORR (oarefr1, oarefr2))
                          , oac''''
                          )
                  _ -> error "singularizeDRAC: different cons at LE radicals"
        draeMulsList = DL.toList draeMuls
        draeAddsList = DL.toList draeAdds
        raeAfterMulsL :: RAE Radicals OAFEReference el
        (!raeAfterMulsL, !oafeCfg') =
            foldl' processMuls
                   (_EMPTY_RAE_, oafeCfg)
                   (map mtrLeft draeMulsList)
        raeAfterAddsL :: RAE Radicals OAFEReference el
        (!raeAfterAddsL, !oafeCfg'') =
            foldl' processAdds
                   (raeAfterMulsL, oafeCfg')
                   (map fst draeAddsList)
        raeAfterMulsR :: RAE Radicals OAFEReference el
        (!raeAfterMulsR, !oafeCfg''') =
            foldl' processMuls
                   (_EMPTY_RAE_, oafeCfg'')
                   (map mtrRight draeMulsList)
        raeAfterAddsR :: RAE Radicals OAFEReference el
        (!raeAfterAddsR, !oafeCfg'''') =
            foldl' processAdds
                   (raeAfterMulsR, oafeCfg''')
                   (map snd draeAddsList)
     in (raeAfterAddsL, raeAfterAddsR, oafeCfg'''')

oneZeroDL :: Field el => DList (el, el)
oneZeroDL = DL.singleton (one, zero)
