{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Types for /RAE/s (/R/andomized /A/ffine /E/ncodings).
module Data.RAE.Types
    ( -- * Basic Data Types
      RAE(..), DRAE(..), RAC, RACFragment, DRAC, DRACFragment
    , DualLinearExpr
      -- * Encryption Key Data Types
    , Key, DualKey, DualKeyPair
      -- * Other Types
    , DualVarName(..)
      -- * Helper Functions
    , genDualVarName
      -- * Convenience Re-exports
    , VariableName
    , VarMapping
    , PrimaryExpression(..)
      -- * Only Exported for Tests
    , leftVar, rightVar
    ) where

import Data.DList (DList)
import Data.String (IsString(..))
import qualified Data.DList as DL
import qualified Data.Text as T

import Data.FieldTypes (Field(..))
import Data.LinearExpression (LinearExpr(..), VarMapping, VariableName)
import Data.OAFE (OAFEReference)

data PrimaryExpression el = Variable VariableName
                          | Constant el

-- | An encryption key.
type Key el = el

-- | A dual encryption key (e.g. the dynamic keys or the static keys)
type DualKey el = (Key el, Key el)

-- | A dual linear expression.
type DualLinearExpr el = (LinearExpr el, LinearExpr el)

-- | A dual encryption key pair made of two dual keys. Usually the static dual
-- key and the dynamic dual key.
type DualKeyPair el = (DualKey el, DualKey el)

-- | /D/ual /R/andomized /A/ffine /E/ncoding.
data DRAE el = DRAE !(DualKeyPair el)
                    !(DList (DualLinearExpr el, DualLinearExpr el))
                    !(DList (DualLinearExpr el))

-- | /D/ual /R/andomized /A/ffine /C/ircuit fragment.
type DRACFragment el = (DualVarName, DRAE el)

-- | /D/ual /R/andomized /A/ffine /C/ircuit.
type DRAC el = DList (DRACFragment el)

instance (Field el, Show el) => Show (DRAE el) where
    show = prettyPrintDRAE

genDualVarName :: VariableName -> DualVarName
genDualVarName vn = DualVarName vn (leftVar vn) (rightVar vn)

data DualVarName = DualVarName { dvnVarName :: !VariableName
                               , dvnLeftVarName :: !VariableName
                               , dvnRightVarName :: !VariableName
                               } deriving (Show, Eq)

instance IsString DualVarName where
    fromString = genDualVarName . fromString

leftVar :: VariableName -> VariableName
leftVar !v = "__e1_" `T.append` v

rightVar :: VariableName -> VariableName
rightVar !v = "__e2_" `T.append` v

prettyPrintDRAE :: (Field el, Show el) => DRAE el -> String
prettyPrintDRAE (DRAE keys muls adds) =
    let showSubList :: Show a => String -> [a] -> String
        showSubList b as =
            case as of
              [] -> ""
              (a:as') -> "\t" ++ b ++ show a ++ "\n" ++ showSubList b as'
     in "DRAE " ++ show keys ++ "\n" ++
            showSubList "M: " (DL.toList muls) ++
            showSubList "A: " (DL.toList adds)

-- | /R/andomized /A/ffine /E/ncoding.
data RAE val el =
    RAE { raeMulTerms   :: [(val, val)]
        , raeAddTerms   :: [val]
        , raeConst      :: el
        }

instance (Show val, Show el) => Show (RAE val el) where
    show = prettyPrintRAE

prettyPrintRAE :: (Show val, Show el) => RAE val el -> String
prettyPrintRAE (RAE muls adds c) =
    let showSubList :: Show a => String -> [a] -> String
        showSubList b as =
            case as of
              [] -> ""
              (a:as') -> "\t" ++ b ++ show a ++ "\n" ++ showSubList b as'
     in "RAE\n" ++
            showSubList "M: " muls ++
            showSubList "A: " adds ++
            showSubList "C: " [c]  ++ "\n"

-- | /R/andomized /A/ffine /C/ircuit fragment.
type RACFragment el = (VariableName, RAE OAFEReference el)

-- | /R/andomized /A/ffine /C/ircuit.
type RAC el = [RACFragment el]
