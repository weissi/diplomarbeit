{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Types for /RAE/s (/R/andomized /A/ffine /E/ncodings).
module Data.RAE.Types
    ( -- * Basic Data Types
      RAE(..), DRAE(..), RAC, RACFragment, DRAC, DRACFragment
    , LinearRadicals(..), MulTermRadicals(..)
    , Radicals(..), Completes(..), DualLinearRadicals(..)
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

data family Radicals a :: *

newtype instance Radicals (LinearExpr el) =
    LER (LinearExpr el, LinearExpr el) deriving Show
newtype instance Radicals OAFEReference =
    ORR (OAFEReference, OAFEReference) deriving Show

data family Completes a

data instance Completes a = C a

newtype LinearRadicals el =
    LinearRadicals { unLR :: (LinearExpr el, LinearExpr el) }
instance (Show el, Field el) => Show (LinearRadicals el) where
    show (LinearRadicals (le1, le2)) =
        "[" ++ show le1 ++ " (+) " ++ show le2 ++ "]"

--newtype OAFERadicalsRef =
--    OAFERadicalsRef { unORR :: (OAFEReference, OAFEReference) } deriving Show

-- | A dual linear expression.
type DualLinearExpr el = (LinearExpr el, LinearExpr el)
newtype DualLinearRadicals el =
    DLR { unDLR :: (LinearRadicals el, LinearRadicals el) }
instance (Show el, Field el) => Show (DualLinearRadicals el) where
    show (DLR (lrl, lrr)) =
        "{DLR L: " ++ show lrl ++ "; DLR R: " ++ show lrr ++ "}"

data MulTermRadicals el =
    MulTermRadicals { mtrLeft  :: DualLinearRadicals el  -- ^ for left @RAC@
                    , mtrRight :: DualLinearRadicals el  -- ^ for right @RAC@
                    }
instance (Field el, Show el) => Show (MulTermRadicals el) where
    show (MulTermRadicals l r) =
        "{L: " ++ show l ++ "} || {R: " ++ show r ++ "}"

-- | A dual encryption key pair made of two dual keys. Usually the static dual
-- key and the dynamic dual key.
type DualKeyPair el = (DualKey el, DualKey el)

-- | /D/ual /R/andomized /A/ffine /E/ncoding.
data DRAE el = DRAE !(DualKeyPair el)
                    !(DList (MulTermRadicals el))
                    !(DList (DualLinearExpr el))

-- | /D/ual /R/andomized /A/ffine /C/ircuit fragment.
type DRACFragment el = (DualVarName, DRAE el)

-- | /D/ual /R/andomized /A/ffine /C/ircuit.
type DRAC el = DList (DRACFragment el)

-- | /R/andomized /A/ffine /E/ncoding.
data RAE r val el =
    RAE { raeMulTerms   :: [(r val, r val)]
        , raeAddTerms   :: [val]
        , raeConst      :: el
        }

-- | /R/andomized /A/ffine /C/ircuit fragment.
type RACFragment el = (VariableName, RAE Radicals OAFEReference el)

-- | /R/andomized /A/ffine /C/ircuit.
type RAC el = [RACFragment el]

instance (Show val, Show (r val), Show el) => Show (RAE r val el) where
    show = prettyPrintRAE

prettyPrintRAE :: (Show val, Show (r val), Show el) => RAE r val el -> String
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

instance (Field el, Show el) => Show (DRAE el) where
    show = prettyPrintDRAE

genDualVarName :: VariableName -> DualVarName
genDualVarName vn = DualVarName vn (leftVar vn) (rightVar vn)

data DualVarName = DualVarName { dvnVarName :: !VariableName
                               , dvnLeftVarName :: !VariableName
                               , dvnRightVarName :: !VariableName
                               }
instance Eq DualVarName where
    (==) l r = dvnVarName l == dvnVarName r

instance Show DualVarName where
    show v = (show . dvnVarName) v

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
