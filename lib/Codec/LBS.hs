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

{-# LANGUAGE BangPatterns #-}

-- | This module transforms arithmetic expressions (@Expr@) to Linear Bijection
-- Straight--line Programs (LBS programs) (@LBSProgram@).
--
-- This approach has been discontinued. The functionality can still be used by
-- running the `ExprToMRM` program.
module Codec.LBS ( InputValues
                 , renderLBSProgram
                 , runLBS, lbsFromExpr, lbsProgramLength
                 , LBSProgram, LBSStmt(..), Register(..), RegisterState
                 , OffsetDirection(..), ScaleFactor(..)
                 ) where

import Control.Monad (liftM)
import Control.Monad.State.Strict (State, get, put, runState)
import Data.DList (DList())
import Data.ExpressionTypes (Expr(..), Operator(..))
import Data.Map (Map())
import Data.Monoid (mappend)
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Builder (Builder(), fromString, toLazyText)
import qualified Data.DList as DL
import qualified Data.Map as M

-- Public Data Types
-- | An LBS program.
type LBSProgram = DL.DList LBSStmt

-- | A statement of an LBS program.
data LBSStmt = Offset Register OffsetDirection Register ScaleFactor

-- | A register.
data Register = Reg { getReg :: Integer } deriving (Eq)

-- | Wheather to offset a register additive or subtractive.
data OffsetDirection = OffsetPlus | OffsetMinus

-- | Factor to scale a @Register@.
data ScaleFactor = ScaleFactorConstant Integer
                 | ScaleFactorInput String

-- | The input values for @LBSProgram@ evaluation.
type InputValues = Map String Integer

type RegisterState = Map Integer Integer

-- Internal Data Types

type RegisterStateMonad = State RegisterState

type ErrorList = DList String

-- Internal Constants
_REG_CONST_1_ :: Register
_REG_CONST_1_ = Reg 0

_INITIAL_REGISTER_STATE_ :: RegisterState
_INITIAL_REGISTER_STATE_ = M.fromList [(0,1)]

fromShow :: Show a => a -> Builder
fromShow a = fromString $ show a

renderLBSStmt :: LBSStmt -> Builder
renderLBSStmt (Offset (Reg o) dir (Reg i) sf) =
    fromString "R" `mappend`
    fromShow o `mappend`
    fromString " <- R" `mappend`
    fromShow o `mappend`
    fromString " " `mappend`
    fromString dirString `mappend`
    fromString " R" `mappend`
    fromShow i `mappend`
    fromString " * " `mappend`
    fromString sfString
    where dirString =
              case dir of
                OffsetPlus -> "+"
                OffsetMinus -> "-"
          sfString =
              case sf of
                ScaleFactorConstant c -> show c
                ScaleFactorInput input -> input

-- | Render an @LBSProgram@ to @Text@.
renderLBSProgram :: LBSProgram -> Text
renderLBSProgram lbs = toLazyText $ DL.foldr joinStmts (fromString "") lbs
    where
    joinStmts :: LBSStmt -> Builder -> Builder
    joinStmts l s = renderLBSStmt l `mappend` fromString "\n" `mappend` s

freeRegister :: [Register] -> Register
freeRegister dirtyRegs = Reg (1 + maximum (map getReg dirtyRegs))

-- |Offsets `output register' by +/- `scale register' * (`expr 1' + `expr 2')
lbsFromAddExpr :: [Register]       -- dirty registers
               -> Register         -- output register
               -> OffsetDirection  -- +/-
               -> Register         -- scale register
               -> Expr Integer     -- expr 1
               -> Expr Integer     -- expr 2
               -> LBSProgram
lbsFromAddExpr dirtyRegs regOut direction regScale el er  =
    lbsl `DL.append` lbsr
    where lbsl :: LBSProgram
          lbsl = lbsFromExpr' dirtyRegs regOut direction regScale el
          lbsr :: LBSProgram
          lbsr = lbsFromExpr' dirtyRegs regOut direction regScale er

-- |Offsets `output register' by +/- `scale register' * (`expr 1' * `expr 2')
lbsFromMulExpr :: [Register]       -- dirty registers
               -> Register         -- output register
               -> OffsetDirection  -- +/-
               -> Register         -- scale register
               -> Expr Integer     -- expr 1
               -> Expr Integer     -- expr 2
               -> LBSProgram
lbsFromMulExpr dirtyRegs regOut direction regScale el er =
    case direction of
      OffsetPlus ->
          DL.concat [ lbsFromExpr' dirtyRegs' rk OffsetMinus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetPlus ri el
                    , lbsFromExpr' dirtyRegs' rk OffsetPlus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetMinus ri el
                    ]
      OffsetMinus ->
          DL.concat [ lbsFromExpr' dirtyRegs' rk OffsetMinus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetMinus ri el
                    , lbsFromExpr' dirtyRegs' rk OffsetPlus rj er
                    , lbsFromExpr' dirtyRegs' rj OffsetPlus ri el
                    ]
    where ri = regScale
          rj = freeRegister dirtyRegs
          rk = regOut
          dirtyRegs' = rj : dirtyRegs

-- |Offsets `output register' by +/- `scale register' * `expr'
lbsFromExpr' :: [Register]       -- dirty registers
             -> Register         -- output register
             -> OffsetDirection  -- +/-
             -> Register         -- scale register
             -> Expr Integer     -- expr
             -> LBSProgram
lbsFromExpr' dirtyRegs regOut direction regScale e =
    case e of
      Op o el er ->
          case o of
            Plus -> lbsFromAddExpr dirtyRegs regOut direction regScale el er
            Minus -> undefined
            Times -> lbsFromMulExpr dirtyRegs regOut direction regScale el er
      Var s ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorInput s)
      Literal i ->
          DL.singleton $Offset regOut direction regScale (ScaleFactorConstant i)

-- | Transform an arithmetic expression (@Expr@) to an @LBSProgram@.
lbsFromExpr :: Expr Integer -> LBSProgram
lbsFromExpr = lbsFromExpr' alwaysDirtyRegs (Reg 1) OffsetPlus _REG_CONST_1_
    where alwaysDirtyRegs = [_REG_CONST_1_, Reg 1]

registerValue :: Register -> RegisterState -> Integer
registerValue (Reg r) = M.findWithDefault 0 r

registerValueM :: Register -> RegisterStateMonad Integer
registerValueM r = liftM (registerValue r) get

updateRegisterM :: Register -> Integer -> RegisterStateMonad ()
updateRegisterM (Reg !r) !v = liftM (M.insert r v) get >>= put

dirOp :: Num a => OffsetDirection -> a -> a -> a
dirOp dir = case dir of
              OffsetPlus -> (+)
              OffsetMinus -> (-)

execLBSStatement :: InputValues
                 -> LBSStmt
                 -> ErrorList
                 -> RegisterStateMonad ErrorList
execLBSStatement inputs (Offset regOut dir regScale sf) accumErrors =
    do outVal <- registerValueM regOut
       scaleVal <- registerValueM regScale
       case sfVal of
         Left err -> return $ accumErrors `DL.snoc` err
         Right val ->
             do updateRegisterM regOut (dirOp dir outVal (scaleVal * val))
                return accumErrors
       where sfVal :: Either String Integer
             sfVal =
                 case sf of
                   ScaleFactorConstant i -> Right i
                   ScaleFactorInput i ->
                       case M.lookup i inputs of
                         Nothing -> Left $ "input `" ++ i ++ "' undefined"
                         Just x -> Right x

execLBSProgram :: InputValues -> LBSProgram -> RegisterStateMonad ErrorList
execLBSProgram inputs p =
    combine (DL.toList stmts) DL.empty
    where stmts :: DList (ErrorList -> RegisterStateMonad ErrorList)
          stmts = DL.map (execLBSStatement inputs) p
          combine :: [ErrorList -> RegisterStateMonad ErrorList]
                  -> ErrorList
                  -> RegisterStateMonad ErrorList
          combine ss errs =
              case ss of
                [] -> return errs
                (s:ss') -> s errs >>= combine ss'

-- | Run (evaluate) an @LBSProgram@.
runLBS :: InputValues -> LBSProgram -> Maybe (Integer, RegisterState)
runLBS inputs lbs =
    if null $ DL.toList errs
      then Just (registerValue (Reg 1) state, state)
      else Nothing
    where (errs, state) =
              runState (execLBSProgram inputs lbs) _INITIAL_REGISTER_STATE_

-- | Calculate the length of an @LBSProgram@.
lbsProgramLength :: LBSProgram -> Int
lbsProgramLength = length . DL.toList
