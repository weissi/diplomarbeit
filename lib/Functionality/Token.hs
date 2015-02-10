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

module Functionality.Token (runOAFEEvaluation) where

-- # STDLIB
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, takeTMVar)
import Control.Monad.STM (atomically)

-- # SITE PACKAGES
import qualified Data.HashMap.Strict as HM

-- # LOCAL
import Data.FieldTypes (Field)
import Data.OAFE ( OAFEConfiguration, OAFEEvaluationRequest
                 , OAFEEvaluationResponse
                 , processOAFEEvaluationRequest
                 )

-- | This is the main 'Token' functionality: Process an OAFE evaluation request.
--
-- The @OAFEConfiguration@ is a @TMVar@ because each variable can only be
-- evaluated once.
runOAFEEvaluation :: Field el
                  => TMVar (OAFEConfiguration el)  -- ^ 'OAFEConfiguration'
                  -> OAFEEvaluationRequest el      -- ^ 'OAFEEvaluationRequest'
                  -> IO (OAFEEvaluationResponse el)-- ^ 'OAFEEvaluationResponse'
runOAFEEvaluation vOAC req =
   atomically $
       do oac <- takeTMVar vOAC
          let rsp = processOAFEEvaluationRequest oac req
              var = fst rsp
              oac' = HM.delete var oac
          putTMVar vOAC oac'
          return $! rsp
