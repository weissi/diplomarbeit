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
                  => TMVar (OAFEConfiguration el)
                  -> OAFEEvaluationRequest el
                  -> IO (OAFEEvaluationResponse el)
runOAFEEvaluation vOAC req =
   do oac <- atomically $ takeTMVar vOAC
      let rsp = processOAFEEvaluationRequest oac req
          var = fst rsp
          oac' = HM.delete var oac
      atomically $ putTMVar vOAC oac'
      return rsp
