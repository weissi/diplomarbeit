-- | Encodes @Expr@ as @DRAC@s, encode @DRAC@s as @RAC@s.
module Data.RAE.Encoder ( exprToDRAC, exprToRAC, singularizeDRAC ) where

-- # SITE PACKAGES
import Control.Monad.CryptoRandom (GenError, CRandom)
import Crypto.Random (CryptoRandomGen)

-- # LOCAL
import Data.ExpressionTypes (Expr)
import Data.FieldTypes (Field)
import Data.OAFE (OAFEConfiguration)
import Data.RAE.Encoder.Internal.DRAC
import Data.RAE.Encoder.Internal.RAC
import Data.RAE.Types (RAC)

-- | Directly transform an @Expr@ to a @RAC@.
exprToRAC :: (CryptoRandomGen g, Field el, CRandom el)
          => g       -- ^ The random number generator
          -> Expr el -- ^ The arithmetic expression to encode
          -> (Either GenError g, RAC el, OAFEConfiguration el) -- ^ The RAC
exprToRAC g expr =
       let (errM, drac) = exprToDRAC g expr
           (rac, oac) = singularizeDRAC drac
        in (errM, rac, oac)
