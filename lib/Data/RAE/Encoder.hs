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

-- | Encodes 'Expr' as 'DRAC's, encode 'DRAC's as 'RAC's.
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

-- | Directly transform an 'Expr' to a 'RAC'.
exprToRAC :: (CryptoRandomGen g, Field el, CRandom el)
          => g       -- ^ Instanciated random number generator.
          -> Expr el -- ^ The arithmetic expression ('Expr') to encode.
          -> (Either GenError g, RAC el, OAFEConfiguration el) -- ^ The 'RAC'
exprToRAC g expr =
       let (errM, drac) = exprToDRAC g expr
           (rac, oac) = singularizeDRAC drac
        in (errM, rac, oac)
