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

-- | Simply to debug the implementation.
--
-- Don't use this module in production code.
module Math.FiniteFields.DebugField (DebugField) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.CryptoRandom (CRandom(..))
import Data.FieldTypes (Field(..))
import Data.RAE.Conduit (ByteSerializable(..))

newtype DebugField = DebugField { unDF :: String }

instance ByteSerializable DebugField where
    serializeBytes = BSL.fromStrict . BS8.pack . unDF
    parseBytes = DebugField . BS8.unpack . BSL.toStrict
instance Field DebugField where
    invert (DebugField s) = DebugField $ "("++s++")^-1"
    one = DebugField "1"
    zero = DebugField "0"
instance Num DebugField where
    (+) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "+" ++ r ++ ")"
    (-) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "-" ++ r ++ ")"
    (*) (DebugField l) (DebugField r) = DebugField $ "(" ++ l ++ "*" ++ r ++ ")"
    negate (DebugField e) = DebugField $ "(-"++e++")"
    signum (DebugField e) = DebugField $ "signum("++e++")"
    abs (DebugField e) = DebugField $ "|"++e++"|"
    fromInteger i = DebugField $ show i
instance CRandom DebugField where
    crandom g =
        case crandom g of
           Right (i, g') ->
               Right (DebugField $ "R"++ (show ((i::Int) `mod` 9999)), g')
           Left e -> Left e

instance Eq DebugField where
    (==) (DebugField l) (DebugField r) = l == r
instance Read DebugField where
    readsPrec n s = (map (\(a,b) -> (DebugField a, b)) . (readsPrec n)) s
instance Show DebugField where
    show (DebugField e) = show e
