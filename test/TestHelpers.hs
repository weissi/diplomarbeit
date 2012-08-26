module TestHelpers where

import Test.Framework
import Math.FiniteFields.F2Pow256

instance Arbitrary F2Pow256 where
    arbitrary = fmap fromInteger $ choose (0, 2^256-1)
