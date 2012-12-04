{-# OPTIONS_GHC -F -pgmF htfpp #-}

import qualified Math.Polynomials as P

-- # HTF
import Test.Framework

prop_hornerEqMonomial :: Integer -> [Integer] -> Bool
prop_hornerEqMonomial x coeffs =
    let hornerOut = P.horner x coeffs
        monomialOut = P.monomial x coeffs
     in hornerOut == monomialOut

main = htfMain htf_thisModulesTests
