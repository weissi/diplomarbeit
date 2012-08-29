{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- # Standard Library
import Data.Word
import Data.ByteString (pack, useAsCStringLen)
import Foreign.C.String

-- # Site Packages

-- # LOCAL
import Data.Helpers

-- # HTF
import Test.Framework
import TestHelpers

-- # DEBUG
import Debug.Trace

_MAX_NUM_ :: Integer
_MAX_NUM_ = 2^256-1

_MAX_NUM_STR_ :: String
_MAX_NUM_STR_ = "\255\255\255\255\255\255\255\255\255\255\255\255\255\255" ++
                "\255\255\255\255\255\255\255\255\255\255\255\255\255\255" ++
                "\255\255\255\255"

_SOME_NUM_ :: Integer
_SOME_NUM_ = 1235412334254465469823

_SOME_NUM_STR_ :: String
_SOME_NUM_STR_ = "\DEL\205\143e\130L\202\248B"

test_integralBytesMaxNum :: IO ()
test_integralBytesMaxNum =
    do let actual = integralBytes _MAX_NUM_
           allBitsSet :: Word8
           allBitsSet = 0xff
           expected = pack $ replicate (256 `div` 8) allBitsSet
       assertEqual expected actual

test_cRepresentationMaxNum :: IO ()
test_cRepresentationMaxNum =
    do useAsCStringLen (integralBytes _MAX_NUM_) $ \ (sptr, len) ->
        do assertEqual 32 len
           s <- peekCAString sptr
           assertEqual _MAX_NUM_STR_ s

test_cRepresentationSomeNum :: IO ()
test_cRepresentationSomeNum =
    do useAsCStringLen (integralBytes _SOME_NUM_) $ \ (sptr, len) ->
        do assertEqual 9 len
           s <- peekCAString sptr
           assertEqual _SOME_NUM_STR_ s

test_cRepresentation256 :: IO ()
test_cRepresentation256 =
    do useAsCStringLen (integralBytes (256 :: Integer)) $ \ (sptr, len) ->
        do assertEqual 2 len
           s <- peekCAStringLen (sptr, len)
           assertEqual "\NUL\SOH" s

main = htfMain htf_thisModulesTests
