{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

#include "ntl_interface_easy.h"

module Math.FiniteFields.Foreign.FFInterface where

import Data.ByteString (ByteString)
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (toBool)
import System.IO.Unsafe
import qualified Data.ByteString.Unsafe as BS

{- REMINDER: Seems not necessary, but there are use-after-free errors
 -           if the bang pattern or the entire usage of toBoolM gets
 -           deleted?!?
 -}
toBoolM :: (Eq a, Num a) => a -> IO Bool
toBoolM a =
    do let !a' = toBool a
       return a'

withOpaqueElement :: OpaqueElement -> (Ptr OpaqueElement -> IO b) -> IO b
{#pointer OpaqueElement foreign newtype #}

foreign import ccall "ntl_interface_easy.h &ff_free_element"
  ffFreeElementPtr :: FunPtr (Ptr OpaqueElement -> IO ())

newGarbageCollectedPointer :: Ptr OpaqueElement -> IO OpaqueElement
newGarbageCollectedPointer p =
    do fp <- newForeignPtr ffFreeElementPtr p
       return $ OpaqueElement fp

newNonCollectedPointer :: Ptr OpaqueElement -> IO OpaqueElement
newNonCollectedPointer p =
    do fp <- newForeignPtr_ p
       return $ OpaqueElement fp

newString :: CString -> IO String
newString p = do
  str <- peekCString p
  free p
  return str

{#fun pure unsafe ff_zero_element as
    ^ { } -> `OpaqueElement' newNonCollectedPointer* #}

{#fun pure unsafe ff_one_element as
    ^ { } -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_add_elements as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_sub_elements as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_mul_elements as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_invert_element as
    ^ { withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_element_to_string as
    ^ { withOpaqueElement* `OpaqueElement' } -> `String' newString* #}

{#fun pure unsafe ff_element_from_string as
    ^ { `String' } -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_equals as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `Bool' toBoolM* #}

ffElementFromBytes :: ByteString -> OpaqueElement
ffElementFromBytes str =
    unsafePerformIO $
    newGarbageCollectedPointer $
    unsafePerformIO $
    BS.unsafeUseAsCStringLen str $ \(strp, len) ->
        {#call unsafe ff_element_from_bytes #} (castPtr strp) (fromIntegral len)
