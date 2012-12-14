{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-} -- recommended by GHC manual
{-# OPTIONS_GHC -Wwarn #-}

#include "ntl_interface_easy.h"

module Math.FiniteFields.Foreign.FFInterface where

import Data.ByteString (ByteString, useAsCString)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS

{- REMINDER: Seems not necessary, but there are use-after-free errors
 -           if the bang pattern or the entire usage of toBoolM gets
 -           deleted?!?
 -}
toBoolM :: (Eq a, Num a) => a -> IO Bool
toBoolM a =
    do let !a' = toBool a
       return a'

{- REMINDER: This function is from bytestring-0.10.2.0, but many
-            libraries seem to depend on 0.10.0.0, so I copied it
-}
unsafePackMallocCStringLen :: CStringLen -> IO ByteString
unsafePackMallocCStringLen (cstr, len) = do
        fp <- newForeignPtr BS.c_free_finalizer (castPtr cstr)
        return $! BS.PS fp 0 len

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
newString p =
    do str <- peekCString p
       free p
       return str

newUtf8ByteString :: CString -> IO ByteString
newUtf8ByteString p =
    do str <- BS.packCString p
       free p
       return str

newSizedByteString :: CString -> CSize -> IO ByteString
newSizedByteString p l =
    do str <- BS.packCStringLen (p, fromIntegral l)
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

{#fun pure unsafe ff_div_elements as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_invert_element as
    ^ { withOpaqueElement* `OpaqueElement' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_element_to_string as
    ^ { withOpaqueElement* `OpaqueElement' } -> `String' newString* #}

{#fun pure unsafe ff_element_to_string as ffElementToUtf8ByteString
      { withOpaqueElement* `OpaqueElement' }
      -> `ByteString' newUtf8ByteString* #}

{#fun pure unsafe ff_element_from_string as
    ^ { `String' } -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_element_from_string as ffElementFromUtf8ByteString
      { useAsCString* `ByteString' }
      -> `OpaqueElement' newGarbageCollectedPointer* #}

{#fun pure unsafe ff_equals as
    ^ { withOpaqueElement* `OpaqueElement', withOpaqueElement* `OpaqueElement' }
      -> `Bool' toBoolM* #}

ffElementFromBytes :: ByteString -> OpaqueElement
ffElementFromBytes str =
    unsafePerformIO $
    do p <- BS.unsafeUseAsCStringLen str $ \(strp, len) ->
                {#call unsafe ff_element_from_bytes #} (castPtr strp)
                                                       (fromIntegral len)
       gbp <- newGarbageCollectedPointer p
       return $! gbp

ffElementToBytes :: OpaqueElement -> ByteString
ffElementToBytes e =
    unsafePerformIO $
    do bswl <- withOpaqueElement e $ \e' ->
               alloca $ \s ->
               {#call unsafe ff_element_to_bytes #} e' s >>= \bs ->
               peek s >>= \ps -> return (bs, ps)
       let (p, size) = bswl
       unsafePackMallocCStringLen (p, fromIntegral size)

{#fun pure unsafe ff_sizeof_element as ^ { } -> `Int'  #};

-- vim: set filetype=haskell :
