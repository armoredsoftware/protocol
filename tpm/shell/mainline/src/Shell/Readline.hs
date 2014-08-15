{-# OPTIONS -XCPP -fvia-C #-}
{-# OPTIONS -XForeignFunctionInterface #-}
{-# LANGUAGES CPP ForeignFunctionInterface #-}
module Shell.Readline where
import Foreign.Ptr
import Foreign.C
import Foreign.C.Error
import Foreign.Storable

foreign import ccall "readline/history.h clear_history" clear_history :: IO ()
foreign import ccall "readline/history.h stifle_history" stifle_history :: CInt -> IO ()
foreign import ccall "readline/history.h read_history" read_history :: CString -> IO Errno
foreign import ccall "readline/history.h write_history" write_history :: CString -> IO Errno
-- foreign import ccall "readline/history.h &history_max_entries" history_max_entries :: Ptr CInt
foreign import ccall "readline/history.h using_history" using_history :: IO ()

