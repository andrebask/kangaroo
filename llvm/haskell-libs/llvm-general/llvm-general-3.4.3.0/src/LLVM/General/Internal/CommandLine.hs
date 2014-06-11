module LLVM.General.Internal.CommandLine where

import qualified LLVM.General.Internal.FFI.CommandLine as FFI

import Foreign.Ptr

import Control.Monad.AnyCont
import Control.Monad.IO.Class

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

-- | <http://llvm.org/doxygen/namespacellvm_1_1cl.html#a992a39dae9eb8d4e54ffee5467902803>
-- Sadly, there is occasionally some configuration one would like to control
-- in LLVM which are accessible only as command line flags setting global state,
-- as if the command line tools were the only use of LLVM. Very sad.
parseCommandLineOptions :: [String] -> Maybe String -> IO ()
parseCommandLineOptions args overview = flip runAnyContT return $ do
  args <- encodeM args
  overview <- maybe (return nullPtr) encodeM overview
  liftIO $ FFI.parseCommandLineOptions args overview