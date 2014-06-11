-- | Pointers exist in Address Spaces 
module LLVM.General.AST.AddrSpace where

import Data.Word

-- | See <http://llvm.org/docs/LangRef.html#pointer-type>
data AddrSpace = AddrSpace Word32
   deriving (Eq, Ord, Read, Show)
