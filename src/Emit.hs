{-# LANGUAGE OverloadedStrings #-}

module Emit where

import GHC.Float (double2Float)

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

--toSig :: [S.DecParam] -> [(AST.Type, AST.Name)]
--toSig = map (\x -> (float, AST.Name x))

codegenTop :: S.Statement -> LLVM ()
--codegenTop (S.Function name args body) = do
--  define float name fnargs bls
--  where
--    fnargs = toSig args
--    bls = createBlocks $ execCodegen $ do
--      entry <- addBlock entryBlockName
--      setBlock entry
--      forM args $ \a -> do
--        var <- alloca float
--        store var (local (AST.Name a))
--       assign a var
--      cgen body >>= ret
--
--codegenTop (S.Extern name args) = do
--  external float name fnargs
--  where fnargs = toSig args
--
codegenTop exp = do
  define float "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp float test

-- Dict to map binOps with actual llvm routines
binops = Map.fromList [
      (S.Plus, fadd)
    , (S.BinMinus, fsub)
    , (S.Times, fmul)
    , (S.Divide, fdiv)
  ]

cgen :: S.Statement -> Codegen AST.Operand
-- This part is for operator overloading
--cgen (S.UnaryOp op a) = do
--  cgen $ S.Call ("unary" ++ op) [a]
--cgen (S.Op S.Eq (S.Id var) val) = do
--  a <- getvar var
--  cval <- cgen val
--  store a cval
--  return cval
cgen (S.Expr (S.Op op a b)) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen (S.Expr a)
      cb <- cgen (S.Expr b)
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Expr (S.Id x)) = getvar x >>= load
cgen (S.Expr (S.Datum (S.Float n))) = return $ cons $ C.Float (F.Single $ double2Float n)
--cgen (S.FunCall fn args) = do
--  largs <- mapM cgen args
--  call (externf (AST.Name fn)) largs
cgen x = do error $ "fottiti!!!" ++ show x

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

--liftError :: ErrorT String IO a -> IO a
--liftError = runErrorT >=> either fail return

codegen :: AST.Module -> [S.Statement] -> IO AST.Module
--codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn
--codegen mod fns = withContext $ \context ->
--  liftError $ withModuleFromAST context newast $ \m -> do
--    llstr <- moduleLLVMAssembly m
--    putStrLn llstr
--    return newast
--  where
--    modn    = mapM codegenTop fns
--    newast  = runLLVM mod modn
