{-# LANGUAGE OverloadedStrings #-}

module Emit where

import GHC.Float (double2Float, int2Float)

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

types = Map.fromList [
      (S.IntType, integer)
    , (S.FloatType, float)
  ]

toLLVMType :: S.Type -> AST.Type
toLLVMType st = case Map.lookup st types of
                 Nothing -> float
                 Just t -> t

toSig :: [S.DecParam] -> [(AST.Type, AST.Name)]
toSig = map (\(S.DecParam ptype id) -> (toLLVMType ptype, AST.Name id))

codegenMain :: S.Statement -> LLVM ()
codegenMain exp = do
  define float "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

codegenTop :: S.Statement -> LLVM ()
codegenTop (S.Dec (S.DecFun name args rty body)) = do
  define (toLLVMType rty) name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.DecParam ptype id) -> do
        var <- alloca $ toLLVMType ptype
        store var (local (AST.Name id))
        assign id var
      let (S.Block sts) = body
        in forM (init sts) cgen >>
           cgen (last sts) >>= ret

--codegenTop (S.Extern name args) = do
--  external float name fnargs
--  where fnargs = toSig args
--

codegenTop exp = do
  define float "exp" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

--codegenTop s = do error $ "Invalid toplevel operation."

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

resolveType :: S.Datum -> C.Constant
resolveType (S.Float n) = C.Float (F.Single $ double2Float n)
-- resolveType (S.Integer n) = C.Int 32 n
resolveType (S.Integer n) = C.Float (F.Single $ int2Float $ fromIntegral n)
-- Other types can beadded here

zero :: C.Constant
zero = C.Float (F.Single 0.0)

cgen :: S.Statement -> Codegen AST.Operand
-- This part is for operator overloading
--cgen (S.UnaryOp op a) = do
--  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.Dec (S.DecVar id t)) = do
  i <- alloca $ toLLVMType t
  zval <- return $ cons zero
  store i zval
  assign id i
  return zval
cgen (S.Assign var val) = do
  a <- getvar var
  cval <- cgen (S.Expr val)
  store a cval
  return cval
cgen (S.Expr (S.Op op a b)) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen (S.Expr a)
      cb <- cgen (S.Expr b)
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Expr (S.Id x)) = getvar x >>= load
cgen (S.Expr (S.Datum d)) = return $ cons $ resolveType d
cgen (S.Expr (S.FunCall fn args)) = do
  largs <- mapM cgen [S.Expr a | a <- args]
  call (externf (AST.Name fn)) largs
cgen x = do error $ "fottiti!!! " ++ show x

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

--liftError :: ErrorT String IO a -> IO a
--liftError = runErrorT >=> either fail return

safeInit [] = []
safeInit list = init list

codegen :: AST.Module -> [S.Statement] -> IO AST.Module
--codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = do x <- mapM codegenTop $ safeInit fns
                 m <- mapM codegenMain $ [last fns]
                 return $ x ++ m
    defsOld = AST.moduleDefinitions mod
    mnew = AST.Module { AST.moduleName = AST.moduleName mod
                      , AST.moduleDataLayout = AST.moduleDataLayout mod
                      , AST.moduleTargetTriple = AST.moduleTargetTriple mod
                      , AST.moduleDefinitions = safeInit defsOld}
    oldast  = runLLVM mnew modn
--codegen mod fns = withContext $ \context ->
--  liftError $ withModuleFromAST context newast $ \m -> do
--    llstr <- moduleLLVMAssembly m
--    putStrLn llstr
--    return newast
--  where
--    modn    = mapM codegenTop fns
--    newast  = runLLVM mod modn
