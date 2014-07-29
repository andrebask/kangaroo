{-# LANGUAGE OverloadedStrings #-}

module Emit where

import GHC.Float (double2Float, int2Float)

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Type as T

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


blks expr = createBlocks $ execCodegen $ do
  entry <- addBlock entryBlockName
  setBlock entry
  cgen expr >>= ret

codegenMain :: S.Statement -> LLVM ()
codegenMain exp@(S.Dec (S.DecFun name _ _ _)) = do
  codegenTop exp
  define float "main" [] (blks (S.Expr (S.Datum (S.Integer 0))))
codegenMain exp = do
  define float "last" [] (blks exp)
  define float "main" [] (blks (S.Expr (S.FunCall "last" [])))

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

condops = Map.fromList [
      (S.Gt, fcmp FP.OGT)
    , (S.Lt, fcmp FP.OLT)
    , (S.Geq, fcmp FP.OGE)
    , (S.Leq, fcmp FP.OLE)
    , (S.Eq, fcmp FP.OEQ)
    , (S.Neq, fcmp FP.ONE)
    , (S.And, cand)
    , (S.Or, cor)
  ]

resolveType :: S.Datum -> C.Constant
resolveType (S.Float n) = C.Float (F.Single $ double2Float n)
-- resolveType (S.Integer n) = C.Int 32 n
resolveType (S.Integer n) = C.Float (F.Single $ int2Float $ fromIntegral n)
resolveType (S.Bool True) = C.Int 32 1
resolveType (S.Bool False) = C.Int 32 0
-- Other types can beadded here

zero :: AST.Operand
zero = cons $ C.Float (F.Single 0)

one :: AST.Operand
one = cons $ C.Float (F.Single 1)

tvoid :: AST.Operand
tvoid = cons $ C.Undef T.VoidType

-- Utility function to compile ElseIf expr, see also cgen
convertGen :: [(S.Expr, [S.Statement])] -> Codegen AST.Operand
convertGen ((elseCond,elseBody):[]) = cgen (S.If elseCond elseBody [])
convertGen ((elseCond,elseBody):elseIfs) = cgen (S.ElseIf elseCond elseBody elseIfs)

-- Utility function to compile a body, see also cgen
-- If the body is empt, it compiles to void
cgenBody :: [S.Statement] -> Codegen AST.Operand
cgenBody [] = do return tvoid
cgenBody (s:[]) = cgen s
cgenBody sts = do forM (init sts) cgen
                  cgen (last sts)


cgen :: S.Statement -> Codegen AST.Operand
-- This part is for operator overloading
--cgen (S.UnaryOp op a) = do
--  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.Dec (S.DecVar id t)) = do
  i <- alloca $ toLLVMType t
  zval <- return zero
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
cgen (S.Expr (S.CondOp op a b)) = do
  case Map.lookup op condops of
    Just f  -> do
      ca <- cgen (S.Expr a)
      cb <- cgen (S.Expr b)
      f ca cb
    Nothing -> error "No such comparison operator"
cgen (S.Expr (S.Id x)) = getvar x >>= load
cgen (S.Expr (S.Datum d)) = return $ cons $ resolveType d
cgen (S.Expr (S.FunCall fn args)) = do
  largs <- mapM cgen [S.Expr a | a <- args]
  call (externf (AST.Name fn)) largs
cgen (S.If cond thenSts elseSts) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen (S.Expr cond)
  --test <- fcmp FP.ONE zero cond
  cbr cond ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgenBody thenSts
  br ifexit                            -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgenBody elseSts         -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi float [(trval, ifthen), (flval, ifelse)]
cgen (S.ElseIf cond thenSts elseIfs) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen (S.Expr cond)
  cbr cond ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgenBody thenSts -- Generate code for the true branch
  br ifexit                            -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- convertGen elseIfs         -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi float [(trval, ifthen), (flval, ifelse)]
cgen x = do error $ "Boom!!! " ++ show x

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
    modn    = case fns of
                 [] -> mapM codegenMain $ []
                 (f:[]) -> mapM codegenMain $ [f]
                 (f:fs) -> do x <- mapM codegenTop $ safeInit fns
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
