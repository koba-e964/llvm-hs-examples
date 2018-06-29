{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor.Foldable hiding (fold)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text.Lazy (Text)
import Foreign.Ptr

import qualified Data.ByteString.Char8      as BS
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text.Lazy.IO          as Text
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.Constant          as LLVM
import qualified LLVM.AST.Float             as LLVM
import qualified LLVM.AST.Type              as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR
import qualified LLVM.Pretty                as LLVMPretty
import qualified LLVM.Context               as LLVMJIT
import qualified LLVM.Linking               as LLVMJIT
import qualified LLVM.Module                as LLVMJIT
import qualified LLVM.OrcJIT                as LLVMJIT
import qualified LLVM.Target                as LLVMJIT

-- * Core expression type

-- | An expression will be any value of type @'Fix' 'ExprF'@, which
--   has as values arbitrarily nested applications of constructors from
--   'ExprF'. This is equivalent to just having an 'Expr type with no type
--   parameter and all @a@s replaced by 'Expr', but the 'Functor' and 'Foldable'
--   instances are quite handy, especially combined with the /recursion-schemes/
--   library.
--
--   This type allows us to express the body of a @Double -> Double@ function,
--   where 'Var' allows us to refer to the (only) argument of the function.
data ExprF a
  = Right
  | Left
  | Inc
  | Dec
  | Output
  | Input
  | Bracket a
  deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

-- * Helpers for building expressions


notImplemented :: String -> a
notImplemented = error . (++ " is not implemented")

-- * Pretty printing

-- | Pretty print an 'Expr'

-- | Core pretty printing function. For each
--   constructor that contains sub expressions,
--   we get the string for the sub expression as
--   well as the original 'Expr' value, to help us
--   decide when to use parens.

paren :: Bool -> String -> String
paren b x
  | b         = "(" ++ x ++ ")"
  | otherwise = x

function name arg =
  name ++ paren True arg


-- * Simple evaluator


xparam :: LLVMIR.ParameterName
xparam = LLVMIR.ParameterName "x"

-- | Generate @declare@ statements for all the intrinsics required for
--   executing the given expression and return a mapping from function
--   name to 'Operand' so that we can very easily refer to those functions
--   for calling them, when generating the code for the expression itself.
declarePrimitives
  :: LLVMIR.MonadModuleBuilder m => m (Map String LLVM.Operand)
declarePrimitives = fmap Map.fromList $ do
  puts <- LLVMIR.extern (LLVM.mkName "puts") [LLVM.ptr LLVM.i8] LLVM.void
  return $ [("puts", puts)]


-- | Generate an LLVM IR module for the given expression,
--   including @declare@ statements for the intrinsics and
--   a function, always called @f@, that will perform the copoutations
--   described by the 'Expr'ession.
codegen :: Expr -> LLVM.Module
codegen fexpr = LLVMIR.buildModule "arith.ll" $ do
  prims <- declarePrimitives
  _ <- LLVMIR.function "main" [(LLVM.double, xparam)] LLVM.void $ \[arg] -> do
    dat <- LLVMIR.alloca LLVM.i8 (Just $ LLVM.ConstantOperand (LLVM.Int 32 0x10000)) 1 `LLVMIR.named` "dat"
    pos <- LLVMIR.alloca LLVM.i32 Nothing 0 `LLVMIR.named` "pos"
    LLVMIR.store pos 0 (LLVM.ConstantOperand $ LLVM.Int 32 0x8000)
    cataM (alg dat pos) fexpr
  return ()

  where alg dat pos Inc = do
          idx <- LLVMIR.load pos 0
          ptr <- LLVMIR.gep dat [idx]
          val <- LLVMIR.load ptr 0
          newVal <- LLVMIR.add val (LLVM.ConstantOperand $ LLVM.Int 8 0)
          LLVMIR.store ptr 0 newVal
          return ()
        {-
        alg arg ps (Output a) = do
          ptr <- LLVMIR.alloca LLVM.i8 (Just $ LLVM.ConstantOperand (LLVM.Int 32 10)) 4
          LLVMIR.store ptr 4 (LLVM.ConstantOperand $ LLVM.Int 8 0x61)
          ptr8 <- LLVMIR.bitcast ptr (LLVM.ptr LLVM.i8)
          _ <- LLVMIR.call (ps Map.! "puts") [(ptr8, [])]
          return a
        -}

codegenText :: Expr -> Text
codegenText = LLVMPretty.ppllvm . codegen

printCodegen :: Expr -> IO ()
printCodegen = Text.putStrLn . codegenText

-- * JIT compilation & loading

-- | This allows us to call dynamically loaded functions
foreign import ccall "dynamic"
  mkDoubleFun :: FunPtr (Double -> Double) -> (Double -> Double)

resolver
  :: LLVMJIT.IRCompileLayer l -> LLVMJIT.MangledSymbol -> IO LLVMJIT.JITSymbol
resolver compileLayer symbol
  = LLVMJIT.findSymbol compileLayer symbol True

symbolFromProcess :: LLVMJIT.MangledSymbol -> IO LLVMJIT.JITSymbol
symbolFromProcess sym = (\addr -> LLVMJIT.JITSymbol addr (LLVMJIT.JITSymbolFlags False True))
    <$> LLVMJIT.getSymbolAddressInProcess sym

resolv :: LLVMJIT.IRCompileLayer l -> LLVMJIT.SymbolResolver
resolv cl = LLVMJIT.SymbolResolver (\sym -> LLVMJIT.findSymbol cl sym True) symbolFromProcess

printIR :: MonadIO m => ByteString -> m ()
printIR = liftIO . BS.putStrLn . ("\n*** LLVM IR ***\n\n"<>)

-- | JIT-compile the given 'Expr'ession and use the resulting function.
withSimpleJIT
  :: Expr
  -> IO ()
withSimpleJIT expr = do
  -- LLVMJIT.loadLibraryPermanently (Just "/usr/lib/libc++.dylib")
  LLVMJIT.loadLibraryPermanently (Just "/usr/lib/libc.dylib")
  LLVMJIT.loadLibraryPermanently (Nothing)
  LLVMJIT.withContext $ \context -> do
  LLVMJIT.withModuleFromAST context (codegen expr) $ \mod' ->
    LLVMJIT.withHostTargetMachine $ \tm ->
    LLVMJIT.withObjectLinkingLayer $ \objectLayer ->
    LLVMJIT.withIRCompileLayer objectLayer tm $ \compileLayer -> do
      asm <- LLVMJIT.moduleLLVMAssembly mod'
      printIR asm
      BS.writeFile "test.ll" asm

-- * Utilities

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t -> m a
cataM alg = c where
  c = alg <=< traverse c . project

-- * Main

e :: Expr
e = Fix (Inc)

main :: IO ()
main = do
  content <- BS.readFile "input.txt"
  BS.putStrLn content
  withSimpleJIT e
  return ()