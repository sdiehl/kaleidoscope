{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import Data.ByteString.Short
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

doubleTy :: LLAST.Type
doubleTy = LLAST.FloatingPointType LLAST.DoubleFP

data CodegenState = CodegenState {
    symbolTable :: Map Name LLAST.Operand
  , functionTable :: Map Name LLAST.Operand
  , modDefinitions :: [LLAST.Definition]
  , anonSupply :: Int
  }

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
    { symbolTable = Map.empty
    , functionTable = Map.empty
    , modDefinitions = []
    , anonSupply = 0
    }

getvar :: Name -> IRB.ModuleBuilderT Codegen LLAST.Operand
getvar name = maybe (error ("unknown variable: " ++ name)) id . Map.lookup name <$> gets symbolTable

assignvar :: Name -> LLAST.Operand -> IRB.ModuleBuilderT Codegen ()
assignvar name var = modify (\s -> s { symbolTable = Map.insert name var (symbolTable s) })

type Codegen =  (StateT CodegenState IO)

codegenIR :: Expr -> IRB.IRBuilderT (IRB.ModuleBuilderT Codegen) LLAST.Operand
codegenIR = \case
  Float d         -> IRB.double d
  Var name        -> (lift $ getvar name) >>= flip IRB.load 0
  UnaryOp op e -> do
    codegenIR (Call ("unary" ++ op) [e])
  BinaryOp "=" (Var var) val -> do
    i <- lift $ getvar var
    v <- codegenIR val
    IRB.store i 0 v
    return v
  BinaryOp op l r -> do
    let callWithOps f = join (f <$> codegenIR l <*> codegenIR r)
    case op of
      "+" -> callWithOps IRB.fadd
      "-" -> callWithOps IRB.fsub
      "*" -> callWithOps IRB.fmul
      "<" -> callWithOps (IRB.fcmp LLAST.ULT) >>= (\operand -> IRB.uitofp operand doubleTy)
      _   -> codegenIR (Call ("binary" ++ op) [l, r])
  Call name args  ->  do
    calleeOperand <- (maybe (error ("unknown function: " ++ name)) id . Map.lookup name) <$> gets functionTable
    IRB.call calleeOperand =<< traverse (fmap (,[]) . codegenIR) args

  If cond tr fl -> mdo
    condOp <- codegenIR cond
    false <- IRB.double 0
    test <- IRB.fcmp LLAST.ONE false condOp
    IRB.condBr test thenBlock elseBlock

    IRB.block `IRB.named` "if.then"
    trval <- codegenIR tr
    thenBlock <- IRB.currentBlock
    IRB.br contBlock

    IRB.block `IRB.named` "if.else"
    flval <- codegenIR fl
    elseBlock <- IRB.currentBlock
    IRB.br contBlock

    contBlock <- IRB.block `IRB.named` "if.cont"
    IRB.phi [(trval, thenBlock), (flval, elseBlock)]
  For ivar start cond step body -> mdo
    i <- IRB.alloca doubleTy Nothing 0 `IRB.named` "i"

    istart <- codegenIR start  -- Generate loop variable initial value
    stepVal <- codegenIR step  -- Generate loop variable step

    IRB.store i 0 istart
    lift $ assignvar ivar i
    IRB.br loopBlock

    loopBlock  <- IRB.block `IRB.named` "for.loop"
    codegenIR body
    iCurrent <- IRB.load i 0
    iNext <- IRB.fadd iCurrent stepVal
    IRB.store i 0 iNext

    condOp <- codegenIR cond
    falseOp <- IRB.double 0
    test <- IRB.fcmp LLAST.ONE falseOp condOp
    IRB.condBr test loopBlock contBlock

    contBlock  <- IRB.block `IRB.named` "for.cont"
    IRB.double 0
  Let var val body -> do
    i <- IRB.alloca doubleTy Nothing 0 `IRB.named` (packShort var)
    v <- codegenIR val
    IRB.store i 0 v
    lift $ assignvar var i
    codegenIR body

codegenDefn :: Defn -> (IRB.ModuleBuilderT Codegen) LLAST.Operand
codegenDefn = \case
  UnaryDef name args body ->
    codegenDefn (Function ("unary" ++ name) [args] body)

  BinaryDef name args body ->
    codegenDefn (Function ("binary" ++ name) args body)
  Function name args body -> do
    funcOperand <- IRB.function
          (LLAST.Name (packShort name))
          (fmap ((doubleTy,) . IRB.ParameterName . packShort) args)
          doubleTy
          $ \argOs -> do
      entryBlock <- IRB.block `IRB.named` "entry"

      forM_ (zip args argOs) $ \(name, arg) -> do
        a <- IRB.alloca doubleTy Nothing 0
        IRB.store a 0 arg
        lift $ assignvar name a

      codegenIR body >>= IRB.ret
    modify (\s -> s { functionTable = Map.insert name funcOperand (functionTable s) })
    return funcOperand
  Extern name args -> do
    extOperand <- IRB.extern (LLAST.Name (packShort name)) (fmap (const doubleTy) args) doubleTy
    modify (\s -> s { functionTable = Map.insert name extOperand (functionTable s) })
    return extOperand

packShort = BSS.toShort . BS.pack

passes :: LLPM.PassSetSpec
passes = LLPM.defaultCuratedPassSetSpec { LLPM.optLevel = Just 3 }

toAnon :: Phrase -> Codegen Defn
toAnon (DefnPhrase f) = return f
toAnon (ExprPhrase e) = do
  lastAnonId <- gets anonSupply
  let newAnonId = lastAnonId + 1
  modify (\s -> s { anonSupply = newAnonId })
  return (Function ("anon" ++ show newAnonId) [] e)

getLastAnon :: Codegen (Maybe String)
getLastAnon = do
  lastAnonId <- gets anonSupply
  return (if lastAnonId == 0 then Nothing else Just ("anon" ++ show lastAnonId))

buildLLModule :: [Phrase] -> Codegen LLAST.Module
buildLLModule phrases = do
    modDefs <- gets modDefinitions
    anonLabeledPhrases <- traverse toAnon phrases
    defs <- IRB.execModuleBuilderT IRB.emptyModuleBuilder (mapM_ codegenDefn anonLabeledPhrases)
    let updatedDefs = modDefs ++ defs
