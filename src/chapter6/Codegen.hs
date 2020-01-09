{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Codegen
  ( Codegen,
    evalCodegen,
    codegenModule,
  )
where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short
import qualified Data.Map as Map
import Data.String
import qualified Data.Text.Lazy.IO as T
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as AST
import LLVM.IRBuilder as IRB
import qualified LLVM.PassManager as LLPM
import LLVM.Pretty
import Syntax
import Text.Pretty.Simple

-------------------------------------------------------------------------------
-- Code Generator Monad
-------------------------------------------------------------------------------

type Codegen = (StateT CodegenState IO)

data CodegenState
  = CodegenState
      { symbolTable :: Map.Map Name AST.Operand,
        functionTable :: Map.Map Name AST.Operand,
        modDefinitions :: [AST.Definition],
        nameSupply :: Word
      }

evalCodegen :: Codegen a -> IO a
evalCodegen = flip evalStateT emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { symbolTable = Map.empty,
    functionTable = Map.empty,
    modDefinitions = [],
    nameSupply = 0
  }

-- Everything is a double!
doubleTy :: AST.Type
doubleTy = AST.FloatingPointType AST.DoubleFP

printdTy :: [AST.Type] -> AST.Type
printdTy argtys = AST.PointerType (AST.FunctionType doubleTy argtys False) (AST.AddrSpace 0)

-------------------------------------------------------------------------------
-- Scoping
-------------------------------------------------------------------------------

getvar :: Name -> ModuleBuilderT Codegen AST.Operand
getvar name = do
  res <- Map.lookup name <$> gets symbolTable
  case res of
    Just x -> pure x
    Nothing -> error ("unknown variable: " ++ show name)

getfun :: Name -> [AST.Type] -> ModuleBuilderT Codegen AST.Operand
getfun name tys = do
  res <- Map.lookup name <$> gets symbolTable
  case res of
    Just x -> pure x
    Nothing -> pure (AST.ConstantOperand (C.GlobalReference (printdTy tys) name))

assignvar :: Name -> AST.Operand -> ModuleBuilderT Codegen ()
assignvar name var = modify (\s -> s {symbolTable = Map.insert name var (symbolTable s)})

-------------------------------------------------------------------------------
-- Code Generator
-------------------------------------------------------------------------------

codegen :: Expr -> IRBuilderT (ModuleBuilderT Codegen) AST.Operand
codegen = \case
  Float d -> pure (double d)
  Var name -> (lift $ getvar name) >>= flip load 0
  UnaryOp op e -> do
    codegen (Call (prefixName "unary" op) [e])
  BinaryOp "=" (Var var) val -> do
    i <- lift (getvar var)
    v <- codegen val
    store i 0 v
    return v
  BinaryOp op l r -> do
    let callWithOps f = join (f <$> codegen l <*> codegen r)
    case op of
      "+" -> callWithOps fadd
      "-" -> callWithOps fsub
      "*" -> callWithOps fmul
      "<" -> callWithOps (fcmp AST.ULT) >>= (\operand -> uitofp operand doubleTy)
      _ -> codegen (Call (prefixName "binary" op) [l, r])
  Call name args -> do
    largs <- traverse (fmap (,[]) . codegen) args
    lfun <- lift (getfun name (replicate (Prelude.length largs) doubleTy))
    call lfun largs
  If cond tr fl -> mdo
    condOp <- codegen cond
    let false = double 0
    test <- fcmp AST.ONE false condOp
    condBr test thenBlock elseBlock
    block `named` "if.then"
    trval <- codegen tr
    thenBlock <- currentBlock
    br contBlock
    block `named` "if.else"
    flval <- codegen fl
    elseBlock <- currentBlock
    br contBlock
    contBlock <- block `named` "if.cont"
    phi [(trval, thenBlock), (flval, elseBlock)]
  For ivar start cond step body -> mdo
    i <- alloca doubleTy Nothing 0 `named` "i"
    istart <- codegen start -- Generate loop variable initial value
    stepVal <- codegen step -- Generate loop variable step
    store i 0 istart
    lift $ assignvar ivar i
    br loopBlock
    loopBlock <- block `named` "for.loop"
    codegen body
    iCurrent <- load i 0
    iNext <- fadd iCurrent stepVal
    store i 0 iNext
    condOp <- codegen cond
    let falseOp = double 0
    test <- fcmp AST.ONE falseOp condOp
    condBr test loopBlock contBlock
    contBlock <- block `named` "for.cont"
    pure (double 0)
  Let var val body -> do
    i <- alloca doubleTy Nothing 0 `named` (unpackName var)
    v <- codegen val
    store i 0 v
    lift $ assignvar var i
    codegen body

codegenDefn :: Defn -> (ModuleBuilderT Codegen) AST.Operand
codegenDefn = \case
  UnaryDef name args body ->
    codegenDefn (Function (prefixName "unary" name) [args] body)
  BinaryDef name args body ->
    codegenDefn (Function (prefixName "binary" name) args body)
  Function name args body -> do
    funcOperand <- function
      name
      [(doubleTy, ParameterName (unpackName nm)) | nm <- args]
      doubleTy
      $ \argOs -> do
        entryBlock <- block `named` "entry"
        forM_ (zip args argOs) $ \(name, arg) -> do
          a <- alloca doubleTy Nothing 0
          store a 0 arg
          lift $ assignvar name a
        retval <- codegen body
        ret retval
    modify (\s -> s {functionTable = Map.insert name funcOperand (functionTable s)})
    return funcOperand
  Extern name args -> do
    extOperand <- extern name (fmap (const doubleTy) args) doubleTy
    modify (\s -> s {functionTable = Map.insert name extOperand (functionTable s)})
    return extOperand

-------------------------------------------------------------------------------
-- Name Handling
-------------------------------------------------------------------------------

prefixName :: String -> Name -> Name
prefixName pre (AST.Name nm) = AST.mkName (pre <> unpackBS nm)
prefixName pre (AST.UnName nm) = AST.mkName (pre <> show nm)

unpackBS :: ShortByteString -> String
unpackBS x = BS.unpack (fromShort x)

packShort :: String -> ShortByteString
packShort = toShort . BS.pack

unpackName :: AST.Name -> ShortByteString
unpackName (AST.Name nm) = nm

toAnon :: Phrase -> Codegen Defn
toAnon (DefnPhrase f) = return f
toAnon (ExprPhrase e) = do
  lastAnonId <- gets nameSupply
  let newAnonId = AST.UnName (lastAnonId + 1)
  modify (\s -> s {nameSupply = (lastAnonId + 1)})
  return (Function (prefixName "anon" newAnonId) [] e)

getLastAnon :: Codegen (Maybe String)
getLastAnon = do
  lastAnonId <- gets nameSupply
  return (if lastAnonId == 0 then Nothing else Just ("anon" ++ show lastAnonId))

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

passes :: LLPM.PassSetSpec
passes = LLPM.defaultCuratedPassSetSpec {LLPM.optLevel = Just 3}

codegenModule :: [Phrase] -> Codegen AST.Module
codegenModule phrases = do
  modDefs <- gets modDefinitions
  anonLabeledPhrases <- traverse toAnon phrases
  --liftIO (pPrint anonLabeledPhrases)
  defs <- IRB.execModuleBuilderT IRB.emptyModuleBuilder (mapM_ codegenDefn anonLabeledPhrases)
  let updatedDefs = modDefs ++ defs
  modify (\s -> s {modDefinitions = updatedDefs})
  mod <- IRB.buildModuleT (packShort "<stdin>") (traverse IRB.emitDefn updatedDefs)
  liftIO (T.putStrLn (ppllvm mod))
  pure mod
