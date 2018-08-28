module Main where

import System.Console.Haskeline
import System.Environment
import Foreign.Ptr (FunPtr, castFunPtr)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Functor.Identity(Identity)
import Data.Maybe (maybe)

import Control.Monad.Trans
import Control.Monad.State.Strict

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import qualified LLVM.Exception as LLException
import qualified LLVM.ExecutionEngine as LLEE
import qualified LLVM.Analysis as LLysis
import qualified LLVM.PassManager as LLPM
import qualified LLVM.Context as LLContext
import qualified LLVM.Module as LLModule
import qualified LLVM.AST as LLAST
import qualified LLVM.AST.FloatingPointPredicate as LLAST
import qualified LLVM.IRBuilder as IRB

-- # Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
        Tok.commentLine = "#"
      , Tok.reservedOpNames = ["+", "*", "-", "/", ";", ",", "<"]
      , Tok.reservedNames = ["def", "extern", "if", "then", "else", "in", "for", "binary", "unary"]
      }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c:cs)

-- # AST

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  deriving (Eq, Ord, Show)

data Defn
  = Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryDef Name [Name] Expr
  | UnaryDef Name Name Expr
  deriving (Eq, Ord, Show)

data Phrase
  = DefnPhrase Defn
  | ExprPhrase Expr
  deriving (Eq, Ord, Show)

-- # Parser

binary
  :: String
  -> Ex.Assoc
  -> Ex.Operator String () Identity Expr
binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

table :: [[Ex.Operator String () Identity Expr]]
table = [ [ binary "*" Ex.AssocLeft
          , binary "/" Ex.AssocLeft
          ]
        , [ binary "+" Ex.AssocLeft
          , binary "-" Ex.AssocLeft
          ]
        , [ binary "<" Ex.AssocLeft
          , binary "<" Ex.AssocLeft
          ]
        , [ binary "=" Ex.AssocRight
          ]
        ]

int :: Parser Expr
int = do
  n <- integer
  return (Float (fromInteger n))

floating :: Parser Expr
floating = do
  n <- float
  return (Float n)

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

expr :: Parser Expr
expr = Ex.buildExpressionParser (table ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = do
  var <- identifier
  return (Var var)

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return (Call name args)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try ifthen
      <|> try for
      <|> variable
      <|> parens expr

binarydef :: Parser Defn
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens (many identifier)
  body <- expr
  return (BinaryDef o args body)

unarydef :: Parser Defn
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens (identifier)
  body <- expr
  return (UnaryDef o args body)

function :: Parser Defn
function = do
  reserved "def"
  name <- identifier
  args <- parens (many identifier)
  body <- expr
  return (Function name args body)

extern :: Parser Defn
extern = do
  reserved "extern"
  name <- identifier
  args <- parens (many identifier)
  return (Extern name args)

defn :: Parser Defn
defn = try extern
   <|> try function
   <|> try binarydef
   <|> try unarydef

phrase :: Parser Phrase
phrase = (DefnPhrase <$> try defn)
     <|> (ExprPhrase <$> try expr)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Phrase]
toplevel = many $ do
  def <- phrase
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Phrase]
parseToplevel s = parse (contents toplevel) "<stdin>" s

-- # Codegen

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

type Codegen =  (StateT CodegenState IO)

codegenIR :: Expr -> IRB.IRBuilderT (IRB.ModuleBuilderT Codegen) LLAST.Operand
codegenIR = \case
  Float d         -> IRB.double d
  Var name        -> maybe (error ("unknown variable: " ++ name)) id . Map.lookup name <$> gets symbolTable
  UnaryOp op e -> do
    codegenIR (Call ("unary" ++ op) [e])
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

    istart <- codegenIR start  -- Generate loop variable initial value
    stepVal <- codegenIR step  -- Generate loop variable step
    entryBlock' <- IRB.currentBlock
    IRB.br loopBlock

    loopBlock  <- IRB.block `IRB.named` "for.loop"
    i <- IRB.phi [(istart, entryBlock'), (iNext, loopBlock')]
    modify (\s -> s { symbolTable = Map.insert ivar i (symbolTable s) })

    codegenIR body
    iNext <- IRB.fadd i stepVal

    condOp <- codegenIR cond
    falseOp <- IRB.double 0
    test <- IRB.fcmp LLAST.ONE falseOp condOp
    loopBlock' <- IRB.currentBlock
    IRB.condBr test loopBlock contBlock

    contBlock  <- IRB.block `IRB.named` "for.cont"
    IRB.double 0

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
      localSymTable <- gets symbolTable
      let updateSymTableWithArgs symT = foldl'
                    (\symTable (name, arg) -> Map.insert name arg symTable)
                    symT
                    (zip args argOs)
      modify $ \s -> s { symbolTable = updateSymTableWithArgs (symbolTable s)}
      codegenIR body >>= IRB.ret
      modify $ \s -> s { symbolTable = localSymTable }
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
    modify (\s -> s { modDefinitions = updatedDefs } )
    IRB.buildModuleT (packShort "<stdin>") (traverse IRB.emitDefn updatedDefs)

printModulIR :: LLAST.Module -> IO ()
printModulIR modul = LLContext.withContext $ \ctx -> do
    LLModule.withModuleFromAST ctx modul $ \m -> do
      LLPM.withPassManager passes $ \pm -> do
        LLysis.verify m
        LLPM.runPassManager pm m
        llir <- LLModule.moduleLLVMAssembly m
        BS.putStrLn llir

-- # JIT

runJIT :: (Maybe String, LLAST.Module) -> IO ()
runJIT (lastAnonName, modul) = do
  LLContext.withContext $ \ctx -> do
    jit ctx $ \executionEngine ->
      LLModule.withModuleFromAST ctx modul $ \m -> do
        LLPM.withPassManager passes $ \pm -> do
          LLysis.verify m `catch` (\(LLException.VerifyException e) -> putStrLn e)
          LLPM.runPassManager pm m

          llir <- LLModule.moduleLLVMAssembly m
          BS.putStrLn llir

          LLEE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- maybe (return Nothing) (LLEE.getFunction ee . LLAST.mkName) lastAnonName
            maybe (return ()) (run >=> print . ("Evaluated to: " ++) . show) mainfn

jit :: LLContext.Context -> (LLEE.MCJIT -> IO a) -> IO a
jit c = LLEE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2     -- optimization level
    model    = Nothing    -- code model (Default)
    ptrelim  = Nothing    -- frame pointer elimination
    fastins  = Nothing    -- fast instruction selection

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

-- # REPL

process :: String -> Codegen ()
process = either (liftIO . print) (buildLLModule >=> (\m -> (, m) <$> getLastAnon) >=> liftIO . runJIT) . parseToplevel

interactive :: Codegen ()
interactive = runInputT defaultSettings loop
  where
    loop :: InputT Codegen ()
    loop = getInputLine "ready> " >>= maybe (outputStrLn "Goodbye.") ((>> loop) . lift . process)

evalWithEmptyCodegen :: Codegen () -> IO ()
evalWithEmptyCodegen = flip evalStateT emptyCodegen

main :: IO ()
main = getArgs >>= \case
  [] -> evalWithEmptyCodegen interactive
  filename :_ -> readFile filename >>= evalWithEmptyCodegen . process
