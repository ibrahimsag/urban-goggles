module Main where

import System.Console.Haskeline

import Data.Functor.Identity(Identity)

import Control.Monad.Trans

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

-- # Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = emptyDef {
        Tok.commentLine = "#"
      , Tok.reservedOpNames = ["+", "*", "-", "/", ";", ",", "<"]
      , Tok.reservedNames = ["def", "extern"]
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

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- # AST

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | BinaryOp Name Expr Expr
  deriving (Eq, Ord, Show)

data Defn
  = Function Name [Name] Expr
  | Extern Name [Name]
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
        ]

int :: Parser Expr
int = do
  n <- integer
  return (Float (fromInteger n))

floating :: Parser Expr
floating = do
  n <- float
  return (Float n)

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
  var <- identifier
  return (Var var)

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens (many identifier)
  body <- expr
  return (Function name args body)

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens (many identifier)
  return (Extern name args)

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return (Call name args)

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens expr

defn :: Parser Expr
defn = try extern
   <|> try function
   <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

-- # RPPL

process :: [Expr] -> IO ()
process expr = mapM_ print expr

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = getInputLine "ready> " >>= \case
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ (either print process . parseToplevel) input) >> loop
