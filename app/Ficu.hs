module Ficu where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Type.Coercion (sym)

{---------------- Grammar ----------------}
{--
let x = 1;
let y = "hello";
let z = {
  a: x,
  b: y,
  num: 3,
  str: 'world',
  bool: true,
  none: nil,
  arr: [1, 2, 3],
  obj: {a: 1, b: 2, c: 3}
};

let f = fn x {
  ret x + 1;
};
let g = fn x => x + 1;

let add = fn x y {
  let z = x + y;
  ret z;
};

let inc = fn x => add(1,y);

--}

{--
<str> ::= "'" <char>* "'"

<val> ::= <num> | <str> | <bool> | <nil>

<expr> ::= <id> | <val> | <access> | <fn> | <fn-call> | <obj> | <arr> | <tuple> | <bin-op> | "(" <expr> ")"

<obj> ::= "{" (<obj-field>("," <obj-field>)*)? "}"

<obj-field> ::= <id> ":" <expr> (",")*
<arr> ::= "[" (<expr>("," <expr>)*)? "]"
<tuple> ::= "(" (<expr>("," <expr>)*)? ")"

<bin-op> ::= <expr> <op> <expr>
<op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"

<fn> ::= "fn" <id>* <block> | "fn" <id>* "=>" <expr>

<fn-call> ::= <expr> "(" (<expr>("," <expr>)*)? ")"

<access> ::= <expr> "." <id> | <expr> "[" <expr> "]"

<obj-match-field> ::= <id> ":" <match-pattern> (",")*

<obj-pattern> ::= "{" (<obj-match-field>("," <obj-match-field>)*)? "}"
<arr-pattern> ::= "[" (<match-pattern>("," <match-pattern>)*)? "]"
<tuple-pattern> ::= "(" (<match-pattern>("," <match-pattern>)*)? ")"
<match-pattern> ::= <id> | <val> | <obj-pattern> | <arr-pattern> | <tuple-pattern>
<match-expr> ::= <match-pattern> "<-" <expr>

<let-stmt> ::= "let" <id> "=" <expr> <stmt-end>

<match-stmt> ::= "let" <match-pattern> "<-" <expr> <stmt-end>

<ret-stmt> ::= "ret" <expr> <stmt-end>

<if-stmt> ::= "if" <expr> <block> ("else" <block>)?

<if-match-stmt> ::= "if" <match-expr> <block> ("else" <block>)?

<stmt-end> ::= ";" (";")* <newline>*

<stmt> ::= <let-stmt> | <match-stmt> | <ret-stmt> | <if-match-stmt> | <if-stmt> | <expr> <stmt-end>

<block> ::= "{" <stmt>* "}"

<program> ::= <stmt>*
--}
{---------------- AST ----------------}

-- <val> ::= <num> | <str> | <bool> | <nil>
data Val
  = VNum Int
  | VStr String
  | VBool Bool
  | VNil
  deriving (Show, Eq)

type Ident = String

-- <expr> ::= <id> | <val> | <access> | <fn> | <fn-call> | <obj> | <arr> | <tuple> | <bin-op> | "(" <expr> ")"
data Expr
  = EId Ident
  | EVal Val
  | EAccess Access
  | EFn Fn
  | EFnCall FnCall
  | EObj Obj
  | EArr Arr
  | ETuple Tuple
  | EBinOp BinOp
  deriving (Show, Eq)

-- <obj> ::= "{" (<obj-field>("," <obj-field>)*)? "}"
-- <obj-field> ::= <id> ":" <expr> (",")*
type Obj = [(Ident, Expr)]

-- <arr> ::= "[" (<expr>("," <expr>)*)? "]"
type Arr = [Expr]

-- <tuple> ::= "(" (<expr>("," <expr>)*)? ")"
type Tuple = [Expr]

-- <bin-op> ::= <expr> <op> <expr>
-- <op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"
type Op = String
type BinOp = (Expr, Op, Expr)

-- <fn> ::= "fn" <id>* <block> | "fn" <id>* "=>" <expr>
data Fn = Fn [Ident] Block 
  deriving (Show, Eq)


-- <fn-call> ::= <expr> "(" (<expr>("," <expr>)*)? ")"
data FnCall = FnCall Expr [Expr]
  deriving (Show, Eq)


-- <access> ::= <expr> "." <id> | <expr> "[" <expr> "]"
data Access
  = AcDot Expr Ident
  | AcBra Expr Expr
  deriving (Show, Eq)


-- <obj-match-field> ::= <id> ":" <match-pattern> (",")*
data ObjMatchField 
  = ObjMatchFieldKey Ident 
  | ObjMatchFieldPair (Ident, MatchPattern)
  deriving (Show, Eq)

-- <obj-pattern> ::= "{" (<obj-match-field>("," <obj-match-field>)*)? "}"
type ObjPattern = [ObjMatchField]

-- <arr-pattern> ::= "[" (<match-pattern>("," <match-pattern>)*)? "]"
type ArrPattern = [MatchPattern]

-- <tuple-pattern> ::= "(" (<match-pattern>("," <match-pattern>)*)? ")"
type TuplePattern = [MatchPattern]

-- <match-pattern> ::= <id> | <val> | <obj-pattern> | <arr-pattern> | <tuple-pattern>
data MatchPattern
  = MPId Ident
  | MPVal Val
  | MPObj ObjPattern
  | MPArr ArrPattern
  | MPTuple TuplePattern
  deriving (Show, Eq)


-- <match-expr> ::= <match-pattern> "<-" <expr>
data MatchExpr = MatchExpr MatchPattern Expr
  deriving (Show, Eq)


-- <stmt> ::= <let-stmt> | <match-stmt> | <ret-stmt> | <if-match-stmt> | <if-stmt> | <expr> <stmt-end>
data Stmt
  = SLet Ident Expr                        -- <let-stmt> ::= "let" <id> "=" <expr> <stmt-end>
  | SMatch MatchPattern Expr               -- <match-stmt> ::= "let" <match-pattern> "<-" <expr> <stmt-end>
  | SRet Expr                              -- <ret-stmt> ::= "ret" <expr> <stmt-end>
  | SIfMatch (IF MatchExpr)                -- <if-match-stmt> ::= "if" <match-expr> <block> ("else" <block>)?
  | SIf (IF Expr)                          -- <if-stmt> ::= "if" <expr> <block> ("else" <block>)?
  | SExpr Expr
  deriving (Show, Eq)


-- <if-match-stmt>(e) ::= "if" e <block> ("else" <block>)?
data IF e = IF e Block (Maybe Block)
  deriving (Show, Eq)


-- <block> ::= "{" <stmt>* "}"
data Block = Block [Stmt]
  deriving (Show, Eq)


-- <program> ::= <stmt>*
data Program = Program [Stmt] 
  deriving (Show, Eq)

{---------------- Parser ----------------}

choices = choice . map try

pt = parseTest expr
pts = parseTest program
-- <val> ::= <num> | <str> | <bool> | <nil>
value :: Parser Val
value = do
  parserTrace "value"
  x <- choice [integer >>= return . VNum, VStr <$> stringLiteral, VBool <$> bool, symbol "nil" >> return VNil]
  return x

-- <expr> ::= <id> | <val> | <access> | <fn> | <fn-call> | <obj> | <arr> | <tuple> | <bin-op> | "(" <expr> ")"
expr :: Parser Expr
-- expr :: choices [exprId , exprVal, exprAccess, exprFn, exprFnCall, exprObj, exprArr, exprTuple, exprBinOp, exprParen]
expr = do 
  parserTrace "expr"
  
  -- (try object >>= return . EObj)
  -- (try array >>= return . EArr)
    -- <|> (try tuple >>= return . ETuple)
    -- <|> (try fnCall >>= return . EFnCall)
    -- <|> (try identifier >>= return . EId)
    -- <|> (try value >>= return . EVal)
    -- <|> (try fn >>= return . EFn)
    -- <|> expr1
  expr1
  
expr1 :: Parser Expr
expr1 = do
  parserTrace "expr1"
  try (binOp >>= return . EBinOp) <|> expr2

expr2 :: Parser Expr
expr2 = do
  parserTrace "expr2"
  try (object >>= return . EObj)
    <|> try (array >>= return . EArr)
    <|> try (tuple >>= return . ETuple)
    <|> expr3

expr3 :: Parser Expr
expr3 = do
  parserTrace "expr3"
  (try fnCall >>= return . EFnCall)
    <|> (try fn >>= return . EFn)
    <|> expr4


expr4 :: Parser Expr
expr4 = do
  parserTrace "expr4"
  try (exprNT EId identifier) 
    <|> try (exprNT EVal value) 
    <|> try (parens expr)
    <|> expr

-- <obj> ::= "{" (<obj-field>("," <obj-field>)*)? "}"
object :: Parser Obj
object = do
  parserTrace "object"
  try (do { symbol "{"; symbol "}"; return []}) <|> braces (commaSep objField)

-- <obj-field> ::= <id> ":" <expr> (",")*
objField :: Parser (Ident, Expr)
objField = do
  i <- identifier
  symbol ":"
  e <- expr1
  return (i, e)

exprNT :: (a -> Expr) -> Parser a -> Parser Expr
exprNT f p = f <$> p


-- <arr> ::= "[" (<expr>("," <expr>)*)? "]"
array :: Parser Arr
array = do 
  parserTrace "array"
  try (do { symbol "["; symbol "]"; return []}) <|> brackets (commaSep expr)

-- <tuple> ::= "(" (<expr>("," <expr>)*)? ")"
tuple :: Parser Tuple
tuple = do
  parserTrace "tuple"
  try (do { symbol "("; symbol ")"; return []}) <|> parens (commaSep expr)

-- <bin-op> ::= <expr> <op> <expr>
binOp :: Parser BinOp
binOp = binOp'
  where binOp' = do
          parserTrace "binOp1"
          left <- expr2
          parserTrace "binOp2"
          o <- op
          parserTrace "binOp3"
          right <- expr1
          return (left, o, right)

-- <op> ::= "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"
op :: Parser Op
op = do 
  parserTrace "op"
  s <- choice $ map symbol ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or"]
  return s

-- <fn> ::= "fn" <id>* <block> | "fn" <id>* "=>" <expr>
fn :: Parser Fn
fn = do
  reserved "fn"
  args <- many (lexeme identifier)
  body <- block <|> (symbol "=>" *> expr >>= \e -> return (Block [SRet e]))
  return (Fn args body)

-- <fn-call> ::= <expr> "(" (<expr>("," <expr>)*)
fnCall :: Parser FnCall
fnCall = do
  parserTrace "fnCall"
  f <- exprNT EId identifier
  args <- between (symbol "(") (symbol ")") (commaSep (try (exprNT EId identifier) <|> try (exprNT EVal value) <|> expr3))
  return (FnCall f args)

-- <access> ::= <expr> "." <id> | <expr> "[" <expr> "]"
access :: Parser Access
access = do
  e <- expr
  choices [ symbol "." >> identifier >>= return . AcDot e
         , brackets expr >>= return . AcBra e
         ]

-- <obj-match-field> ::= <id> ":" <match-pattern> (",")*
-- data ObjMatchField 
--   = ObjMatchFieldKey Ident 
--   | ObjMatchFieldPair (Ident, MatchPattern)
--   deriving (Show, Eq)
objMatchField :: Parser ObjMatchField
objMatchField = do
  key <- identifier
  choices [ ObjMatchFieldKey key <$ symbol ":"
         , ObjMatchFieldPair . (key,) <$> matchPattern
         ]

-- <obj-pattern> ::= "{" (<obj-match-field>("," <obj-match-field>)*)? "}"
objPattern :: Parser ObjPattern
objPattern = braces (commaSep objMatchField) <|> pure []

-- <arr-pattern> ::= "[" (<match-pattern>("," <match-pattern>)*)? "]"
arrPattern :: Parser ArrPattern
arrPattern = brackets (commaSep matchPattern) <|> pure []

-- <tuple-pattern> ::= "(" (<match-pattern>("," <match-pattern>)*)? ")"
tuplePattern :: Parser TuplePattern
tuplePattern = parens (commaSep matchPattern) <|> pure []


-- <match-pattern> ::= <id> | <val> | <obj-pattern> | <arr-pattern> | <tuple-pattern>
matchPattern :: Parser MatchPattern
matchPattern = choices
  [ MPId <$> identifier
  , MPVal <$> value
  , MPObj <$> objPattern
  , MPArr <$> arrPattern
  , MPTuple <$> tuplePattern
  ]

-- <match-expr> ::= <match-pattern> "<-" <expr>
matchExpr :: Parser MatchExpr
matchExpr = do
  ptn <- matchPattern
  symbol "<-"
  e <- expr
  return (MatchExpr ptn e)




-- <stmt> ::= <let-stmt> | <match-stmt> | <ret-stmt> | <if-match-stmt> | <if-stmt> | <expr> <stmt-end>
stmtEnd :: Parser ()
stmtEnd = return () -- semi <|> eof

stmt :: Parser Stmt
stmt = do
  parserTrace "stmt"
  stmt'
  where stmt' = try (letStmt) <|> try (matchStmt) <|> try (retStmt) <|> try (ifMatchStmt) <|> ifStmt
        letStmt = do
          reserved "let"
          i <- identifier
          symbol "="
          e <- expr
          return (SLet i e)
        matchStmt = do
          reserved "let"
          ptn <- matchPattern
          symbol "<-"
          e <- expr
          stmtEnd
          return (SMatch ptn e)
        retStmt = do
          reserved "ret"
          e <- expr
          stmtEnd
          return (SRet e)
        ifMatchStmt = do
          reserved "if"
          me <- matchExpr
          b <- block
          mb <- optionMaybe (reserved "else" >> block)
          return (SIfMatch (IF me b mb))
        ifStmt = do
          reserved "if"
          e <- expr
          b <- block
          mb <- optionMaybe (reserved "else" >> block)
          return (SIf (IF e b mb))

stmts :: Parser [Stmt]
stmts = stmt `endBy` semi

-- <block> ::= "{" <stmt>* "}"
block :: Parser Block
block = Block <$> braces (many stmt)

-- <program> ::= <stmt>*
program :: Parser Program
program = Program <$> many stmt

program' :: Parser Program
program' = lexeme program

parser :: String -> Either ParseError Program
parser = parse program ""



-- {---------------- Lexer ----------------}

lexer :: Token.TokenParser ()
lexer = lexer' { Token.stringLiteral = between (char '\'') (char '\'') (many (noneOf "'")) }
  where lexer' = Token.makeTokenParser (emptyDef
                  { Token.reservedNames = ["true", "false", "nil", "fn", "let", "ret", "if", "else"]
                  , Token.reservedOpNames = ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", ".", "=>", "<-", "="]
                  , Token.commentLine = "--"
                  }) 

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Int
integer = do 
  i <- Token.integer lexer
  return (fromInteger i)

bool :: Parser Bool
bool = choice [True <$ reserved "true", False <$ reserved "false"]

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

operator :: Parser String
operator = Token.operator lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer

comma :: Parser ()
comma = Token.comma lexer >> return ()

semi :: Parser ()
semi = Token.semi lexer >> return ()


lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer


{---------------- Pretty Printer ----------------}