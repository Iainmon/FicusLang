module Ficu where

import Text.Parsec 
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Type.Coercion (sym)
import Data.List (intercalate)

import System.IO.Unsafe (unsafePerformIO)

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
  deriving Eq

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
  deriving Eq

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
  deriving Eq


-- <fn-call> ::= <expr> "(" (<expr>("," <expr>)*)? ")"
data FnCall = FnCall Expr [Expr]
  deriving Eq


-- <access> ::= <expr> "." <id> | <expr> "[" <expr> "]"
data Access
  = AcDot Expr Ident
  | AcBra Expr Expr
  deriving Eq


-- <obj-match-field> ::= <id> ":" <match-pattern> (",")*
data ObjMatchField 
  = ObjMatchFieldKey Ident 
  | ObjMatchFieldPair (Ident, MatchPattern)
  deriving Eq

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
  deriving Eq


-- <match-expr> ::= <match-pattern> "<-" <expr>
data MatchExpr = MatchExpr MatchPattern Expr
  deriving Eq


-- <stmt> ::= <let-stmt> | <match-stmt> | <ret-stmt> | <if-match-stmt> | <if-stmt> | <expr> <stmt-end>
data Stmt
  = SLet Ident Expr                        -- <let-stmt> ::= "let" <id> "=" <expr> <stmt-end>
  | SMatch MatchPattern Expr               -- <match-stmt> ::= "let" <match-pattern> "<-" <expr> <stmt-end>
  | SRet Expr                              -- <ret-stmt> ::= "ret" <expr> <stmt-end>
  | SIfMatch (IF MatchExpr)                -- <if-match-stmt> ::= "if" <match-expr> <block> ("else" <block>)?
  | SIf (IF Expr)                          -- <if-stmt> ::= "if" <expr> <block> ("else" <block>)?
  | SExpr Expr
  deriving Eq


-- <if-match-stmt>(e) ::= "if" e <block> ("else" <block>)?
data IF e = IF e Block (Maybe Block)
  deriving Eq


-- <block> ::= "{" <stmt>* "}"
data Block = Block [Stmt]
  deriving Eq


-- <program> ::= <stmt>*
data Program = Program [Stmt] 
  deriving Eq

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
  try (do { symbol "{"; symbol "}"; return []}) <|> braces (commaSep (whiteSpace >> objField))

-- <obj-field> ::= <id> ":" <expr> (",")*
objField :: Parser (Ident, Expr)
objField = do
  i <- identifier
  symbol ":"
  e <- try (exprNT EId identifier) <|> try (exprNT EVal value) <|> expr
  whiteSpace
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
  args <- try (do { symbol "("; symbol ")"; return []}) <|> parens (commaSep expr) -- between (symbol "(") (symbol ")") (commaSep (try (exprNT EId identifier) <|> try (exprNT EVal value) <|> expr3))
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
--   deriving Eq
objMatchField :: Parser ObjMatchField
objMatchField = do
  key <- identifier
  choices [ ObjMatchFieldKey key <$ symbol ":"
         , ObjMatchFieldPair . (key,) <$> matchPattern
         ]

-- <obj-pattern> ::= "{" (<obj-match-field>("," <obj-match-field>)*)? "}"
objPattern :: Parser ObjPattern
objPattern = try (do { symbol "{"; symbol "}"; return []}) <|> braces (commaSep objMatchField)

-- <arr-pattern> ::= "[" (<match-pattern>("," <match-pattern>)*)? "]"
arrPattern :: Parser ArrPattern
arrPattern = try (do { symbol "["; symbol "]"; return []}) <|> brackets (commaSep matchPattern)

-- <tuple-pattern> ::= "(" (<match-pattern>("," <match-pattern>)*)? ")"
tuplePattern :: Parser TuplePattern
tuplePattern = try (do { symbol "("; symbol ")"; return []}) <|> parens (commaSep matchPattern)


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
  where stmt' = try (letStmt) <|> try (matchStmt) <|> try (retStmt) <|> try ifStmt <|> try ifMatchStmt <|> exprStmt
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
          s <- getInput
          setInput (';':s)
          return (SIfMatch (IF me b mb))
        ifStmt = do
          reserved "if"
          e <- expr
          b <- block
          mb <- optionMaybe (reserved "else" >> block)
          s <- getInput
          setInput (';':s)
          return (SIf (IF e b mb))
        exprStmt = do
          e <- fnCall <|> fail "exprStmt"
          return (SExpr (EFnCall e))

stmtSep :: Parser ()
stmtSep = do
  _ <- many1 (whiteSpace >> semi >> whiteSpace)
  return ()
stmts :: Parser [Stmt]
stmts = do
  whiteSpace
  ss <- stmt `endBy` stmtSep
  whiteSpace
  return ss

-- <block> ::= "{" <stmt>* "}"
block :: Parser Block
block = Block <$> braces stmts

-- <program> ::= <stmt>*
program :: Parser Program
program = Program <$> stmts

program' :: Parser Program
program' = lexeme program

parser :: String -> Either ParseError Program
parser = parse program ""


{-# INLINE source #-}
source :: FilePath -> String
source fn = unsafePerformIO (readFile fn)

{-# INLINE run #-}
run :: IO ()
run = do
  s <- readFile "prog.ficu"
  x <- parseTest program s
  print x
  let res = id $! parser s
  case res of
    Left _  -> print ()
    Right _ -> print ()
  putStrLn "------------------ parser output ------------------"
  putStrLn ""
  case res of
    Left err -> print err
    Right p -> putStrLn $ pprint p




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

instance Show Val where
  show (VNum i) = show i
  show (VStr s) = "'" ++ s ++ "'"
  show (VBool b) = show b
  show VNil = "nil"

instance Show Expr where
  show (EId i) = i
  show (EVal v) = show v
  show (EAccess a) = show a
  show (EFn f) = show f
  show (EFnCall f) = show f
  show (EObj o) = showObj o
  show (EArr a) = showArr a
  show (ETuple t) = showTuple t
  show (EBinOp b) = showBinOp b

showObj o = "{ " ++ intercalate ", " (map showObjField o) ++ " }"
  where showObjField (i, e) = i ++ ": " ++ show e
showArr a = "[" ++ intercalate "," (map show a) ++ "]"
showTuple t = "(" ++ intercalate "," (map show t) ++ ")"

showBinOp (e1, op, e2) = show e1 ++ " " ++ op ++ " " ++ show e2

instance Show Fn where
  show (Fn args block) = "fn " ++ unwords args ++ " " ++ show block

instance Show FnCall where
  show (FnCall e args) = show e ++ "(" ++ intercalate "," (map show args) ++ ")"

instance Show Access where
  show (AcDot e i) = show e ++ "." ++ i
  show (AcBra e1 e2) = show e1 ++ "[" ++ show e2 ++ "]"


instance Show ObjMatchField where
  show (ObjMatchFieldKey i) = i
  show (ObjMatchFieldPair (i, p)) = i ++ ": " ++ show p

instance Show MatchPattern where
  show (MPId i) = i
  show (MPVal v) = show v
  show (MPObj o) = showObjMatchPattern o
  show (MPArr a) = showArrMatchPattern a
  show (MPTuple t) = showTupleMatchPattern t

showObjMatchPattern o = "{ " ++ intercalate ", " (map show o) ++ " }"
showArrMatchPattern a = "[" ++ intercalate "," (map show a) ++ "]"
showTupleMatchPattern t = "(" ++ intercalate "," (map show t) ++ ")"

instance Show MatchExpr where
  show (MatchExpr p e) = show p ++ " <- " ++ show e

instance Show Stmt where
  show (SLet i e) = "let " ++ i ++ " = " ++ show e
  show (SMatch p e) = "let " ++ show p ++ " <- " ++ show e
  show (SRet e) = "ret " ++ show e
  show (SIfMatch (IF e b1 b2)) = "if " ++ show e ++ " " ++ show b1 ++ showElse b2
  show (SIf (IF e b1 b2)) = "if " ++ show e ++ " " ++ show b1 ++ showElse b2
  show (SExpr e) = show e

showElse Nothing = ""
showElse (Just b) = " else " ++ show b


intercalate1 :: String -> [String] -> String
intercalate1 _ [] = ""
intercalate1 s [x] = x ++ s
intercalate1 s (x:xs) = x ++ s ++ intercalate1 s xs

instance Show Block where
  show (Block stmts) = "{\n" ++ intercalate1 ";\n" (map show stmts) ++ "}"

instance Show Program where
  show (Program stmts) = intercalate1 ";\n" (map show stmts)


pprint :: Program -> String
pprint = format 0 . lines . show
  where format _ [] = ""
        format n (x:xs) | ('{':_) <- reverse x = concat (replicate n spacer) ++ x ++ "\n" ++ format (n+1) xs
                        | ('}':_) <- x = concat (replicate (n-1) spacer) ++ x ++ "\n" ++ format (n-1) xs
                        | otherwise = concat (replicate n spacer) ++ x ++ "\n" ++ format n xs
        spacer = "  "
{---------------- Interpreter ----------------}