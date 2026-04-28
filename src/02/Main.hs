#!/usr/bin/env cabal
{- cabal:
build-depends: base, parser-combinators,containers, megaparsec, pretty, mtl
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (guard, void, when, replicateM_)
import Control.Monad.Combinators.Expr
import Data.Bool (bool)
import Data.Char (chr)
import Data.Fixed (div')
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..), nub)
import Data.Void (Void)
import System.Console.GetOpt (ArgDescr(NoArg))
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Internal as LL
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (quotes)
import Text.Read (Lexeme(Char))
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State (StateT, modify, evalStateT, MonadState (get, put))
import Data.Containers.ListUtils (nubOrd)
import Debug.Trace (traceShow, trace)

{- | A data type for tokens. `TBlanks` stores the size of the blank space,
 - because we need it to measure the indentation width. -}
data Tok
  = TInt Int
  | TNewLine
  | TBlanks Int
  | TString String
  | TIdent String
  | TChar Int
  | TOperator Char
  | TLeftParenthesis
  | TRightParenthesis
  | TTwoDots
  deriving (Show, Eq, Ord)

-- | We use this function to show the tokens in error messages from the parser,
-- such as "unexpected TNewLine". If you want better error messages (such as
-- "unexpected line ending" :D ), modify this function first.
showTok :: Tok -> String
showTok = show

{- | Because of the need to have sensible source-related error messages from
 - the SECOND level of parsing, we will need to reconstruct the actual original
 - input of the FIRST level of parsing so that we can point into it for
 - highlighting the error location.
 -
 - So, in short, we save the whole intact original string in the token `T`.
 - With more token types, we'd have:
 -
 - T "a" (TIdentifier "a")
 - T "0x123" (TInt 291)
 - T "\"a s d\"" (TString "a s d")
 - T "\t\t  " (TBlanks 18)
 -}
data T a = T
  { strT :: String -- ^ discards the data and returns the original string
  , unT :: !a -- ^ discards the wrap and returns the data
  } deriving (Show)

isNewLine (T _ TNewLine) = True
isNewLine _ = False

{-
 - FIRST LEVEL:
 - LEXING/TOKENIZATION
 -}
-- | The tokenizer eats normal Strings
type Tokenizer = Parsec Void String

-- | This parses out all tokens (you might want to extend the token count)
tok :: Tokenizer (T Tok)
tok =
  choice
    [ try ((T <$> id <*> TInt . read) <$> some digitChar)
    , try $ do
        ident <- some (alphaNumChar <|> char '_')
        return $ T ident (TIdent ident)
    , try $ do
        str <- char '"' *> manyTill L.charLiteral (char '"')
        return $ T str (TString str)
    , try ((T <$> id <*> TBlanks . length) <$> some (char ' '))
    , try ((\op -> T [op] (TOperator op)) <$> oneOf "+-></*%=")
    , T "(" TLeftParenthesis <$ char '('
    , T ")" TRightParenthesis <$ char ')'
    , T "\n" TNewLine <$ char '\n'
    , T ":" TTwoDots <$ char ':'
    , try
        ((\op -> T [op] (TOperator op))
           <$> (char '\'' *> anySingle <* char '\''))
    ]

toks :: Tokenizer [T Tok]
toks = many tok

-- | A nice wrapper for the token stream (we'll need to make instances on this,
-- so we want to have a type tag, not just a list alias).
newtype TokStream = TokStream
  { unTokStream :: [T Tok]
  } deriving (Show)

-- | This runs the tokenizer
tokenize = runParser (TokStream <$> toks <* eof)

{- 
 - SECOND LEVEL:
 - ACTUAL PARSING OF SYNTAX
 -}
-- | The parser takes the tagged stream of tokens as an input
type Parser = Parsec Void TokStream

data NOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | GreaterThan
  | LesserThan
  deriving (Show, Eq)

data Ast
  = CharLiteral Int
  | IntLiteral Int
  | Variable String
  | FunctionDefinition String [String] [Ast]
  | FunctionCall String [Ast]
  | Assignment String Ast
  | BinaryExpression NOperator Ast Ast
  | Condition Ast [Ast] (Maybe [Ast])
  | Loop Ast [Ast]
  | Pass
  deriving (Show)

-- | parse any amount of blanks
blanks = void $ many (satisfy isBlank)
  where
    isBlank (TBlanks _) = True
    isBlank _ = False

-- | parse a single integer
pInt = do
  {- the line below carries an additional label for error messages (this allows
   - the parser to print stuff like "expected an integer") -}
  TInt i <- satisfy isInt <?> "an integer"
  return i
  where
    isInt (TInt _) = True
    isInt _ = False

-- | eat blanks after a given parse
pLexeme :: Parser a -> Parser a
pLexeme = (<* blanks)

-- | eat 2 integers and a newline
pairLine = (,) <$> pLexeme pInt <*> pLexeme pInt <* pLexeme (single TNewLine)

-- | parse lots of integer pairs, each on a single line
parsePairs = runParser (blanks *> many pairLine <* eof)

parseCharLiteral = do
  TChar c <- blanks *> satisfy isChar <?> "char"
  return $ CharLiteral c
  where
    isChar (TChar _) = True
    isChar _ = False

parseIntLiteral = do
  TInt c <- blanks *> satisfy isInt <?> "int"
  return (IntLiteral c)
  where
    isInt (TInt _) = True
    isInt _ = False

parseVariable = do
  TIdent name <- blanks *> satisfy isIdent
  return $ Variable name

parseLiteral =
  choice [try parseIntLiteral, try parseCharLiteral, try parseVariable]

isIdent (TIdent _) = True
isIdent _ = False

parseFuncArgs =
  single TLeftParenthesis
    *> sepBy
         (satisfy isIdent >>= \(TIdent arg) -> return arg)
         (single (TIdent ","))
    <* single TRightParenthesis

parseIndentedBlock :: Pos -> Parser [Ast]
parseIndentedBlock ref = do
  _ <- satisfy (== TNewLine)
  many (parseBlockLine ref)

sc :: Parser ()
sc = L.space (void $ satisfy isSpaceToken) empty empty
  where
    isSpaceToken (TBlanks _) = True
    isSpaceToken TNewLine = False
    isSpaceToken _ = False

parseBlockLine :: Pos -> Parser Ast
parseBlockLine ref = do
  L.indentGuard sc GT ref
  pLexeme (choice [try parseAssignment, parseExpression])
    <* satisfy (== TNewLine)

parseOneLiner :: Parser [Ast]
parseOneLiner = do
  statement <- blindwormParser
  return [statement]

parseFuncDefinition = do
  ref <- L.indentLevel
  TIdent name <-
    blanks *> pLexeme (single (TIdent "def")) *> pLexeme (satisfy isIdent)
  args <- parseFuncArgs <* pLexeme (single TTwoDots)
  body <- try parseOneLiner <|> parseIndentedBlock ref
  return (FunctionDefinition name args body)

parseFuncCall = do
  TIdent name <- blanks *> satisfy isIdent
  args <-
    single TLeftParenthesis
      *> sepBy parseExpression (single (TIdent ","))
      <* single TRightParenthesis
  return (FunctionCall name args)

parseOperator = do
  blanks *> satisfy isOp
  where
    isOp (TOperator _) = True
    isOp _ = False

parseToken f = satisfy (isJust . f) >>= \tok -> pure (fromJust (f tok))
  where
    isJust (Just _) = True
    isJust Nothing = False
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing - this should never happen"

parseAssignment = do
  TIdent ident <- satisfy isIdent <* single (TOperator '=')
  Assignment ident <$> parseExpression

parseLoop = do
  ref <- L.indentLevel
  cond <- single (TIdent "while") *> parseExpression
  body <- pLexeme (single TTwoDots) *> parseIndentedBlock ref
  return (Loop cond body)

parseCondition = do
  ref <- L.indentLevel
  cond <- single (TIdent "if") *> parseExpression <* single TTwoDots
  thenBlock <- parseIndentedBlock ref
  elseBlock <-
    optional $ do
      L.indentGuard sc EQ ref
      _ <- single (TIdent "else") *> single TTwoDots
      parseIndentedBlock ref
  return (Condition cond thenBlock elseBlock)

parseTerm :: Parser Ast
parseTerm =
  pLexeme
    $ choice
        [ between
            (pLexeme (single TLeftParenthesis))
            (pLexeme (single TRightParenthesis))
            parseExpression
        , try parseFuncCall
        , parseLiteral
        ]

parseExpression :: Parser Ast
parseExpression = makeExprParser parseTerm opTable

binary :: Char -> NOperator -> Operator Parser Ast
binary opChar astConstructor =
  InfixL (BinaryExpression astConstructor <$ pOperator opChar)
  where
    pOperator c =
      pLexeme
        $ parseToken $ \case
        TOperator op
          | op == c -> Just ()
        _ -> Nothing

opTable :: [[Operator Parser Ast]]
opTable =
  [ [binary '*' Multiply, binary '/' Divide, binary '%' Modulo]
  , [binary '+' Add, binary '-' Subtract]
  , [binary '>' GreaterThan, binary '<' LesserThan]
  ]

parseSingleLineStatement :: Parser Ast
parseSingleLineStatement =
  pLexeme (choice [try parseAssignment, parseExpression])
    <* satisfy (== TNewLine)

blindwormParser =
  choice
    [parseFuncDefinition, parseLoop, parseCondition, parseSingleLineStatement]

parseCode ::
     String -> TokStream -> Either (ParseErrorBundle TokStream Void) [Ast]
parseCode = runParser (many blindwormParser <* eof)

-- | a bit of demonstration
main2 = do
  let msg x = putStrLn $ "\n*** " ++ x ++ ": ***\n"
  msg "tokenizer output"
  let Right tokens = tokenize "input.txt" "if i<j:\n j=a+2\nelse:\n j=a+5\n" --"while i<j:\n test(i)\n i=i+1\n"
  let Right a = parseCode "index.p" tokens
   in print a
  let Right err = tokenize "input.txt" "0 1 \n   four 5\n"
  msg "parser output"
  print err
  msg "parser output"
  let Right exprs = parsePairs "input.txt" tokens
  print exprs
  let Left err =
        parsePairs
          "input.txt"
          (TokStream
             $ [T "123" (TInt 123), T " " (TBlanks 1)] ++ unTokStream tokens)
  msg "error message example"
  putStrLn $ errorBundlePretty err
  let Left err =
        parsePairs
          "input.txt"
          (TokStream
             $ unTokStream tokens ++ [T " " (TBlanks 1), T "123" (TInt 123)])
  msg "another error message with line counting"
  putStrLn $ errorBundlePretty err
  let Right tokens = tokenize "input.txt" "1 1 \n\n 2  3 3  3\n5\n"
  msg "tokens for the demo below"
  print tokens
  let Left err = parsePairs "input.txt" tokens
  msg "error message that handles empty line"
  putStrLn $ errorBundlePretty err

opStr Add         = "+"
opStr Subtract    = "-"
opStr Multiply    = "*"
opStr Divide      = "/"
opStr Modulo      = "%"
opStr GreaterThan = ">"
opStr LesserThan  = "<"

type StackOffset = Int
data Env = Env {globals :: Map String StackOffset, locals :: Map String StackOffset}
type CodeGen = ReaderT Env (StateT StackOffset (Writer [String])) ()

emit :: Ast -> CodeGen

emit (IntLiteral n) = do
    modify (+1)
    tell [show n]

emit (Variable name) = do
    Env gbls lcls <- ask
    depth <- get
    let lookupLocal =
          fmap (\slot -> depth - 1 - slot) (Map.lookup name  lcls)
        lookupGlobal =
          fmap (\slot -> depth - 1 - slot) (Map.lookup name  gbls)
    let offset = case lookupLocal <|> lookupGlobal of
                  Just ofs -> ofs
                  Nothing -> error $ "Unbound variable: " ++ name
    modify (+1)
    tell [show offset, "peek"]


emit (Assignment name rhs) = do                  
    Env gbls lcls <- ask
    depth <- get
    emit rhs 
    let a = trace (show depth) depth
    let lookupLocal =
          fmap (\slot ->  trace ("") depth - 1 - slot) (Map.lookup name lcls)
        lookupGlobal =
          fmap (\slot ->  trace ("") depth - 1 - slot) (Map.lookup name gbls)
    let offset = case lookupLocal <|> lookupGlobal of
                  Just ofs -> ofs
                  Nothing -> error $ "Unknown variable: " ++ name
    modify(\n -> n -1)
    tell [show offset]
    tell ["poke", "0"]

emit (BinaryExpression op a b) = do
    emit a
    emit b
    modify (\n -> n - 1)
    tell [opStr op]

emit (FunctionCall fn args) = do
    mapM_ emit args
    st <- get
    modify (\n -> n - length args + 1)
    tell [fn]

emit Pass = do
    modify (+1)
    tell ["0"]

emit (FunctionDefinition fname args body) = do
    tell [":", fname]
    Env gbls _ <- ask
    oldDepth <- get
    let localVar = collectGlobals body
    let lcls = Map.fromList $ zip (args++localVar) [oldDepth..]
    let funEnv = Env {globals = gbls, locals = lcls}
    modify (+ length args)
    replicateM_  (length  localVar) $ emit  $ IntLiteral 0 
    local (const funEnv) $ mapM_ emit body
    put (oldDepth)
    tell [";"]

emit (Condition cond thenB elseB) = do
    env <- ask
    depth <- get
    let estimate code =
          execWriter $ evalStateT (runReaderT (mapM_ emit code) env) depth
    let thenLength = length (estimate thenB)
    let elseLength = maybe 0 (length . estimate) elseB
    let elseJump = show (thenLength + 2)
    let endJump  = show (elseLength + 1)
    emit cond
    tell ["?branch", elseJump]
    mapM_ emit thenB
    case elseB of
      Nothing   -> do
        modify (+1)
        tell ["0"]
      Just eb -> do
        tell ["branch", endJump]
        mapM_ emit eb

emit (Loop cond body) = do
    env <- ask
    depth <- get
    let estimate code =
            execWriter $ evalStateT (runReaderT (mapM_ emit code) env) depth
    let bodyLen = length (estimate body)
    let condLen = length (estimate [cond])
    let jumpOut = show (bodyLen + 4)
    let jumpBack = show (-(bodyLen + condLen + 2))
    emit cond
    tell ["?branch", jumpOut]
    mapM_ emit body
    tell ["branch", jumpBack]
    modify (+1) -- dummy value
    tell ["0"]

collectGlobals :: [Ast] -> [String]
collectGlobals = go []
  where
    go acc [] = acc
    go acc (Assignment name _:rest) = go (name:acc) rest
    go acc (FunctionDefinition _ _ _ :rest) = go acc rest
    go acc (Loop _ body :rest) = go (go acc body) rest
    go acc (Condition _ tb (Just eb) :rest) = go (go (go acc tb) eb) rest
    go acc (Condition _ tb Nothing :rest) = go (go acc tb) rest
    go acc (_:rest) = go acc rest

emitProgram :: [Ast] -> [String]
emitProgram asts =
  let globalsList = nubOrd $ collectGlobals asts     
      gblMap = Map.fromList $ zip globalsList [0..]
      allocations = replicate (Map.size gblMap) "0"
      env = Env gblMap Map.empty
      code = execWriter $ evalStateT (runReaderT (mapM_ emit asts) env) (Map.size gblMap)
  in allocations ++ code

main = do
  args <- getArgs
  when (null args) $ die "no input file supplied"
  let input = head args
  contents <- readFile input
  case tokenize input contents of
    Right tokens ->
      case parseCode input tokens of
        Right res ->  let hrotfCode = emitProgram res in
                      mapM_ putStrLn hrotfCode
        Left err -> putStrLn $ errorBundlePretty err
    Left err -> print err

-- | This is a megaparsec Stream instance for our `TokStream`, which works as
-- an adapter between our lists of labeled tokens and megaparsec. Essentially,
-- it tells megaparsec how to consume the TokStream. Similar instances exist
-- for String, Text, ByteString, and other parser-input types.
--
-- Essentially, megaparsec is able to parse anything as long as it has the
-- Stream instance defined.
instance Stream TokStream where
  type Token TokStream = Tok -- token type that the parsing function is interested in
  type Tokens TokStream = [Tok] -- type for "several tokens"
  tokenToChunk _ = (: []) -- some conversion functions
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokStream (x:xs)) = Just (unT x, TokStream xs) -- extracts a token
  take1_ _ = Nothing
  takeN_ n (TokStream l@(_:_)) = Just (map unT $ take n l, TokStream $ drop n l) -- extracts a chunk
  takeN_ _ _ = Nothing
  takeWhile_ f (TokStream l) =
    (map unT $ takeWhile (f . unT) l, TokStream $ dropWhile (f . unT) l)

-- | THIS BELOW you don't usually want to read.
--
-- This is the "rest" of the TokStream instances that is required for
-- megaparsec to be able to reconstruct good error messages from our TokStream.
--
-- In particular, whole the `Stream` instance tells megaparsec how to get
-- values out of the stream, the VisualStream tells it how to show the tokens
-- to the user in case some token is e.g. known to be expected but missing...
instance VisualStream TokStream where
  showTokens _ (a :| b) = intercalate ", " $ map showTok (a : b)

-- | ...and the TraversableStream instance tells it how to walk the token
-- stream in such a way that reconstructing a good "example source" of where
-- the error has occured is easy.
instance TraversableStream TokStream where
  reachOffset o pst =
    let (reachtoks, resttoks) =
          splitAt (o - pstateOffset pst) . unTokStream $ pstateInput pst
        (rln, rst) = break isNewLine (reverse reachtoks)
        linesFinished = length . filter isNewLine $ rst
        sameLine = linesFinished == 0
        line = unLine (reverse rln)
        rest = unLine (takeWhile (not . isNewLine) resttoks)
        unLine = unTab . concatMap strT
        unTab "" = ""
        unTab ('\t':cs) = replicate (unPos $ pstateTabWidth pst) ' ' ++ unTab cs
        unTab (c:cs) = c : unTab cs
        unempty "" = "<empty line>"
        unempty a = a
        pfx = bool id (pstateLinePrefix pst ++) sameLine
        sp = pstateSourcePos pst
        col = length line + bool 1 (unPos $ sourceColumn sp) sameLine
        row = unPos (sourceLine sp) + linesFinished
     in ( Just . unempty $ pfx line ++ rest
        , pst
            { pstateInput = TokStream resttoks
            , pstateOffset = o
            , pstateSourcePos =
                sp {sourceLine = mkPos row, sourceColumn = mkPos col}
            , pstateLinePrefix = pfx line
            })

printAst :: Ast -> P.Doc
printAst (CharLiteral c) = quotes (P.char (chr c))
printAst (IntLiteral i) = P.int i
printAst (Variable s) = P.text s
printAst Pass = P.text "pass"
printAst (BinaryExpression op l r) =
  printAst l <> printOperator op <> printAst r
printAst (Assignment name value) = P.text name <> P.equals <> printAst value
printAst (FunctionDefinition name args body) =
  P.vcat
    [ P.text "def"
        P.<+> P.text name
                P.<> P.parens (P.hsep (P.punctuate P.comma (map P.text args)))
        P.<+> P.lbrace
    , P.nest 4 (P.vcat (map printAstStatement body))
    , P.rbrace <> P.text "\n"
    ]
printAst (FunctionCall name args) =
  P.text name <> P.parens (P.hsep (P.punctuate P.comma (map printAst args)))
printAst (Loop cond body) =
  P.vcat
    [ P.text "while" P.<+> P.parens (printAst cond) P.<+> P.lbrace
    , P.nest 4 (printBlock body)
    , P.rbrace <> P.text "\n"
    ]
printAst (Condition cond thenBlock elseBlock) =
  let ifBlock =
        P.vcat
          [ P.text "if" <> printAst cond <> P.colon
          , P.nest 4 (printBlock thenBlock)
          ]
   in case elseBlock of
        Nothing -> ifBlock
        Just elseBody ->
          P.vcat
            [ifBlock, P.text "else" <> P.colon, P.nest 4 (printBlock elseBody)]

printOperator :: NOperator -> P.Doc
printOperator Add = P.char '+'
printOperator Subtract = P.char '-'
printOperator Multiply = P.char '*'
printOperator Divide = P.char '/'
printOperator Modulo = P.char '%'
printOperator GreaterThan = P.char '>'
printOperator LesserThan = P.char '<'

printBlock :: [Ast] -> P.Doc
printBlock statements = P.vcat (map printAstStatement statements)

printAstStatement :: Ast -> P.Doc
printAstStatement ast =
  case ast of
    FunctionDefinition {} -> printAst ast
    Loop {} -> printAst ast
    Condition {} -> printAst ast
    _ -> printAst ast <> P.semi

prettyPrintAst ast = putStrLn (P.render (P.vcat (map printAstStatement ast)))
