{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module BlindwormParser where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Identity as Data.Functor.Identity
import Data.Char (ord)

{- | A data type for tokens. `TBlanks` stores the size of the blank space,
 - because we need it to measure the indentation width. -}
data Tok
  = TInt Int
  | TNewLine
  | TBlanks Int
  | TString String
  | TIdent String
  | TChar Char
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

isNewLine :: T Tok -> Bool
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
tokenize :: String -> String -> Either (ParseErrorBundle String Void) TokStream
tokenize = runParser (TokStream <$> toks <* eof)

{- 
 - SECOND LEVEL:
 - ACTUAL PARSING OF SYNTAX
 -}
-- | The parser takes the tagged stream of tokens as an input
type Parser = Parsec Void TokStream

data AstOperator
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
  | BinaryExpression AstOperator Ast Ast
  | Condition Ast [Ast] (Maybe [Ast])
  | Loop Ast [Ast]
  | Pass
  deriving (Show)

-- | parse any amount of blanks
blanks :: ParsecT Void TokStream Data.Functor.Identity.Identity ()
blanks = void $ many (satisfy isBlank)
  where
    isBlank (TBlanks _) = True
    isBlank _ = False

-- | eat blanks after a given parse
pLexeme :: Parser a -> Parser a
pLexeme = (<* blanks)

parseCharLiteral :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseCharLiteral = do
  TChar c <- blanks *> satisfy isChar <?> "char"
  return $ CharLiteral (ord c)
  where
    isChar (TChar _) = True
    isChar _ = False

parseIntLiteral :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseIntLiteral = do
  TInt c <- blanks *> satisfy isInt <?> "int"
  return (IntLiteral c)
  where
    isInt (TInt _) = True
    isInt _ = False

parsePass :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parsePass = do 
  TIdent _ <- blanks *> satisfy (== TIdent "pass")
  return Pass

parseVariable :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseVariable = do
  TIdent name <- blanks *> satisfy isIdent
  return $ Variable name

parseLiteral :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseLiteral =
  choice [try parseIntLiteral, try parseCharLiteral, try parseVariable]

isIdent :: Tok -> Bool
isIdent (TIdent _) = True
isIdent _ = False

parseFuncArgs :: ParsecT Void TokStream Data.Functor.Identity.Identity [String]
parseFuncArgs =
  single TLeftParenthesis
    *> sepBy
         (satisfy isIdent >>= \(TIdent arg) -> return arg)
         (single (TIdent ","))
    <* single TRightParenthesis

parseIndentedBlock :: Pos -> Parser [Ast]
parseIndentedBlock ref = do
  _ <- satisfy (== TNewLine)
  some (parseBlockLine ref) <?> "code block cannot be empty - hint: use pass keyword"

sc :: Parser ()
sc = L.space (void $ satisfy isSpaceToken) empty empty
  where
    isSpaceToken (TBlanks _) = True
    isSpaceToken TNewLine = False
    isSpaceToken _ = False

parseBlockLine :: Pos -> Parser Ast
parseBlockLine ref = do
  _ <- L.indentGuard sc GT ref
  pLexeme (choice [try parseAssignment, parsePass, parseExpression])
    <* satisfy (== TNewLine)

parseOneLiner :: Parser [Ast]
parseOneLiner = do
  statement <- blindwormParser
  return [statement]

parseFuncDefinition :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseFuncDefinition = do
  ref <- L.indentLevel
  TIdent name <-
    blanks *> pLexeme (single (TIdent "def")) *> pLexeme (satisfy isIdent)
  args <- parseFuncArgs <* pLexeme (single TTwoDots)
  body <- try parseOneLiner <|> parseIndentedBlock ref
  return (FunctionDefinition name args body)

parseFuncCall :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseFuncCall = do
  TIdent name <- blanks *> satisfy isIdent
  args <-
    single TLeftParenthesis
      *> sepBy parseExpression (single (TIdent ","))
      <* single TRightParenthesis
  return (FunctionCall name args)


parseToken :: MonadParsec e s m => (Token s -> Maybe b) -> m b
parseToken f = satisfy (isJust . f) >>= \parsed_token -> pure (fromJust (f parsed_token))
  where
    isJust (Just _) = True
    isJust Nothing = False
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing - this should never happen"

parseAssignment :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseAssignment = do
  TIdent ident <- satisfy isIdent <* single (TOperator '=')
  Assignment ident <$> parseExpression

parseLoop :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseLoop = do
  ref <- L.indentLevel
  cond <- single (TIdent "while") *> parseExpression
  body <- pLexeme (single TTwoDots) *> parseIndentedBlock ref
  return (Loop cond body)

parseCondition :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
parseCondition = do
  ref <- L.indentLevel
  cond <- single (TIdent "if") *> parseExpression <* single TTwoDots
  thenBlock <- parseIndentedBlock ref
  elseBlock <-
    optional $ do
      _ <- L.indentGuard sc EQ ref
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

binary :: Char -> AstOperator -> Operator Parser Ast
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
  pLexeme (choice [try parseAssignment, parsePass, parseExpression])
    <* satisfy (== TNewLine)

blindwormParser :: ParsecT Void TokStream Data.Functor.Identity.Identity Ast
blindwormParser =
  choice
    [parseFuncDefinition, parseLoop, parseCondition, parseSingleLineStatement]

parseCode ::
     String -> TokStream -> Either (ParseErrorBundle TokStream Void) [Ast]
parseCode = runParser (many blindwormParser <* eof)

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