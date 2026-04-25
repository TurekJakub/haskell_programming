#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec
-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad (void)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Fixed (div')

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
        return $ T ident (TIdent ident),
     try $ do
        str <- char '"' *> manyTill L.charLiteral (char '"')
        return $ T str (TString str)
    , try ((T <$> id <*> TBlanks . length) <$> some (char ' '))
    , try ((\op -> T [op] (TOperator op))  <$> oneOf "+-></*%=")
    , T "(" TLeftParenthesis <$ char '('
    , T ")" TRightParenthesis <$ char ')'
    , T "\n" TNewLine <$ char '\n'
    , T ":" TTwoDots <$ char ':'
    , try((\op -> T [op] (TOperator op))  <$> (char '\'' *> anySingle <* char '\''))
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

data Ast = 
     CharLiteral Int 
    | IntLiteral Int
    | Variable String
    | FunctionDefinition String [String] [Ast]
    | FunctionCall String [Ast]
    | Assignment String Ast
    | BinaryExpression Char Ast Ast
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
  TChar c <- blanks *> satisfy isInt <?> "int"
  return $ CharLiteral c 
  where 
      isInt (TChar _) = True
      isInt _ = False 

parseVariable = do  
  TIdent name <- blanks *> satisfy isIdent
  return $ Variable name

parseLiteral = choice [try parseIntLiteral, try parseCharLiteral, try parseVariable]

isIdent (TIdent _) = True
isIdent _ = False

parseFuncArgs = single TLeftParenthesis *> sepBy (satisfy isIdent  >>= \(TIdent arg) -> return arg) (single (TIdent ",")) <* single TRightParenthesis 

parseFuncHeader = do 
  TIdent name <- blanks *> pLexeme (single (TIdent "def")) *> pLexeme (satisfy isIdent )
  args <- parseFuncArgs <* pLexeme (single TTwoDots) <* eof
  return (FunctionDefinition name args [])
      
parseFn = runParser parseFuncHeader

parseFuncCall = do
    TIdent name <- blanks *> satisfy isIdent
    args <- single TLeftParenthesis *> sepBy parseLiteral (single (TIdent ",")) <* single TRightParenthesis
    return (FunctionCall name args)

parseS = runParser (choice [
                try parseFuncCall,
  try parseLiteral,
                try parseIntLiteral])

-- parseCharLiteral = 

-- parseFnDef = single (TString "def") *> blanks
-- | a bit of demonstration
main = do
  let msg x = putStrLn $ "\n*** " ++ x ++ ": ***\n"
  msg "tokenizer output"
  let Right tokens = tokenize "input.txt" " test(x)"
  let Right a = parseS "index.p" tokens in
    print a
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
        rest = unLine (fst $ break isNewLine resttoks)
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
