module PrettyPrinter where

import BlindwormParser
import Data.Char (chr)
import Text.PrettyPrint
import qualified Text.PrettyPrint as P ((<>))

printAst :: Ast -> Doc
printAst (CharLiteral c) = quotes (char (chr c))
printAst (IntLiteral i) = int i
printAst (Variable s) = text s
printAst Pass = text "pass"
printAst (BinaryExpression op l r) =
  printAst l <+> printOperator op <+> printAst r
printAst (Assignment name value) = text name <+> equals <+> printAst value
printAst (FunctionDefinition name args body) =
  vcat
    [ text "def"
        <+> text name
        <+> parens (hsep (punctuate comma (map text args)))
        <+> lbrace
    , nest 4 (vcat (map printAstStatement body))
    , rbrace <+> text "\n"
    ]
printAst (FunctionCall name args) =
  text name P.<> parens (hsep (punctuate comma (map printAst args)))
printAst (Loop cond body) =
  vcat
    [ text "while" <+> parens (printAst cond) <+> lbrace
    , nest 4 (printBlock body)
    , rbrace <+> text "\n"
    ]
printAst (Condition cond thenBlock elseBlock) =
  let ifBlock =
        vcat
          [ text "if" <+> parens (printAst cond) <+> lbrace
          , nest 4 (printBlock thenBlock)
          , rbrace <+> text "\n"
          ]
   in case elseBlock of
        Nothing -> ifBlock
        Just elseBody ->
          vcat
            [ ifBlock
            , text "else" <+> lbrace
            , nest 4 (printBlock elseBody)
            , rbrace <+> text "\n"
            ]

printOperator :: AstOperator -> Doc
printOperator Add = char '+'
printOperator Subtract = char '-'
printOperator Multiply = char '*'
printOperator Divide = char '/'
printOperator Modulo = char '%'
printOperator GreaterThan = char '>'
printOperator LesserThan = char '<'

printBlock :: [Ast] -> Doc
printBlock statements = vcat (map printAstStatement statements)

printAstStatement :: Ast -> Doc
printAstStatement ast =
  case ast of
    FunctionDefinition {} -> printAst ast
    Loop {} -> printAst ast
    Condition {} -> printAst ast
    _ -> printAst ast P.<> semi

prettyPrintAst :: [Ast] -> IO ()
prettyPrintAst ast = putStrLn (render (vcat (map printAstStatement ast)))
