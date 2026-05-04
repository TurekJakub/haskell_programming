import Control.Monad (replicateM_)
import Control.Monad.Reader
import Control.Monad.State (MonadState(get, put), StateT, evalStateT, modify)
import Control.Monad.Writer
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec

import BlindwormParser
import Executor

type StackOffset = Int

type VariableScope = Map String StackOffset

data Env = Env
  { globals :: VariableScope
  , locals :: VariableScope
  }

type CodeGen = ReaderT Env (StateT StackOffset (Writer [String])) ()

opToStr :: AstOperator -> String
opToStr Add = "+"
opToStr Subtract = "-"
opToStr Multiply = "*"
opToStr Divide = "/"
opToStr Modulo = "%"
opToStr GreaterThan = ">"
opToStr LesserThan = "<"

lookupVarDepth :: String -> Env -> Int -> Int
lookupVarDepth name env depth = do
  let Env {globals = globalVariables, locals = localVariables} = env
  let lookupVar scope = fmap (\slot -> depth - 1 - slot) (Map.lookup name scope)
  case lookupVar localVariables <|> lookupVar globalVariables of
    Just ofs -> ofs
    Nothing -> error $ "Unbound variable: " ++ name

codeGenBlock :: [Ast] -> CodeGen
codeGenBlock [] = return ()
codeGenBlock [last_stm] = codeGen last_stm
codeGenBlock (stmt:rest) = do
  codeGen stmt
  tell ["drop"]
  codeGenBlock rest

codeGenStatement :: Ast -> CodeGen
codeGenStatement statement = do
  codeGen statement
  tell ["0", "drop"]
  modify (\d -> d - 1)

codeGen :: Ast -> CodeGen
codeGen (IntLiteral n) = do
  modify (+ 1)
  tell [show n]
codeGen (Variable name) = do
  env <- ask
  depth <- get
  let offset = lookupVarDepth name env depth
  modify (+ 1)
  tell [show offset, "peek"]
codeGen (Assignment name rhs) = do
  env <- ask
  depth <- get
  codeGen rhs
  let offset = lookupVarDepth name env depth
  modify (\n -> n - 1)
  tell [show offset, "poke", "0"]
  modify (+ 1)
codeGen (BinaryExpression op a b) = do
  codeGen a
  codeGen b
  modify (\n -> n - 1)
  tell [opToStr op]
codeGen (FunctionCall fn args) = do
  mapM_ codeGen args
  case fn of
    "write" -> do
      tell ["int_out", "10", "char_out"]
      modify (+ 1)
      tell ["0"]
    "print" -> do
      tell ["int_in"]
      modify (+ 1)
    _ -> do
      modify (\n -> n - length args + 1)
      tell [fn]
codeGen Pass = do
  modify (+ 1)
  tell ["0"]
codeGen (FunctionDefinition name args body) = do
  tell [":", name]
  Env globalVariables _ <- ask
  oldDepth <- get
  let localVar = collectVariables body
  let localScope = Map.fromList $ zip (args ++ localVar) [oldDepth ..]
  let funEnv = Env {globals = globalVariables, locals = localScope}
  modify (+ length args)
  replicateM_ (length localVar) $ codeGen $ IntLiteral 0
  local (const funEnv) $ codeGenBlock body
  put oldDepth
  tell [";", "0"]
  modify (+ 1)
codeGen (Condition cond thenB elseB) = do
  env <- ask
  depth <- get
  let getBlockLen code =
        execWriter
          $ evalStateT (runReaderT (mapM_ codeGenStatement code) env) depth
  let thenLength = length (getBlockLen thenB)
  let elseLength = maybe 0 (length . getBlockLen) elseB
  let elseJump = show (thenLength + 2)
  let endJump = show (elseLength + 1)
  codeGen cond
  tell ["?branch", elseJump]
  mapM_ codeGenStatement thenB
  case elseB of
    Just eb -> do
      tell ["branch", endJump]
      mapM_ codeGenStatement eb
    Nothing -> return ()
  modify (+ 1)
  tell ["0"]
codeGen (Loop cond body) = do
  env <- ask
  depth <- get
  let estimate code =
        execWriter
          $ evalStateT (runReaderT (mapM_ codeGenStatement code) env) depth
  let bodyLen = length (estimate body)
  let condLen = length (estimate [cond])
  let jumpOut = show (bodyLen + 4)
  let jumpBack = show (-(bodyLen + condLen + 2))
  codeGen cond
  tell ["?branch", jumpOut]
  mapM_ codeGenStatement body
  tell ["branch", jumpBack]
  modify (+ 1)
  tell ["0"]
codeGen (CharLiteral c) = do
  modify (+ 1)
  tell [show c]

collectVariables :: [Ast] -> [String]
collectVariables = collect []
  where
    collect :: [String] -> [Ast] -> [String]
    collect acc [] = acc
    collect acc (Assignment name _:rest) = collect (name : acc) rest
    collect acc (FunctionDefinition {}:rest) = collect acc rest
    collect acc (Loop _ body:rest) = collect (collect acc body) rest
    collect acc (Condition _ tb (Just eb):rest) =
      collect (collect (collect acc tb) eb) rest
    collect acc (Condition _ tb Nothing:rest) = collect (collect acc tb) rest
    collect acc (_:rest) = collect acc rest

codeGenAst :: [Ast] -> [String]
codeGenAst ast =
  let global_vars = nubOrd $ collectVariables ast
      globalsScope = Map.fromList $ zip global_vars [0 ..]
      globalsDefaults = replicate (Map.size globalsScope) "0"
      env = Env globalsScope Map.empty
      code =
        execWriter
          $ evalStateT
              (runReaderT (mapM_ codeGenStatement ast) env)
              (Map.size globalsScope)
   in globalsDefaults ++ code

printHrotFCode :: [String] -> IO ()
printHrotFCode = mapM_ putStrLn

main :: IO ()
main = doMain $ printHrotFCode . codeGenAst
