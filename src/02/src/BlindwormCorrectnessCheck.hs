import qualified Text.PrettyPrint as P
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Control.Monad.State (StateT (runStateT), modify, gets, MonadState (put, get))
import Control.Monad (unless, when,forM_)

import PrettyPrinter (printAst)
import BlindwormParser
import Executor


data ValidationError
  = UndefinedVariable String
  | UndefinedFunction String
  | FunctionRedefinition String
  | NestedFuncDefinition String
  | ArityMismatch String Int Int
  | NotAFunction String
  | NotAVariable String
  deriving (Eq, Ord, Show)

type CheckEnv = Map String Int

type CheckState = Set.Set String

type ErrorContext = [String]
data CheckError = AnalysisError
  { errorType    :: ValidationError
  , errorContext :: ErrorContext
  } deriving (Show)

type CorrectnessCheck a = ReaderT CheckEnv (ReaderT ErrorContext (StateT CheckState (Writer [CheckError]))) a

checkFuncExists :: String -> CheckEnv -> Bool
checkFuncExists = Map.member

addFunc :: String -> Int -> CheckEnv -> CheckEnv
addFunc  = Map.insert

lookupFuncArity :: String -> CorrectnessCheck (Maybe Int)
lookupFuncArity name = do asks (Map.lookup name)

addVar :: String -> CorrectnessCheck ()
addVar name = do 
  lift . lift $ modify (Set.insert name)

checkVarExists :: String -> CorrectnessCheck Bool
checkVarExists name = do
  lift . lift $ gets (Set.member name) 

logError :: ValidationError -> CorrectnessCheck ()
logError err = do
  ctx <- lift ask
  tell [AnalysisError err ctx]

withContext :: String -> CorrectnessCheck a -> CorrectnessCheck a
withContext newContext  = mapReaderT (local (newContext :)) 

checkCodeBlock :: [Ast] -> CorrectnessCheck()
checkCodeBlock block = 
  forM_ block $ \statement -> work statement where
    work :: Ast -> CorrectnessCheck()
    work (FunctionDefinition name args body) = do 
      logError $ NestedFuncDefinition name
      check $ FunctionDefinition name args body
    work s =  check s

collectFunctions :: [Ast] -> (CheckEnv, [CheckError])
collectFunctions = foldr collect (Map.fromList [("print", 1), ("read", 0)], [])
  where
    collect :: Ast -> (CheckEnv, [CheckError]) -> (CheckEnv, [CheckError])
    collect (FunctionDefinition name args _) (env, errs) =
      if checkFuncExists name env
        then (env, AnalysisError (FunctionRedefinition name) [] : errs)
        else (addFunc name (length args) env, errs)
    collect _ acc = acc

check :: Ast -> CorrectnessCheck ()

check (Variable name)  = do
  exists <- checkVarExists name 
  unless exists  (logError $ UndefinedVariable name)

check (Assignment name rhs) = withContext ("in assignment " ++ P.render (printAst (Assignment name rhs))) $ do
  check rhs
  addVar name

check (FunctionCall name args) = withContext ("in function call " ++ name ++ "()") $ do
  expectedArityMaybe <- lookupFuncArity name
  varExists <- checkVarExists name
  case expectedArityMaybe of
    Nothing -> if varExists then  logError (NotAFunction name) else logError (UndefinedFunction name)
    Just expectedArity -> do
      let actualArity = length args
      when (expectedArity /= actualArity) $
        logError (ArityMismatch name expectedArity actualArity)
  mapM_ check args

check (FunctionDefinition name args body) =
  withContext ("in definition of function '" ++ name ++ "'") $ do
    originalScope <- get
    modify (Set.union (Set.fromList args))
    checkCodeBlock body
    put originalScope

check (Loop cond body) = 
  withContext ("in loop while " ++ P.render(printAst cond) ++ ":") $ do
      check cond
      checkCodeBlock body

check (Condition cond thenBlock elseBlock) = 
  withContext ("in condition if " ++ P.render (printAst cond) ++ ":") $ do 
      check cond
      checkCodeBlock thenBlock
      F.forM_ elseBlock checkCodeBlock

check (BinaryExpression _ lhs rhs) =  do
  check lhs
  check rhs

check (IntLiteral _) = do 
  return ()

check (CharLiteral _) = do
  return ()

check Pass = do
  return ()

prettyPrintErrors :: [CheckError] -> IO ()
prettyPrintErrors errs = forM_ errs $ \err -> do
  putStrLn $ "Error: " ++ show (errorType err)
  forM_ (reverse $ errorContext err) $ \ctx ->
    putStrLn $ "  .. " ++ ctx

doCorrectnessCheck :: [Ast] -> [CheckError]
doCorrectnessCheck ast = 
  let (env, redefinitionErrors) = collectFunctions ast
      remainingErrors = execWriter (runStateT (runReaderT (runReaderT (mapM_ check ast) env) []) Set.empty)
  in redefinitionErrors ++ remainingErrors

main :: IO ()
main = doMain $ prettyPrintErrors . doCorrectnessCheck