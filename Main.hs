import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e

-- Lists
evalExpr env (ArrayLit []) = return $ List []
evalExpr env (ArrayLit [expr]) = do
    val <- evalExpr env expr
    return $ List [val]
evalExpr env (ArrayLit (l:ls)) = do
    cabeca <- evalExpr env l
    (List cauda) <- evalExpr env (ArrayLit ls)
    return $ List (cabeca:cauda)


----lista de literal
--evalExpr env (ArrayLit []) = return Nil
--evalExpr env (ArrayList [expr]) = do
--    e <- evalExpr env expr
--        case e of
--            (Int e) -> if (e) then algo else return Nil
--            _ -> error $ "Lista não feita de literais"
--evalExpr env (ArrayList (a:as)) = do




--evalExpr env 

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr

-- blocos de statements
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt [stmt]) = evalStmt env stmt
evalStmt env (BlockStmt (stmt:stmts)) = do
    cabeca <- evalStmt env stmt
    case cabeca of
        Break -> return Break
        _ -> evalStmt env (BlockStmt stmts)


-- if com um único stmt --
evalStmt env (IfSingleStmt expr stmt) = do
    v <- evalExpr env expr
    case v of
        (Bool b) -> if (b) then evalStmt env stmt else return Nil
        _ -> error $ "Not a valid expression"

-- blocos de statements
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt [stmt]) = evalStmt env stmt
evalStmt env (BlockStmt (stmt:stmts)) = do
    cabeca <- evalStmt env stmt
    evalStmt env (BlockStmt stmts)

-- if com 2 stmts --   if x then stmt1 else stmt2
evalStmt env (IfStmt expr stmt1 stmt2) = do
    v <- evalExpr env expr
    case v of
        (Bool b) -> if (b) then evalStmt env stmt1 else evalStmt env stmt2
        _ -> error $ "Not a valid expression"


-- While 
evalStmt env (WhileStmt expr stmt) = do
    result <- evalExpr env expr
    case result of 
        (Bool b) -> if b then (evalStmt env stmt) >> (evalStmt env (WhileStmt expr stmt)) else return Nil
        _ -> error "Boolean expression expected."

--DoWhile
evalStmt env (DoWhileStmt stmt expr) = do
    evalStmt env stmt
    result <- evalExpr env expr
    case result of 
        (Bool b) -> if b then evalStmt env (DoWhileStmt stmt expr) else return Nil
        _ -> error "Boolean expression expected."

-- BreakStmt
evalStmt env (BreakStmt Nothing) = return Break

-- ForStmt for normal
evalStmt env (ForStmt initial test inc stmt) = do
    v <- evalForInit env initial
    testRes <- evalForTest env test -- Asks if the loop should continue 
    case testRes of
        (Bool True) -> do
            evalForInc env inc
            evalStmt env stmt
            evalStmt env (ForStmt NoInit test inc stmt)
        (Bool False) -> return Nil
        -- TODO Error
        _ -> error "Not a valid expression"


-- Evaluates For increment expression
evalForInc :: StateT -> (Maybe Expression) -> StateTransformer Value
evalForInc _ Nothing = return Nil
evalForInc env (Just inc) = evalExpr env inc

-- Evaluates For test expression
evalForTest :: StateT -> (Maybe Expression) -> StateTransformer Value
evalForTest _ Nothing = return $ Bool True
evalForTest env (Just test) = evalExpr env test

-- Evaluates For initialization
evalForInit :: StateT -> ForInit -> StateTransformer Value
evalForInit env NoInit = return Nil
evalForInit env (VarInit list) = evalStmt env (VarDeclStmt list)
evalForInit env (ExprInit expr) = evalExpr env expr

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
