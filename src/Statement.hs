-- Pilar Frutos and Santino Pepe
module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skip = require "skip" #- require ";" >-> (\_ -> Skip)

readStmt = require "read" -# word #- require ";" >-> Read

writeStmt = require "write" -# Expr.parse #- require ";" >-> Write

begin = require "begin" -# iter statement #- require "end" >-> Begin

ifStmt = require "if" -# Expr.parse #- require "then" # statement #- require "else" # statement >-> (\((c,t),e) -> If c t e)

whileStmt = require "while" -# Expr.parse #- require "do" # statement >-> uncurry While

statement :: Parser Statement
statement = assignment
        !   skip
        !   readStmt
        !   writeStmt
        !   begin
        !   ifStmt
        !   whileStmt

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where

    execute [] _ _ = []

    execute (If cond thenStmts elseStmts: stmts) dict input =
        case (Expr.value cond dict) of
            Left err -> error err
            Right v ->
                if v > 0 then
                    execute (thenStmts: stmts) dict input
                else
                    execute (elseStmts: stmts) dict input

    execute (Skip : stmts) dict input =
        execute stmts dict input

    execute (Assignment var expr : stmts) dict input =
        case Expr.value expr dict of
            Left _ ->
                execute stmts (Dictionary.insert (var, 0) dict) input
            Right val ->
                execute stmts (Dictionary.insert (var, val) dict) input

    execute (Read var : stmts) dict (x:xs) =
        execute stmts (Dictionary.insert (var, x) dict) xs

    execute (Read _ : _) _ [] = error "empty input"

    execute (Write expr : stmts) dict input =
        case Expr.value expr dict of
            Left err -> error err
            Right val ->
                val : execute stmts dict input

    execute (Begin ss : stmts) dict input =
        execute (ss ++ stmts) dict input

    execute (While cond stmt : stmts) dict input =
        case Expr.value cond dict of
            Left err -> error err
            Right v ->
                if v > 0 then
                    execute (stmt : While cond stmt : stmts) dict input
                else
                    execute stmts dict input

instance Parse Statement where
    parse = statement
    toString = showStmt

showStmt :: Statement -> String
showStmt (Assignment v e) = v ++ " := " ++ Expr.toString e ++ ";"
showStmt Skip = "skip;"
showStmt (Begin ss) = "begin " ++ concatMap showStmt ss ++ "end"
showStmt (If c t e) =
    "if " ++ Expr.toString c ++ " then " ++ showStmt t ++ " else " ++ showStmt e
showStmt (While c s) =
    "while " ++ Expr.toString c ++ " do " ++ showStmt s
showStmt (Read v) = "read " ++ v ++ ";"
showStmt (Write e) = "write " ++ Expr.toString e ++ ";"
