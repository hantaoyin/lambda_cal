{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import StringQQ

----------------------------------------------------------------------
-- compile 
ren :: Int -> Symbol
ren n = "v" ++ show n

lookupSym :: [(Symbol, Symbol)] -> Symbol -> Symbol
lookupSym [] s = "NForm_ptr(new NForm(\"" ++ s ++ "\"))"
lookupSym ((x,v):xs) s
    | s == x = v
    | otherwise = lookupSym xs s

analyze' :: [(Symbol,Symbol)] -> Int -> LamExpr -> String
analyze' env _ (Sym s) = lookupSym env s
analyze' env n (Lam s e) = 
    let s' = ren n
        new_env = (s,s'):env
    in "lambda(" ++ s' ++ "," ++ analyze' new_env (n+1) e ++ ")"
analyze' env n (App f p) =
    let fn = analyze' env (n+1) f
        pn = analyze' env (n+1) p
    in "apply(" ++ fn ++ "," ++ pn ++ ")"

analyze :: LamExpr -> String
analyze = analyze' [] 0

addMain :: String -> String
addMain s = "int main() {std::cout << show(" 
        ++ s 
        ++ ") << std::endl;return 0; }\n"

----------------------------------------------------------------------
headerStr :: String
headerStr = [stringQQ|
#include "lam_header.h"

|]

main :: IO ()
main = 
    do val <- getContents
       putStrLn headerStr
       putStrLn $ addMain $ analyze $ readExpr val
