{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import Transformer
import StringQQ

----------------------------------------------------------------------
-- compile 
compileHelper :: [Symbol] -> LamExpr -> String
compileHelper env (Sym s) = 
    if elem s env
    then s
    else "NForm_ptr(new NForm(\"" ++ s ++ "\"))"

compileHelper env (Lam s e) = "lambda(" ++ s ++ "," ++ compileHelper (s:env) e ++ ")"
compileHelper env (App f p) =
    let fn = compileHelper env f
        pn = compileHelper env p
    in "apply(" ++ fn ++ "," ++ pn ++ ")"

compile :: LamExpr -> String
compile expr = compileHelper [] $ rename expr

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
       putStrLn $ addMain $ compile $ readExpr val
