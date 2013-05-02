{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import Transformer
import StringQQ

----------------------------------------------------------------------
-- data LamDeBruijn = UnboundSym String
--                  | BoundSym Int
--                  | LamDB LamDeBruijn -- 1 parameter functions, we no
--                                    -- longer need to specify the dummy
--                                    -- parameter.
--                  | AppDB LamDeBruijn LamDeBruijn

analyze :: LamDeBruijn -> String
analyze (UnboundSym s) = "symbol(\"" ++ s ++ "\")"
analyze (BoundSym n) = "variable(" ++ show n ++ ")"
analyze (LamDB v) = "lambda(" ++ analyze v ++ ")"
analyze (AppDB a b) = "apply(" ++ analyze a ++ "," ++ analyze b ++ ")"

----------------------------------------------------------------------
headerStr :: String
headerStr = [stringQQ|
#include <iostream>
#include "lam_int.h"

|]

addMain :: String -> String
addMain s = "int main() {std::cout << show(NULL," 
        ++ s 
        ++ ") << std::endl;return 0; }\n"

main :: IO ()
main = 
    do val <- getContents
       putStrLn headerStr
       putStrLn $ addMain $ analyze $ toDeBruijn $ readExpr val
