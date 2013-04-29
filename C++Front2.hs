{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import StringQQ

----------------------------------------------------------------------
-- Lambda expressions in the De Bruijn notation.
data LamDeBruijn = UnboundSym String
                 | BoundSym Int
                 | LamDB LamDeBruijn -- 1 parameter functions, we no
                                     -- longer need to specify the dummy
                                     -- parameter.
                 | AppDB LamDeBruijn LamDeBruijn

instance Show LamDeBruijn where show = showDB

showDB :: LamDeBruijn -> String
showDB (UnboundSym s) = s
showDB (BoundSym n) = show n
showDB (LamDB v) = "-> " ++ showDB v
showDB (AppDB a b) = showFun a ++ " " ++ showArg b
        where showArg v@(AppDB _ _) = "(" ++ showDB v ++ ")"
              showArg v@(LamDB _) = "(" ++ showDB v ++ ")"
              showArg v = showDB v
              showFun v@(LamDB _) = "(" ++ showDB v ++ ")"
              showFun v = showDB v

-- return -1 if not found.
lookupSym :: [Symbol] -> Symbol -> Int
lookupSym env sym = let go [] _ _ = -1
                        go (x:xs) s ret
                           | x == s = ret
                           | otherwise = go xs s (ret + 1)
                    in go env sym 0

-- [String] serves as the environment when we do the translation.
transformToDB :: [Symbol] -> LamExpr -> LamDeBruijn
transformToDB env (Sym sym) = let dbv = lookupSym env sym
                              in if dbv < 0
                                 then UnboundSym sym
                                 else BoundSym dbv
transformToDB env (Lam sym expr) = LamDB (transformToDB (sym:env) expr)
transformToDB env (App f p) = AppDB (transformToDB env f) (transformToDB env p)

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
       putStrLn $ addMain $ analyze $ transformToDB [] $ readExpr val
