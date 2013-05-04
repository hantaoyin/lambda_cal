{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import Transformer
import StringQQ
import Data.List

-- data LamExprI = UbSymI Symbol
--              | BSymI Symbol
--              | LamI ID [Symbol] [Symbol] LamExprI
--              | AppI [Symbol] [LamExprI]
----------------------------------------------------------------------
-- compile 
idToName :: Int -> String
idToName n = "_cl_" ++ show n

genClosureLine :: LamExprI -> String
genClosureLine (UbSymI sym) = sym
genClosureLine (BSymI sym) = sym
genClosureLine (LamI n free_vars _ _) =
    let fv_list =
            if length free_vars == 0
            then ""
            else "," ++ intercalate "," free_vars
    in "create_closure(" ++ idToName n ++ fv_list ++ ")"
genClosureLine (AppI _ exprs) =
    "apply(" ++ intercalate "," (map genClosureLine exprs) ++ ")"

genClosureBody :: String -> LamExprI -> String
genClosureBody prefix (UbSymI sym) = prefix ++ sym ++ ";\n"
genClosureBody prefix (BSymI sym)  = prefix ++ sym ++ ";\n"
genClosureBody prefix v@(LamI _ _ _ _) =
    prefix ++ genClosureLine v ++ ";\n"
genClosureBody prefix (AppI _ exprs) =
    (concatMap (\x -> "    push(" ++ genClosureLine x ++ ");\n") . reverse . tail $ exprs) ++
    prefix ++ (genClosureLine . head) exprs ++ ";\n"

genFreeVars :: [Symbol] -> String
genFreeVars xs = 
    let header = "    closure **_fv_ptr = (closure **)(cur_closure + 1);\n"
        go :: Int -> Symbol -> String
        go n sym = "    closure *" ++ sym ++ 
                   " = _fv_ptr[" ++ show n ++ "];\n"
    in header ++ concat (zipWith go [0..] xs)

genBndVars :: [Symbol] -> String
genBndVars xs = 
    let go :: Int -> Symbol -> String
        go n sym = "    closure *" ++ sym ++
                   " = arg_stack[arg_size - 1 - " ++ 
                   show n ++ "];\n"
    in concat $ zipWith go [0 ..] xs


genClosureDef :: LamExprI -> String
genClosureDef (LamI n fvs bvs expr) =
    let nbnd_var = length bvs
        header = "def_closure(" ++ idToName n ++ ")\n"
        need_args = "    need_args(" ++ show nbnd_var ++ ");\n"
        pop_stack = "    arg_size -= " ++ show nbnd_var ++ ";\n"
        body = genClosureBody "    return " expr
    in header ++ "{\n" ++ need_args ++ 
       genBndVars bvs ++ 
       pop_stack ++ 
       genFreeVars fvs ++ 
       body ++ "}\n\n"
genClosureDef _ = error "FIXME: can't call me for any non-lambda expr."
   
genAllClosures :: LamExprI -> String
genAllClosures (UbSymI _) = ""
genAllClosures (BSymI _) = ""
genAllClosures v@(LamI _ _ _ body) = 
    genAllClosures body ++ genClosureDef v
genAllClosures (AppI _ exprs) = 
    concatMap genAllClosures exprs

createSymbol :: Symbol -> String
createSymbol sym = "create_ubsym(" ++ sym ++ ");\n"

genAllUbSyms :: LamExprI -> String
genAllUbSyms (UbSymI sym) = createSymbol sym
genAllUbSyms (BSymI _) = ""
genAllUbSyms (LamI _ free_vars _ _) = 
    concatMap createSymbol free_vars
genAllUbSyms (AppI free_vars _) = 
    concatMap createSymbol free_vars

genMainPrefix :: String
genMainPrefix = [stringQQ|
int main()
{
    initialize();

|]

genMainPostfix :: String
genMainPostfix = [stringQQ|

    while(1) {
        cur_closure = cur_closure->code();
    };
    return 0;
}
|]

compile :: LamExprI -> String
compile expr = 
    genAllUbSyms expr ++ 
    genAllClosures expr ++
    genMainPrefix ++
    genClosureBody "    cur_closure = " expr ++
    genMainPostfix 

----------------------------------------------------------------------
headerStr :: String
headerStr = [stringQQ|
#include "lam_stg.h"

|]

main :: IO ()
main = 
    do val <- getContents
       -- putStrLn "/*"
       -- putStrLn . show . toLamExprI . rename $ readExpr val
       -- putStrLn "*/"
       putStrLn headerStr
       putStrLn . compile . toLamExprI . rename $ readExpr val
