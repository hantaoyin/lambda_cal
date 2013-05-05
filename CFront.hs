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
callFunc :: String -> String -> String
callFunc func arg = func ++ "(" ++ arg ++ ")"

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
    in callFunc "create_closure" $ idToName n ++ fv_list
genClosureLine (AppI _ exprs) =
    callFunc "apply" $ intercalate "," $ map genClosureLine exprs

genClosureBody :: String -> LamExprI -> String
genClosureBody prefix (UbSymI sym) = prefix ++ sym ++ ";\n"
genClosureBody prefix (BSymI sym)  = prefix ++ sym ++ ";\n"
genClosureBody prefix v@(LamI _ _ _ _) =
    prefix ++ genClosureLine v ++ ";\n"
genClosureBody prefix (AppI _ exprs) =
    let convertLine x = "    push(" ++ genClosureLine x ++ ");\n"
    in (concatMap convertLine . reverse . tail $ exprs) ++
       prefix ++ (genClosureLine $ head exprs) ++ ";\n"

genFreeVars :: [Symbol] -> String
genFreeVars [] = ""
genFreeVars xs = 
    let header = "    closure **_fv_ptr = (closure **)(cur_closure + 1);\n"
        go :: Int -> Symbol -> String
        go n sym = "    closure *" ++ sym ++ 
                   " = _fv_ptr[" ++ show n ++ "];\n"
    in header ++ concat (zipWith go [0..] xs)

extractFreeVars :: LamExprI -> [Symbol]
extractFreeVars (UbSymI sym) = [sym]
extractFreeVars (BSymI sym) = [sym]
extractFreeVars (LamI _ free_vars _ _) = free_vars
extractFreeVars (AppI free_vars _) = free_vars

genBndVars :: [Symbol] -> [Symbol] -> String
genBndVars xs ys =
    let go :: Int -> Symbol -> String
        go n sym = if elem sym ys
                   then "    closure *" ++ sym ++
                        " = get_param(" ++ show n ++ ");\n"
                   else ""
    in concat $ zipWith go [0 ..] xs

genDebugComment :: LamExprI -> String
genDebugComment expr = "// " ++ shortShow expr ++ "\n"

genClosureDef :: LamExprI -> String
genClosureDef lamexpr@(LamI n fvs bvs expr) =
    let nbnd_var = show $ length bvs
        debug_info = genDebugComment lamexpr
        header = (callFunc "def_closure" $ idToName n) ++ "\n"
        need_args = "    need_args(" ++ nbnd_var ++ ");\n"
        pop_stack = "    pop(" ++ nbnd_var ++ ");\n"
        body = genClosureBody "    return " expr
    in debug_info ++
       header ++ 
       "{\n" ++ 
       need_args ++ 
       genBndVars bvs (extractFreeVars expr)++
       pop_stack ++ 
       genFreeVars fvs ++ 
       body ++ 
       "}\n\n"
genClosureDef _ = error "FIXME: can't call me on any non-lambda expr."
   
genAllClosures :: LamExprI -> String
genAllClosures v@(LamI _ _ _ body) = 
    genAllClosures body ++ genClosureDef v
genAllClosures (AppI _ exprs) = 
    concatMap genAllClosures exprs
genAllClosures _ = ""

createSymbol :: Symbol -> String
createSymbol sym = "create_ubsym(" ++ sym ++ ",\"" ++ sym ++ "\");\n"

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
        gc();
    };
    return 0;
}
|]

compile :: LamExprI -> String
compile expr = 
    genAllUbSyms expr ++ "\n" ++
    genAllClosures expr ++
    genDebugComment expr ++
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
       putStrLn headerStr
       putStrLn . compile . toLamExprI . rename $ readExpr val
