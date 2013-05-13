{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import Transformer
import StringQQ
import Data.List

-- type Name = Symbol
-- type FreeVars = [Name]
-- type Params = [Name]

-- data LamExprI = UbSymI Name Symbol
--              | BSymI Name
--              | LamI ID FreeVars Params LamExprI
--              | AppI FreeVars [LamExprI]
----------------------------------------------------------------------
createSymbol :: (Name,Symbol) -> String
createSymbol (name,sym) = 
    "create_ubsym(" ++ name ++ "," ++ show sym ++ ");\n"

findAllUbSyms :: LamExprI -> [(Name,Symbol)]
findAllUbSyms (UbSymI name sym) = [(name,sym)]
findAllUbSyms (LamI _ _ _ expr) = findAllUbSyms expr
findAllUbSyms (AppI _ exprs) = 
    foldl union [] $ map findAllUbSyms exprs
findAllUbSyms _ = []

genAllUbSyms :: LamExprI -> String
genAllUbSyms expr =
    concatMap createSymbol $ findAllUbSyms expr

callFunc :: String -> String -> String
callFunc func arg = func ++ "(" ++ arg ++ ")"

idToName :: Int -> String
idToName n = "cl_" ++ show n

genMakeClosure :: Int -> String
genMakeClosure n = 
    let n_str = show n
        paramList = concatMap ((", closure *c" ++) . show) ([0..(n-1)]::[Int])
        header = "closure *make_closure" ++ n_str ++ 
                 "(func_ptr func" ++ paramList ++ ")\n"
        body0 = "    closure *ret = alloc_heap(" ++ n_str ++ ");\n" ++
                "    ret->code = func;\n" ++
                "    ret->fv_cnt = " ++ n_str ++ ";\n"
        body1 = if n == 0
                then ""
                else "\n    closure **fv_ptr = (closure **)(ret + 1);\n"
        oneLine k = "    fv_ptr[" ++ show k ++ "] = c" ++ show k ++ ";\n"
        body2 = concatMap oneLine $ ([0..(n-1)]::[Int])
        body3 = "\n    return ret;\n"
    in header ++ "{\n" ++ body0 ++ body1 ++ body2 ++ body3 ++ "}\n\n"
               
genAllMkClosures :: LamExprI -> String
genAllMkClosures expr =
    let go :: LamExprI -> [Int]
        go (LamI _ fvs _ (AppI _ es)) = 
            foldl union [length fvs] $ map go es
        go (LamI _ fvs _ e) = union [length fvs] $ go e
        go (AppI fvs es) = foldl union [1 + length fvs] $ map go es
        go _ = []
    in concatMap genMakeClosure $ sort $ go expr

genClosureLine :: LamExprI -> String
genClosureLine (UbSymI name _) = name
genClosureLine (BSymI sym) = sym
genClosureLine (LamI n fvs _ _) =
    let fv_cnt = length fvs
        fv_list = concatMap ("," ++) fvs
    in callFunc ("make_closure" ++ show fv_cnt) $ idToName n ++ fv_list
genClosureLine (AppI _ exprs) =
    let applyn = "make_closure" ++ (show $ length exprs) ++ "(apply_upd,"
    in applyn ++ (intercalate "," $ map genClosureLine exprs) ++ ")"

genClosureBody :: String -> LamExprI -> String
genClosureBody prefix (UbSymI name _) = prefix ++ name ++ ";\n"
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
    let header = "    closure **fv_ptr = (closure **)(cur_closure + 1);\n"
        go :: Int -> Symbol -> String
        go n sym = "    closure *" ++ sym ++ 
                   " = fv_ptr[" ++ show n ++ "];\n"
    in header ++ concat (zipWith go [0..] xs)

genBndVars :: [Symbol] -> [Symbol] -> String
genBndVars xs ys =
    let go :: Int -> Symbol -> String
        go n sym = if elem sym ys
                   then "    closure *" ++ sym ++
                        " = get_param(" ++ show n ++ ");\n"
                   else ""
    in concat $ zipWith go [0..] xs

genDebugComment :: LamExprI -> String
genDebugComment expr = "// " ++ shortShow expr ++ "\n"

genClosureDef :: LamExprI -> String
genClosureDef lamexpr@(LamI n fvs bvs expr) =
    let nbnd_var = show $ length bvs
        debug_info = genDebugComment lamexpr
        header = "closure *" ++ idToName n ++ "(void)\n"
        need_args = "    need_args(" ++ nbnd_var ++ ");\n"
        pop_stack = "    pop(" ++ nbnd_var ++ ");\n"
        body = genClosureBody "    return " expr
    in debug_info ++
       header ++ "{\n" ++ 
       need_args ++ 
       genBndVars bvs (getFreeVars expr)++
       pop_stack ++ 
       genFreeVars fvs ++ 
       body ++ "}\n\n"
genClosureDef _ = error "FIXME: can't call me on any non-lambda expr."
   
genAllClosures :: LamExprI -> String
genAllClosures v@(LamI _ _ _ body) = 
    genAllClosures body ++ genClosureDef v
genAllClosures (AppI _ exprs) = 
    concatMap genAllClosures exprs
genAllClosures _ = ""

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
        if(need_gc()) {
            gc();
        }

    };
    return 0;
}
|]

compile :: LamExprI -> String
compile expr = 
    genAllUbSyms expr ++ "\n" ++
    genAllMkClosures expr ++
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
