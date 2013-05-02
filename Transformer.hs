module Transformer (rename,
                   LamDeBruijn(..), toDeBruijn,
                   LamExprF(..), toLamExprF,
                   LamInfo(..), toLamInfo) where
import Data.List
import LamParser

-- data LamExpr = Sym Symbol
--             | Lam Symbol LamExpr -- 1 parameter functions
--             | App LamExpr LamExpr

----------------------------------------------------------------------
-- rename the variables
genSymbol :: Int -> Symbol
genSymbol n = "v" ++ show n

renameSym :: [(Symbol, Symbol)] -> Symbol -> LamExpr
renameSym [] s = Sym s    -- unbounded variable
renameSym ((x,v):xs) s
    | s == x = Sym v
    | otherwise = renameSym xs s

renameHelper :: [(Symbol, Symbol)] -> Int -> LamExpr -> LamExpr
renameHelper env _ (Sym s) = renameSym env s
renameHelper env n (Lam sym expr) =
    let sym' = genSymbol n
        new_env = (sym, sym'):env
    in Lam sym' $ renameHelper new_env (n+1) expr
renameHelper env n (App a b) =
    let an = renameHelper env n a
        bn = renameHelper env n b
    in App an bn

rename :: LamExpr -> LamExpr
rename = renameHelper [] 0

----------------------------------------------------------------------
-- Lambda expressions in the De Bruijn notation.
data LamDeBruijn = UnboundSym Symbol
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
transToDB :: [Symbol] -> LamExpr -> LamDeBruijn
transToDB env (Sym sym) = let dbv = lookupSym env sym
                              in if dbv < 0
                                 then UnboundSym sym
                                 else BoundSym dbv
transToDB env (Lam sym expr) = LamDB (transToDB (sym:env) expr)
transToDB env (App f p) = AppDB (transToDB env f) (transToDB env p)

toDeBruijn :: LamExpr -> LamDeBruijn
toDeBruijn = transToDB []

----------------------------------------------------------------------
-- Flattern the expression (combine multiple lambdas or applications
-- into a single one)
data LamExprF = SymF Symbol
          | LamF [Symbol] LamExprF
          | AppF [LamExprF]

toLamExprF :: LamExpr -> LamExprF
toLamExprF (Sym sym) = SymF sym
toLamExprF (Lam sym expr) =
           let exprf = toLamExprF expr
           in case exprf of
              LamF syms bodyf -> LamF (sym:syms) bodyf
              v -> LamF [sym] v
toLamExprF (App a b) = 
           let af = toLamExprF a
               bf = toLamExprF b
           in case af of
              AppF afs -> AppF (afs ++ [bf])
              v -> AppF [v,bf]
                        
----------------------------------------------------------------------
data LamInfo = UbSymI Symbol
             | BSymI Symbol
             | LamI [Symbol] [Symbol] LamInfo
             | AppI [Symbol] [LamInfo]

getFreeVar :: LamInfo -> [Symbol]
getFreeVar (UbSymI sym) = [sym]
getFreeVar (BSymI sym) = [sym]
getFreeVar (LamI syms _ _) = syms
getFreeVar (AppI syms _) = syms

extractInfo :: [Symbol] -> LamExprF -> LamInfo
extractInfo env (SymF sym) = let dbv = lookupSym env sym
                            in if dbv < 0
                               then UbSymI sym
                               else BSymI sym
extractInfo env (LamF syms expr) = let einfo = extractInfo (syms ++ env) expr
                                       efree = getFreeVar einfo
                                       symlist = foldl' (flip delete) efree syms
                                   in LamI symlist syms einfo
extractInfo env (AppF as) = let ais = map (extractInfo env) as
                                symlist = foldl' union [] $ map getFreeVar ais
                            in AppI symlist ais

toLamInfo :: LamExprF -> LamInfo
toLamInfo = extractInfo []

instance Show LamInfo where show = showInfo

showInfo :: LamInfo -> String
showInfo (UbSymI sym) = sym
showInfo (BSymI sym) = sym
showInfo (LamI vs xs expr) = "\n[" ++ intercalate "," vs ++ "] \\"
                              ++ intercalate " " xs ++ " -> "
                              ++ showInfo expr
showInfo (AppI vs exprs) = "[" ++ intercalate "," vs ++ "] "
                         ++ (intercalate " " $ map showArg exprs)
         where showArg v@(AppI _ _) = "(" ++ showInfo v ++ ")"
               showArg v@(LamI _ _ _) = "(" ++ showInfo v ++ ")"
               showArg v = showInfo v
