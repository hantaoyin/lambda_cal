module Transformer (rename, ID,
                   LamDeBruijn(..), toDeBruijn,
                   LamExprI(..), toLamExprI, shortShow) where
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
type ID = Int

data LamExprF = SymF Symbol
          | LamF ID [Symbol] LamExprF
          | AppF [LamExprF]

toLamExprF :: LamExpr -> LamExprF
toLamExprF (Sym sym) = SymF sym
toLamExprF (Lam sym expr) =
           let exprf = toLamExprF expr
           in case exprf of
              LamF _ syms bodyf -> LamF 0 (sym:syms) bodyf
              v -> LamF 0 [sym] v
toLamExprF (App a b) = 
           let af = toLamExprF a
               bf = toLamExprF b
           in case af of
              AppF afs -> AppF (afs ++ [bf])
              v -> AppF [v,bf]

----------------------------------------------------------------------
nameLamHelper :: Int -> LamExprF -> (Int, LamExprF)
nameLamHelper n v@(SymF _) = (n, v)
nameLamHelper n (LamF _ xs expr) =
    let (n', expr') = nameLamHelper (n+1) expr
    in (n',LamF n xs expr')
nameLamHelper n (AppF exprs) =
    let go e (k, es) =
            let (k',e') = nameLamHelper k e
            in (k',e':es)
        (n',exprs') = foldr go (n,[]) exprs
    in (n', AppF exprs')

nameLam :: LamExprF -> LamExprF
nameLam expr = 
    let (_,expr') = nameLamHelper 0 expr
    in expr'
                        
----------------------------------------------------------------------
data LamExprI = UbSymI Symbol
             | BSymI Symbol
             | LamI ID [Symbol] [Symbol] LamExprI
             | AppI [Symbol] [LamExprI]

getFreeVar :: LamExprI -> [Symbol]
getFreeVar (UbSymI sym) = [sym]
getFreeVar (BSymI sym) = [sym]
getFreeVar (LamI _ syms _ _) = syms
getFreeVar (AppI syms _) = syms

extractInfo :: [Symbol] -> LamExprF -> LamExprI
extractInfo env (SymF sym) =
    let dbv = lookupSym env sym
    in if dbv < 0
       then UbSymI sym
       else BSymI sym

extractInfo env (LamF n syms expr) = 
    let einfo = extractInfo (syms ++ env) expr
        efree = getFreeVar einfo
        symlist = foldl' (flip delete) efree syms
    in LamI n symlist syms einfo

extractInfo env (AppF as) = 
    let ais = map (extractInfo env) as
        symlist = foldl' union [] $ map getFreeVar ais
    in AppI symlist ais

toLamExprI :: LamExpr -> LamExprI
toLamExprI = extractInfo [] . nameLam . toLamExprF

instance Show LamExprI where show = showLamExprI

showLamExprI :: LamExprI -> String
showLamExprI (UbSymI sym) = sym
showLamExprI (BSymI sym) = sym
showLamExprI (LamI n vs xs expr) = 
    "\n{" ++ intercalate "," vs ++ "} [" ++ show n ++ "]\\{" ++
    intercalate " " xs ++ "} -> " ++
    showLamExprI expr
showLamExprI (AppI vs exprs) = "{" ++ intercalate "," vs ++ "} "
                         ++ (intercalate " " $ map showArg exprs)
         where showArg v@(AppI _ _) = "(" ++ showLamExprI v ++ ")"
               showArg v@(LamI _ _ _ _) = "(" ++ showLamExprI v ++ ")"
               showArg v = showLamExprI v

-- A simpler version, don't delve into nested lambdas.
shortShowHelper :: LamExprI -> String
shortShowHelper (UbSymI sym) = sym
shortShowHelper (BSymI sym) = sym
shortShowHelper (LamI n vs _ _) = 
    "\\Lam [" ++ show n ++ "] {" ++ intercalate "," vs ++ "}"
shortShowHelper (AppI _ exprs) = 
    intercalate " " $ map showArg exprs
    where showArg v@(AppI _ _) = "(" ++ shortShowHelper v ++ ")"
          showArg v@(LamI _ _ _ _) = "(" ++ shortShowHelper v ++ ")"
          showArg v = shortShowHelper v

shortShow :: LamExprI -> String
shortShow (LamI n vs xs expr) = 
    "Lam [" ++ show n ++ "] {" ++ intercalate "," vs ++ "} \\n {" ++
    intercalate " " xs ++ "} -> " ++
    shortShowHelper expr
shortShow v = shortShowHelper v
