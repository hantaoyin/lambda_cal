module Transformer (rename, ID, Name, FreeVars, Params,
                   LamDeBruijn(..), toDeBruijn,
                   LamExprI(..), getFreeVars, toLamExprI, shortShow) where
import Data.List
import Data.Maybe
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
showDB (AppDB a b) = 
    let showArg v@(AppDB _ _) = "(" ++ showDB v ++ ")"
        showArg v@(LamDB _) = "(" ++ showDB v ++ ")"
        showArg v = showDB v
        showFun v@(LamDB _) = "(" ++ showDB v ++ ")"
        showFun v = showDB v
    in showFun a ++ " " ++ showArg b

-- return -1 if not found.
lookupSym :: [Symbol] -> Symbol -> Int
lookupSym env sym = 
    let go [] _ _ = -1
        go (x:xs) s ret
           | x == s = ret
           | otherwise = go xs s (ret + 1)
    in go env sym 0

-- [String] serves as the environment when we do the translation.
transToDB :: [Symbol] -> LamExpr -> LamDeBruijn
transToDB env (Sym sym) = 
    let dbv = lookupSym env sym
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
type Name = Symbol
type FreeVars = [Name]
type Params = [Name]

data LamExprI = UbSymI Name Symbol
             | BSymI Name
             | LamI ID FreeVars Params LamExprI
             | AppI FreeVars [LamExprI]

getFreeVars :: LamExprI -> [Name]
getFreeVars (UbSymI name _) = [name]
getFreeVars (BSymI name) = [name]
getFreeVars (LamI _ names _ _) = names
getFreeVars (AppI names _) = names

extractInfo :: [Symbol] -> LamExprF -> LamExprI
extractInfo env (SymF sym) =
    let dbv = lookupSym env sym
    in if dbv < 0
       then UbSymI sym $ error "unnamed free variable!"
       else BSymI sym

extractInfo env (LamF n syms expr) = 
    let einfo = extractInfo (syms ++ env) expr
        efree = getFreeVars einfo
        symlist = foldl' (flip delete) efree syms
    in LamI n symlist syms einfo

extractInfo env (AppF as) = 
    let ais = map (extractInfo env) as
        symlist = foldl' union [] $ map getFreeVars ais
    in AppI symlist ais

renFreeVarsHelper :: [(Symbol,Name)] -> LamExprI -> LamExprI
renFreeVarsHelper env (UbSymI sym _) =
    UbSymI (fromJust $ lookup sym env) sym
renFreeVarsHelper _ v@(BSymI _) = v
renFreeVarsHelper env (LamI n fvs ps expr) =
    LamI n (map (\x -> fromMaybe x $ lookup x env) fvs) ps 
         $ renFreeVarsHelper env expr
renFreeVarsHelper env (AppI fvs exprs) =
    AppI (map (\x -> fromMaybe x $ lookup x env) fvs) 
         $ map (renFreeVarsHelper env) exprs

genFreeVarNames :: [Symbol] -> [(Symbol,Name)]
genFreeVarNames syms = 
    zip syms $ map (("f" ++) . show) ([0..] :: [Int])

renFreeVars :: LamExprI -> LamExprI
renFreeVars expr = 
    let fv_list = getFreeVars expr
        env = genFreeVarNames fv_list
    in renFreeVarsHelper env expr

toLamExprI :: LamExpr -> LamExprI
toLamExprI = renFreeVars . extractInfo [] . nameLam . toLamExprF

instance Show LamExprI where show = showLamExprI

showLamExprI :: LamExprI -> String
showLamExprI (UbSymI name sym) =
    "(" ++ name ++ "," ++ show sym ++ ")"
showLamExprI (BSymI sym) = sym
showLamExprI (LamI n vs xs expr) = 
    "\n{" ++ intercalate "," vs ++ "} [" ++ show n ++ "]\\{" ++
    intercalate " " xs ++ "} -> " ++
    showLamExprI expr
showLamExprI (AppI _ exprs) = 
    let showArg v@(AppI _ _) = "(" ++ showLamExprI v ++ ")"
        showArg v@(LamI _ _ _ _) = "(" ++ showLamExprI v ++ ")"
        showArg v = showLamExprI v
    in intercalate " " $ map showArg exprs

-- A simpler version, don't delve into nested lambdas.
shortShowHelper :: LamExprI -> String
shortShowHelper (UbSymI name sym) = 
    "(" ++ name ++ "," ++ show sym ++ ")"
shortShowHelper (BSymI sym) = sym
shortShowHelper (LamI n vs _ _) = 
    "\\Lam [" ++ show n ++ "] {" ++ intercalate "," vs ++ "}"
shortShowHelper (AppI _ exprs) = 
    let showArg v@(AppI _ _) = "(" ++ shortShowHelper v ++ ")"
        showArg v@(LamI _ _ _ _) = "(" ++ shortShowHelper v ++ ")"
        showArg v = shortShowHelper v
    in intercalate " " $ map showArg exprs

shortShow :: LamExprI -> String
shortShow (LamI n vs xs expr) = 
    "Lam [" ++ show n ++ "] {" ++ intercalate "," vs ++ "} \\n {" ++
    intercalate " " xs ++ "} -> " ++
    shortShowHelper expr
shortShow v = shortShowHelper v
