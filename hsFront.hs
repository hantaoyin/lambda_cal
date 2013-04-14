{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import StringQQ

----------------------------------------------------------------------
ren :: Int -> Symbol
ren n = "v" ++ show n

lookupSym :: [(Symbol, Symbol)] -> Symbol -> Symbol
lookupSym [] s = "(Sym " ++ "\"" ++ s ++ "\")"
lookupSym ((x,v):xs) s
    | s == x = v
    | otherwise = lookupSym xs s

analyze' :: [(Symbol,Symbol)] -> Int -> LamExpr -> String
analyze' env _ (Sym s) = lookupSym env s
analyze' env n (Lam s e) = 
    let s' = ren n
        new_env = (s,s'):env
    in "(Lam (\\" ++ s' ++ " -> " ++ analyze' new_env (n+1) e ++ "))"
analyze' env n (App f p) =
    let fn = analyze' env (n+1) f
        pn = analyze' env (n+1) p
    in "(apply " ++ fn ++ " " ++ pn ++ ")"

analyze :: LamExpr -> String
analyze = analyze' [] 0

----------------------------------------------------------------------
headerStr :: String
headerStr = [stringQQ|
module Main where

type Symbol = String

data NForm = Sym Symbol
           | Lam (NForm -> NForm)
           | App NForm NForm

apply :: NForm -> NForm -> NForm
apply (Lam f) p = f p
apply f p = App f p

showNF :: NForm -> String
showNF (Sym s) = s
showNF (Lam f) = "#FUNC"
--showNF (x:xs) (Lam f) = "\\" ++ x ++ " -> " ++ (showNF xs $ f $ Sym x)
showNF (App a b) = let showA a@(Lam _) = "(" ++ showNF a ++ ")"
                       showA a = showNF a
                       showB b@(App _ _) = "(" ++ showNF b ++ ")"
                       showB b = showNF b
                   in showA a ++ " " ++ showB b

instance Show NForm where show = showNF

main = putStrLn $ show $ expr
|]

main :: IO ()
main = 
    do val <- getContents
       putStrLn headerStr
       putStrLn $ "expr = " ++ (analyze $ readExpr val)
