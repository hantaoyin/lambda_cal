{-# LANGUAGE QuasiQuotes #-}
module Main where
import LamParser
import Transformer
import StringQQ

----------------------------------------------------------------------
compileHelper :: [Symbol] -> LamExpr -> String
compileHelper env (Sym s) =
    if elem s env
    then s
    else "(Sym " ++ "\"" ++ s ++ "\")"

compileHelper env (Lam s e) = "(Lam (\\" ++ s ++ " -> " ++ compileHelper (s:env) e ++ "))"
compileHelper env (App f p) =
    let fn = compileHelper env f
        pn = compileHelper env p
    in "(apply " ++ fn ++ " " ++ pn ++ ")"

compile :: LamExpr -> String
compile = compileHelper [] . rename

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
       putStrLn $ "expr = " ++ (compile $ readExpr val)
