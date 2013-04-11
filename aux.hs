module Aux where

type Symbol = String

data NForm = Sym Symbol
           | Lam (NForm -> NForm)
           | App NForm NForm

apply :: NForm -> NForm -> NForm
apply (Lam f) p = f p
apply f p = App f p

showNF :: NForm -> String
showNF (Sym s) = s
showNF (Lam _) = "#FUNC"
showNF (App a b) = showNF a ++ " " ++ showArg b
       where showArg v@(App _ _) = "(" ++ showNF v ++ ")"
             showArg v = showNF v

instance Show NForm where show = showNF
