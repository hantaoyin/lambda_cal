{-# LANGUAGE QuasiQuotes #-}
module Main where
import Text.Parsec
import Text.Parsec.String
import StringQQ

type Symbol = String

data LamExpr = Sym Symbol
            | Lam String LamExpr -- 1 parameter functions
            | App LamExpr LamExpr

instance Show LamExpr where show = showVal

showVal :: LamExpr -> String
showVal (Sym s) = s
showVal (Lam s v) = "\\" ++ s ++ " -> " ++ showVal v
showVal (App a b) = showFun a ++ " " ++ showArg b
        where showArg v@(App _ _) = "(" ++ showVal v ++ ")"
              showArg v = showVal v
              showFun v@(Lam _ _) = "(" ++ showVal v ++ ")"
              showFun v = showVal v
        
readExpr :: String -> LamExpr
readExpr str = case parse pLamExpr (take 10 str) str of
    Left e -> error $ show e
    Right expr -> expr

pLamExpr :: Parser LamExpr
pLamExpr = chainl1 (try $ pSpaces >> (pParentheseExpr <|> pVar <|> pLam)) (return App)

pSpaces :: Parser ()
pSpaces = spaces >> (try pComment <|> return ())

pComment :: Parser ()
pComment = do
    _ <- string ";" <|> try (string "--")
    _ <- many $ noneOf "\n"
    _ <- char '\n'
    pSpaces

pParentheseExpr :: Parser LamExpr
pParentheseExpr = do
    _ <- char '('
    expr <- pLamExpr
    pSpaces
    _ <- char ')'
    return expr

pVar :: Parser LamExpr
pVar = do
    x <- pName
    return $ Sym x

pName :: Parser Symbol
pName = do
    cs <- many1 (oneOf "+-*/<>=!@%&_?" <|> alphaNum)
    if cs `elem` ["->", "--"]
    then fail ""
    else return cs

pLam :: Parser LamExpr
pLam = do
    _ <- char '\\'
    pSpaces
    xs <- many1 (try $ pSpaces >> pName)
    pSpaces
    _ <- try $ string "->"
    pSpaces
    expr <- pLamExpr
    return $ genLamList xs expr

genLamList :: [Symbol] -> LamExpr -> LamExpr
genLamList [] expr = expr
genLamList (x:xs) expr = Lam x $ genLamList xs expr

--------------------------------------------------------------------------------
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
showNF (Lam _) = "#FUNC"
showNF (App a b) = showNF a ++ " " ++ showArg b
       where showArg v@(App _ _) = "(" ++ showNF v ++ ")"
             showArg v = showNF v

instance Show NForm where show = showNF

main = putStrLn $ show $ expr
|]

main :: IO ()
main = 
    do val <- getContents
       putStrLn headerStr
       putStrLn $ "expr = " ++ (analyze $ readExpr val)
