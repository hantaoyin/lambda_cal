module LamParser (Symbol, LamExpr(Sym, Lam, App), readExpr) where
import Text.Parsec
import Text.Parsec.String

type Symbol = String

data LamExpr = Sym Symbol
            | Lam Symbol LamExpr -- 1 parameter functions
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
    cs <- many1 (oneOf "+-*/<>=!@%&_?\"" <|> alphaNum)
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
