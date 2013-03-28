{-# LANGUAGE ExistentialQuantification #-}
module Main where
import Text.Parsec
import Text.Parsec.String

type Name = String
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

pName :: Parser Name
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

genLamList :: [Name] -> LamExpr -> LamExpr
genLamList [] expr = expr
genLamList (x:xs) expr = Lam x $ genLamList xs expr

--------------------------------------------------------------------------------
data Env = Env [(Symbol, NForm)]

data NForm = NSym Symbol
           | NLam (NForm -> NForm)
           | NApp NForm NForm

instance Show NForm where show = showNF

showNF :: NForm -> String
showNF (NSym s) = s
showNF (NLam _) = "#FUNC"
showNF (NApp a b) = showNF a ++ " " ++ showArg b
       where showArg v@(NApp _ _) = "(" ++ showNF v ++ ")"
             showArg v = showNF v

newEnv :: Env
newEnv = Env []

addBinding :: Symbol -> NForm -> Env -> Env
addBinding s nf (Env old_env) = Env ((s, nf):old_env)

lookupVar :: Symbol -> Env -> NForm
lookupVar s (Env []) = NSym s
lookupVar s (Env ((a,v):es))
          | s == a = v
          | otherwise = lookupVar s (Env es)

--------------------------------------------------------------------------------
analyzeLambda :: Symbol -> LamExpr -> NForm -> Env -> NForm
analyzeLambda s expr nf env = analyze expr $ addBinding s nf env

analyzeApp :: LamExpr -> LamExpr -> Env -> NForm
analyzeApp a b env = let an = analyze a env
                         bn = analyze b env
                         apply (NLam nfunc) nparam = nfunc nparam
                         apply x@_ y = NApp x y
                     in apply an bn

analyze :: LamExpr -> Env -> NForm
analyze (Sym s) env = lookupVar s env
analyze (Lam s v) env = NLam (\nf -> analyzeLambda s v nf env)
analyze (App a b) env = analyzeApp a b env

main :: IO ()
main = do 
     val <- getContents
     putStrLn $ showNF $ analyze (readExpr val) newEnv
