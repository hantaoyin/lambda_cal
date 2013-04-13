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
-- Lambda expressions in the De Bruijn notation.
data LamDeBruijn = UnboundSym String
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
transformToDB :: [Symbol] -> LamExpr -> LamDeBruijn
transformToDB env (Sym sym) = let dbv = lookupSym env sym
                              in if dbv < 0
                                 then UnboundSym sym
                                 else BoundSym dbv
transformToDB env (Lam sym expr) = LamDB (transformToDB (sym:env) expr)
transformToDB env (App f p) = AppDB (transformToDB env f) (transformToDB env p)

--------------------------------------------------------------------------------
type Env = [NForm]

newEnv :: Env
newEnv = []

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

addBinding :: NForm -> Env -> Env
addBinding nf old_env = nf:old_env

lookupVar :: Int -> Env -> NForm
lookupVar n xs = xs !! n

--------------------------------------------------------------------------------
-- data LamDeBruijn = UnboundSym String
--                  | BoundSym Int
--                  | LamDB LamDeBruijn -- 1 parameter functions, we no
--                                    -- longer need to specify the dummy
--                                    -- parameter.
--                  | AppDB LamDeBruijn LamDeBruijn

analyzeLambda :: LamDeBruijn -> NForm -> Env -> NForm
analyzeLambda expr nf env = analyze expr $ addBinding nf env

analyzeApp :: LamDeBruijn -> LamDeBruijn -> Env -> NForm
analyzeApp a b env = let an = analyze a env
                         bn = analyze b env
                         apply (NLam nfunc) nparam = nfunc nparam
                         apply x@_ y = NApp x y
                     in apply an bn

analyze :: LamDeBruijn -> Env -> NForm
analyze (UnboundSym s) _ = NSym s
analyze (BoundSym n) env = lookupVar n env
analyze (LamDB v) env = NLam (\nf -> analyzeLambda v nf env)
analyze (AppDB a b) env = analyzeApp a b env

main :: IO ()
main = do
     val <- getContents
     -- putStrLn $ show $ transformToDB [] $ readExpr val
     -- putStrLn ""
     putStrLn $ show $ analyze (transformToDB [] $ readExpr val) newEnv
