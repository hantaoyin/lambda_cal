module Main where
import LamParser
import Transformer

----------------------------------------------------------------------
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

----------------------------------------------------------------------
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
     putStrLn $ show $ toLamInfo $ toLamExprF $ readExpr val
     putStrLn $ show $ analyze (toDeBruijn $ readExpr val) newEnv
