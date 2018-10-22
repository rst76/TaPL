type Info = String

type Context = [(String, Binding)]

data Binding = NameBind deriving Show

data Term
  = TmVar Info Int Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving Show

i = TmVar "" 0 1
k = TmVar "" 1 2
s = TmApp "" (TmApp "" (TmVar "" 2 3) (TmVar "" 0 3)) (TmApp "" (TmVar "" 1 3) (TmVar "" 0 3))

isNameBound :: Context -> String -> Bool
isNameBound ctx x = any ((== x) . fst) ctx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = ((x, NameBind) : ctx, x)

ctxLength :: Context -> Int
ctxLength = length

index2name :: Info -> Context -> Int -> String
index2name fi ctx n = fst (ctx !! n)

showTm :: Context -> Term -> String
showTm ctx (TmAbs fi x t1) = "(λ" ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = pickFreshName ctx x
showTm ctx (TmApp fi t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (TmVar fi x n)
  | ctxLength ctx == n = index2name fi ctx x
  | otherwise = "[bad index]"

termShift :: Int -> Term -> Term
termShift d = walk 0
  where
  walk c (TmVar fi x n)
    | x >= c = TmVar fi (x+d) (n+d) 
    | otherwise = TmVar fi x (n+d)
  walk c (TmAbs fi x t1) = TmAbs fi x (walk (c+1) t1)
  walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where
  walk c (TmVar fi x n)
    | x == j+c = termShift c s
    | otherwise = TmVar fi x n
  walk c (TmAbs fi x t1) = TmAbs fi x (walk (c+1) t1)
  walk c (TmApp fi t1 t2) = TmApp fi (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal _ TmAbs {} = True
isVal _ _ = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmApp fi (TmAbs _ x t12) v2)
  | isVal ctx v2 = Just (termSubstTop v2 t12)
eval1 ctx (TmApp fi v1 t2)
  | isVal ctx v1 = TmApp fi v1 <$> eval1 ctx t2
eval1 ctx (TmApp fi t1 t2) = flip (TmApp fi) t2 <$> eval1 ctx t1

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 ctx t)

main :: IO ()
main = do
  let ctx = [(s, NameBind) | s <- ["x", "y", "a", "b"]]
  putStrLn $ showTm [] $ TmAbs "" "a" i
  putStrLn $ showTm [] $ TmAbs "" "a" $ TmAbs "" "b" k
  putStrLn $ showTm [] $ TmAbs "" "a" $ TmAbs "" "b" $ TmAbs "" "c" s
  putStrLn $ showTm [] $ eval [] $ TmAbs "" "a" $ TmApp "" (TmApp "" (TmApp "" s k) k) i

  putStrLn $ showTm ctx $ TmVar "" 0 4 -- x
  putStrLn $ showTm ctx $ TmVar "" 1 4 -- y
  putStrLn $ showTm ctx $ TmVar "" 2 4 -- a
  putStrLn $ showTm ctx $ TmVar "" 3 4 -- b
  putStrLn $ showTm ctx $ termShift 1 $ TmVar "" 0 3 -- y
  putStrLn $ showTm ctx $ termShift 1 $ TmVar "" 1 3 -- a
  putStrLn $ showTm ctx $ termShift 1 $ TmVar "" 2 3 -- b
  putStrLn $ showTm ctx $ termShift 2 $ TmVar "" 0 2 -- a
  putStrLn $ showTm ctx $ termShift 2 $ TmVar "" 1 2 -- b
