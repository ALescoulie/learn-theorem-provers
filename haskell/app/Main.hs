module Main (main) where

import Control.Applicative
import Data.Maybe
import Data.Tuple

data Prop
  = A
  | B
  | C
  | Prod Prop Prop
  | Fun Prop Prop
  deriving (Show, Eq)

data Proof
  = Var String
  | Pair Proof Proof
  | Fst Proof
  | Snd Proof
  | Lam String Proof
  | App Proof Proof
  deriving (Show, Eq)

type Ctx = [(Proof, Prop)]

idRule :: Ctx -> Prop -> [Proof]
idRule g p = [m | (m, p') <- g, p' == p]

andRight :: Ctx -> Prop -> [Proof]
andRight g (Prod a b) = do
  m <- prove g a
  n <- prove g b
  return $ Pair m n
andRight _ _ = []

funRight :: Ctx -> Prop -> [Proof]
funRight g (Fun a b) = do
  let x = "x" ++ show (length g)
  m <- prove ((Var x,a):g) b
  return $ Lam x m
funRight _ _ = []

rightRules :: Ctx -> Prop -> [Proof]
rightRules g a =
      andRight g a
  <|> funRight g a

grab :: (a -> Bool) -> [a] -> Maybe (a, [a])
grab p [] = Nothing
grab p (x:xs)
  | p x = return (x,xs)
  | otherwise = do
      (y,ys) <- grab p xs
      return (y, x:ys)

isProd :: Prop -> Bool
isProd (Prod _ _) = True
isProd _ = False

andLeft1 :: Ctx -> Prop -> [Proof]
andLeft1 g c = 
  case grab (isProd.snd) g of
  Just ((m, Prod a b), g') -> do
    prove ((Fst m, a):g') c
  _ -> []

andLeft2 :: Ctx -> Prop -> [Proof]
andLeft2 g c =
  case grab (isProd.snd) g of
    Just ((m, Prod a b), g') -> do
      prove ((Snd m, b):g') c
    _ -> []

isFun :: Prop -> Bool
isFun (Fun _ _) = True
isFun _ = False

funLeft :: Ctx -> Prop -> [Proof]
funLeft g c =
  case grab (isFun.snd) g of
    Just ((m, Fun a b), g') -> do
      n <- prove g' a
      prove ((App m n, b) : g') c
    _ -> []

leftRules :: Ctx -> Prop -> [Proof]
leftRules g p =
      andLeft1 g p
  <|> andLeft2 g p
  <|> funLeft g p

prove :: Ctx -> Prop -> [Proof]
prove g p =
  idRule g p <|> rightRules g p <|> leftRules g p

main :: IO ()
main = do
  print $ prove [] A
  print $ prove [(Var "x", A)] A
  print $ prove [(Var "x", A), (Var "y", B)] (Prod A B)
  print $ prove [(Var "p", Prod A B)] B
  print $ prove [(Var "p", Prod A B)] (Prod B A)
  print $ prove [] (Fun A A)
  print $ prove [(Var "x", B)] (Fun A B)
  print $ prove [(Var "f", Fun A B), (Var "x", A)] B
  print $ prove [] (Fun A (Fun (Fun A B) B))
  print $ prove [(Var "x", Prod A (Prod B C))] (Prod (Prod A B) C)
  print $ prove [(Var "f", Fun (Prod A B) C)] (Fun A (Fun B C))
  print $ prove [(Var "x", A), (Var "y", A)] A
  print $ prove [(Var "x", Prod A B), (Var "y", Fun B A)] A

