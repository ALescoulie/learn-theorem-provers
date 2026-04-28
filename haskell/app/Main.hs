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

idRule :: Ctx -> Prop -> Maybe Proof
idRule g p = lookup p (map swap g)

andRight :: Ctx -> Prop -> Maybe Proof
andRight g (Prod a b) = do
  m <- prove g a
  n <- prove g b
  return $ Pair m n
andRight _ _ = Nothing

funRight :: Ctx -> Prop -> Maybe Proof
funRight g (Fun a b) = do
  let x = "x" ++ show (length g)
  m <- prove ((Var x,a):g) b
  return $ Lam x m
funRight _ _ = Nothing

rightRules :: Ctx -> Prop -> Maybe Proof
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

andLeft1 :: Ctx -> Prop -> Maybe Proof
andLeft1 g c = do
  ((m, Prod a b), g') <- grab (isProd.snd) g
  prove ((Fst m, a):g') c

andLeft2 :: Ctx -> Prop -> Maybe Proof
andLeft2 g c = do
  ((m, Prod a b), g') <- grab (isProd.snd) g
  prove ((Snd m, b):g') c

isFun :: Prop -> Bool
isFun (Fun _ _) = True
isFun _ = False

funLeft :: Ctx -> Prop -> Maybe Proof
funLeft g c = do
  ((m, Fun a b), g') <- grab (isFun.snd) g
  n <- prove g' a
  p <- prove ((App m n, b) : g') c
  return p

leftRules :: Ctx -> Prop -> Maybe Proof
leftRules g p =
      andLeft1 g p
  <|> andLeft2 g p
  <|> funLeft g p

prove :: Ctx -> Prop -> Maybe Proof
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

