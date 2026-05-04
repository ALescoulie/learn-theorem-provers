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

data MetaProof
  = ID Proof
  | ANDR MetaProof MetaProof
  | FUNR String MetaProof
  | ANDL1 Proof MetaProof
  | ANDL2 Proof MetaProof
  | FUNL Proof MetaProof MetaProof
  deriving (Show, Eq)

type Ctx = [(Proof, Prop)]

idRule :: Ctx -> Prop -> [MetaProof]
idRule g p = [ID m | (m, p') <- g, p' == p]

andRight :: Ctx -> Prop -> [MetaProof]
andRight g (Prod a b) = do
  m <- prove g a
  n <- prove g b
  return $ ANDR m n
andRight _ _ = []

funRight :: Ctx -> Prop -> [MetaProof]
funRight g (Fun a b) = do
  let x = "x" ++ show (length g)
  m <- prove ((Var x,a):g) b
  return $ FUNR x m
funRight _ _ = []

rightRules :: Ctx -> Prop -> [MetaProof]
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

andLeft1 :: Ctx -> Prop -> [MetaProof]
andLeft1 g c = 
  case grab (isProd.snd) g of
    Just ((m, Prod a b), g') -> do
      d <- prove ((Fst m, a):g') c
      return $ ANDL1 m d
    _ -> []

andLeft2 :: Ctx -> Prop -> [MetaProof]
andLeft2 g c =
  case grab (isProd.snd) g of
    Just ((m, Prod a b), g') -> do
      d <- prove ((Snd m, b):g') c
      return $ ANDL2 m d
    _ -> []

isFun :: Prop -> Bool
isFun (Fun _ _) = True
isFun _ = False

funLeft :: Ctx -> Prop -> [MetaProof]
funLeft g c =
  case grab (isFun.snd) g of
    Just ((m, Fun a b), g') -> do
      d <- prove g' a
      e <- prove ((App m (extractProof d), b) : g') c
      return $ FUNL m d e
    _ -> []

leftRules :: Ctx -> Prop -> [MetaProof]
leftRules g p =
      andLeft1 g p
  <|> andLeft2 g p
  <|> funLeft g p

prove :: Ctx -> Prop -> [MetaProof]
prove g p =
  idRule g p <|> rightRules g p <|> leftRules g p

extractProof :: MetaProof -> Proof
extractProof (ID m) = m
extractProof (ANDR d e) = Pair (extractProof d) (extractProof e)
extractProof (FUNR x d) = Lam x (extractProof d)
extractProof (ANDL1 _ d) = extractProof d
extractProof (ANDL2 _ d) = extractProof d
extractProof (FUNL _ _ e) = extractProof e

main :: IO ()
main = do
--  print $ prove [] A
--  print $ prove [(Var "x", A)] A
--  print $ prove [(Var "x", A), (Var "y", B)] (Prod A B)
--  print $ prove [(Var "p", Prod A B)] B
--  print $ prove [(Var "p", Prod A B)] (Prod B A)
--  print $ prove [] (Fun A A)
--  print $ prove [(Var "x", B)] (Fun A B)
--  print $ prove [(Var "f", Fun A B), (Var "x", A)] B
--  print $ prove [] (Fun A (Fun (Fun A B) B))
--  print $ prove [(Var "x", Prod A (Prod B C))] (Prod (Prod A B) C)
--  print $ prove [(Var "f", Fun (Prod A B) C)] (Fun A (Fun B C))
--  mapM_ print $ prove [(Var "x", A), (Var "y", A)] A
  mapM_ print $ prove [(Var "x", Prod A B), (Var "y", Fun B A)] A

