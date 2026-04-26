module Main (main) where

data Prop
  = A
  | B
  | C
  | Prod Prop Prop
  deriving (Show, Eq)

data Proof
  = PA
  | PB
  | PC
  | Pair Proof Proof
  | Fst Proof
  | Snd Proof
  deriving (Show, Eq)

prove :: [(Proof, Prop)] -> Prop -> Maybe Proof
prove g p =
  case [m | (m, p') <- g, p == p'] of
    (m:ms) -> Just m
    _ ->
      case p of
        Fst p -> 
        Prod a b -> do
          
          
      
  

main :: IO ()
main = putStrLn "Hello, Haskell!"
