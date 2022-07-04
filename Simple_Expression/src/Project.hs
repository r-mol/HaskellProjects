{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Project where

import Data.Functor
import Data.List
import Data.Maybe



-- | A simple expression with variables.
data Expr a
  = Lit Integer             -- ˆ Integer literal.
  | Var a                   -- ˆ Variable
  | Add (Expr a) (Expr a)   -- ˆ Addition.
  | Mul (Expr a) (Expr a)   -- ˆ Multiplication.
  deriving Show

-- | Num instance for nice syntax.
instance Num (Expr a) where 
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2 
  fromInteger n = Lit n

x = Var "x"
y = Var "y"
z = Var "z"

sampleExpr1 :: Expr String
sampleExpr1 = (Add (Var "x") (Var "y"))


-- | Evaluate an expression with all variables instantiated.
eval :: Expr Integer -> Integer
eval (Lit n)     = n
eval (Var n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- | Evaluate an expression using an associative list
-- to lookup variable values and a default value
-- (in case there is no value in associative list). 
evalWith :: Eq var => Integer -> [(var, Integer)] -> Expr var -> Integer
evalWith def list expp = eval' expp list
   where
    -- | Evaluate an expression with all variables instantiated.
    
    eval' (Lit n) _xs    = n
    eval' (Var n) xs    
      | lookupp n xs == Nothing = def
      | otherwise = fromJust (lookupp n xs)
    eval' (Add e1 e2) xs = eval' e1 xs + eval' e2 xs
    eval' (Mul e1 e2) xs = eval' e1 xs * eval' e2 xs

lookupp :: (Foldable t, Eq a) => a -> t(a,b) -> Maybe b
lookupp k xs = fmap snd (find (\(k', _) -> k == k') xs)


-- | Display an expression with variables.
display :: Expr String -> String
display (Lit n)     = show n
display (Var s)     = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"


main :: IO ()
main = print("evalWith 0 vars ((x + y)^2 + z))")