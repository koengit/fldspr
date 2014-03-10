module Expr where

--------------------------------------------------------------------------------

type Name
  = String

--------------------------------------------------------------------------------

data Loc
  = Vr Name
  | Ar Name (E Int)
 deriving Eq

instance Show Loc where
  show (Vr v)   = v
  show (Ar a i) = a ++ "[" ++ show i ++ "]"

--------------------------------------------------------------------------------

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Max Expr Expr
  | Min Expr Expr
  | Expr :< Expr
  | Cond Expr Expr Expr
  | F Expr
  | Lc Loc
  
  -- propagation
  | AtMost Expr Expr
 deriving Eq 

instance Show Expr where
  show (Num x)      = show x
  show (Add a b)    = show1 a ++ "+" ++ show1 b
  show (Mul a b)    = show1 a ++ "*" ++ show1 b
  show (Div a b)    = show1 a ++ "/" ++ show1 b
  show (Mod a b)    = show1 a ++ "%" ++ show1 b
  show (Max a b)    = "max(" ++ show a ++ "," ++ show b ++ ")"
  show (Min a b)    = "min(" ++ show a ++ "," ++ show b ++ ")"
  show (a :< b)     = show1 a ++ " < " ++ show1 b
  show (Cond c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b 
  show (F a)        = "f(" ++ show a ++ ")"
  show (Lc l)       = show l

  --show (AtMost a b) = show a ++ " /* < " ++ show b ++ " */"
  show (AtMost a b) = show a

show1 :: Expr -> String
show1 a | isAtom a  = show a
        | otherwise = "(" ++ show a ++ ")"
 where
  isAtom (Num x) = x>=0
  isAtom (F _)   = True
  isAtom (Lc _)  = True
  isAtom _       = False

--------------------------------------------------------------------------------

newtype E a = E{ unE :: Expr }
 deriving Eq

instance Show (E a) where
  show (E a) = show a

var :: Name -> E a
var v = E (Lc (Vr v))

num :: Int -> E Int
num n = E (Num n)

ff (E x) = E (F x)

--------------------------------------------------------------------------------

(.+), (.-), (.*), (./), (.%), (.^), (.\/) :: E Int -> E Int -> E Int
E (Num a) .+ E (Num b)           = E (Num (a+b))
E (Num 0) .+ y                   = y
x         .+ E (Num 0)           = x
E (Num a) .+ E ((Add (Num b) x)) = E (Num (a+b)) .+ E x
x         .+ E (Num b)           = E (Num b) .+ x
E x       .+ E y                 = E (Add x y)

x .- y = x .+ (num (-1) .* y)

E (Num a) .* E (Num b)           = E (Num (a*b))
E (Num 0) .* y                   = E (Num 0)
x         .* E (Num 0)           = E (Num 0)
E (Num 1) .* y                   = y
x         .* E (Num 1)           = x
E (Num a) .* E ((Mul (Num b) x)) = E (Num (a*b)) .* E x
x         .* E (Num b)           = E (Num b) .* x
E x       .* E y                 = E (Mul x y)

E (Num a) ./ E (Num b) | b /= 0  = E (Num (a `div` b))
E (Mul a b) ./ E c | c == b      = E a
                   | c == a      = E b
E (Num 0) ./ y                   = E (Num 0)
x         ./ E (Num 1)           = x
E x       ./ E y                 = E (Div x y)

E (Num a) .% E (Num b) | b /= 0  = E (Num (a `mod` b))
E (Num 0) .% y                   = E (Num 0)
x         .% E (Num 1)           = E (Num 0)
E x       .% E y                 = E (Mod x y)

E (Num a) .^ E (Num b)           = E (Num (a `max` b))
E x       .^ E y | x == y        = E x
E x       .^ E y                 = E (Max x y)

E (Num a) .\/ E (Num b)           = E (Num (a `min` b))
E x       .\/ E y | x == y        = E x
E x       .\/ E y                 = E (Min x y)

(.<) :: E Int -> E Int -> E Bool
E (AtMost _ (Num a))  .< E (Num b) | a <  b = E (Num 1)
E (AtMost _ a)        .< E b       | a == b = E (Num 1)
E (Num a)   .< E (Num b) = E (Num (if (a < b) then 1 else 0))
E (Div a b) .< y         = E a .< (E b .* y)
E x         .< E y       = E (x :< y)

(?) :: E Bool -> (E a, E a) -> E a
c         ? (x,y) | unE x == unE y = x
c         ? (x,y) | c == E (Num 0) = y
E (Num _) ? (x,y)                  = x
E c       ? (E x,E y)              = E (Cond c x y)

atMost :: E Int -> E Int -> E Int
E a `atMost` E b = E (AtMost a b)

--------------------------------------------------------------------------------

