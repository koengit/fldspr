module Storable where

import Expr
import Thread

--------------------------------------------------------------------------------

class Storable a where
  sizeof :: a -> E Int
  assign :: Loc -> a -> Program
  readd  :: Loc -> a

instance Storable (E a) where
  sizeof _        = E (Num 1)
  assign lc (E x) = lc =: (E x)
  readd lc        = E (Lc lc)

--------------------------------------------------------------------------------

