module Fldspr where

import Expr
import Thread
import Storable

--------------------------------------------------------------------------------

type Pull a = (E Int, E Int -> a)

mkPull :: Name -> Pull (E Int)
mkPull arr = (var (arr ++ "_size"), \i -> E (Lc (Ar arr i)))

(...) :: E Int -> E Int -> Pull (E Int)
a ... b = (b .- a, \i -> a .+ i)

mapp :: (a->b) -> Pull a -> Pull b
mapp f (n, h) = (n, f . h)

zippWith :: (a->b->c) -> Pull a -> Pull b -> Pull c
zippWith f (n,h) (m,g) = (n .^ m, \i -> f (h i) (g i))

pair :: Pull a -> Pull (a,a)
pair (n,h) = (n ./ num 2, \i -> (h (i .* num 2), h ((i .* num 2) .+ num 1)))

revp :: Pull a -> Pull a
revp (n,h) = (n, \i -> h (n.-(i.+num 1)))

(!) :: Pull a -> E Int -> a
(_, h) ! i = h i

compress :: Pull (E Int) -> Pull (E Int)
compress = mapp (uncurry (.+)) . pair 

--------------------------------------------------------------------------------

type Push a = (E Int, ((E Int,a) -> Program) -> Program)

-- back, forth

push :: Pull a -> Push a
push (n, h) = (n, \k -> par n (\i -> k (i, h i)))

withPush :: Storable a => Push a -> (Pull a -> Program) -> Program
withPush (n, p) w =
  withType $ \a ->
    let _ = p (\(_,b) -> (b `asTypeOf` a) `seq` (undefined :: Program)) in
      alloc (n .* sizeof a) $ \lc ->
        store (n,p) lc .>> w (n, \i -> readd (lc i)) 

store :: Storable a => Push a -> (E Int -> Loc) -> Program
store (n, p) lc = p (\(i,x) -> lc i `assign` x)

-- basic

maph :: (a->b) -> Push a -> Push b
maph f (n, p) = (n, \k -> p (\(i,x) -> k (i,f x)))

unpair :: Push (a,a) -> Push a
unpair (n,p) = (n .* num 2, \k -> p (\(i,(x,y)) -> k (i.*num 2,x) .|| k ((i.*num 2).+num 1,y)))

revh :: Push a -> Push a
revh (n,p) = (n, \k -> p (\(i,x) -> k (n.-(i.+num 1),x)))

--------------------------------------------------------------------------------

withType :: (a -> b) -> b
withType f = f (error "do not look at the value of type variables!")

--------------------------------------------------------------------------------

mkLoc :: Name -> (E Int -> Loc)
mkLoc arr = \i -> Ar arr i

--------------------------------------------------------------------------------

main =
--  putStr $ unlines $ showP 0 $ prog $ a
--  putStr $ unlines $ concatMap (showP 0) $ Program $ prog $ a
--  putStr $ unlines $ pPrograms "the_Program" $ prog $ a
  writeFile "pthreads_include.c" $ unlines $ showpThread "cykel" $ prog1 $ a
--  putStr $ unlines $ showpThread "apa" $ prog $ a

a = mkPull "arr"
b = mkLoc "result"

-- f = rev . unpair . rev . pair

f :: Pull (E Int) -> Push (E Int)
f = revh . unpair . maph tung . push . revp . pair

tung (x,y) = (ff x .+ y, x .* ff y)

prog1 a = f a `store` b

prog2 a = withPush (unpair . push . pair $ a) $ \a' ->
            push (revp a') `store` b




