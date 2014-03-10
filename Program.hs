module Program where

import Expr
import Data.List( intersperse )

--------------------------------------------------------------------------------

data Program
  = Assign Loc (E Int)
  | Program :>> Program
  | Program :|| Program
  | If (E Bool) Program
  | Par (E Int) (E Int -> Program)
  | Alloc (E Int) ((E Int -> Loc) -> Program)
  | Skip

iff :: E Bool -> Program -> Program
iff (E (Num a)) p | a == 0    = Skip
                  | otherwise = p
iff c           p             = If c p

instance Show Program where
  show p = unlines (showP 0 p)

showP :: Int -> Program -> [String]
showP t (Assign loc a) = [ show loc ++ " = " ++ show a ++ ";" ]
showP t (p :>> q)      = showP t p ++ showP t q
showP t (p :|| q)      = prep "{ " ps ++ finp " }" (prep "# " qs)
 where
  ps = showP t p
  qs = showP t q
showP t (If c p)       = [ "if (" ++ show c ++ ")" ]
                      ++ prep "{ " ps
                      ++ [ "}" ]
 where
  ps = showP t p
showP t (Par e pf)     = [ "par (" ++ show i ++ " < " ++ show e ++ ")" ]
                      ++ prep "{ " ps
                      ++ [ "}" ]
 where
  i  = var ("_tid" ++ show t)
  ps = showP (t+1) (pf i)
showP t (Alloc n pf)   = [ show m ++ " = (int*)malloc(" ++ show n ++ " * sizeof(int));" ]
                      ++ prep "{ " ps
                      ++ [ "}"
                         , "free(" ++ show m ++ ");"
                         ]
 where
  s  = "_mem" ++ show t
  m  = var s
  ps = showP (t+1) (pf (\i -> Ar s i))
showP t Skip           = []

prep :: String -> [String] -> [String]
prep p []     = [p ++ "0;"]
prep p (x:xs) = (p ++ x) : map (replicate k ' ' ++) xs
 where
  k = length p

finp :: String -> [String] -> [String]
finp p [] = ["0;" ++ p]
finp p xs = init xs ++ [last xs ++ p]

--------------------------------------------------------------------------------

{-
thread :: (E Int -> Loc) -> Program -> (Maybe (E Int), [Program])
thread mem p@(Assign _ _) = (Nothing, [p])

thread mem (p :>> q) = (m1 +^+ m2, ps ++ qs)
 where
  (m1,ps) = thread mem p
  (m2,qs) = thread mem q

thread mem (If e p) = (m, map (pushIn (If e)) ps)
 where
  (m,ps) = thread mem p

thread mem Skip = (Nothing, [])

thread mem (p :|| q) = (m1 +++ m2, zipWith' id merge id ps qs)
 where
  (m1,ps) = thread mem p
  (m2,qs) = thread (\i -> mem (m1 .+ i)) q

thread mem (Par m pf)    =
  [ Par n (\z ->
      let p = thread (pf (zMod z)) !! k in
        case p of
          Par _ pf -> pf (zDiv z)
          _        -> p
    )
  | (k,p) <- [0..] `zip` thread (pf (var "?"))
  , let (n, zMod, zDiv) =
          case p of
            Par n _ -> (n .* m, (.% m), (./ m))
            _       -> (m, id, id)
  ]
-- todo: take care of memory allocation

pushIn :: (Program -> Program) -> Program -> Program
pushIn f (Par m pf) = Par m (\i -> f (pf i))
pushIn f p          = f p

merge :: Program -> Program -> Program
merge (Par n pf) (Par m qf)
  | n == m    = Par n (\i -> pf i :>> qf i)
  | otherwise = Par (n .^ m) (\i -> iff (i .< n) (pf i) :>> iff (i .< m) (qf i))
merge p@(Par _ _) q = merge p (parify q)
merge p q@(Par _ _) = merge (parify p) q
merge p q           = p :>> q

parify :: Program -> Program
parify p@(Par _ _) = p
parify p           = Par (num 1) (\_ -> p)
-}

--------------------------------------------------------------------------------

{-
pthreads :: String -> Program -> [String]
pthreads s p =
  [ "void *" ++ s ++ "(void *" ++ arg ++ ") {"
  , "  int " ++ threadId ++ " = (int)" ++ arg ++ ";"
  , "  int " ++ taskId ++ ";"
  , "  int " ++ blockSize ++ ";"
  , ""
  ] ++
  map ("  " ++) ls ++
  [ "}"
  ]
 where
  arg        = "arg" -- "__arg__"
  threadId   = "tid" -- "__THREAD_ID__"
  taskId     = "i" -- "__TASK_ID__"
  blockSize  = "block" -- "__BLOCK_SIZE__"
  numThreads = "__NUM_THREADS__"

  ls =
    concat $
    intersperse [ "sync();", "" ]
    [ [ blockSize ++ " = " ++ show ((n .+ (var numThreads .- num 1)) ./ var numThreads) ++ ";"
      , "for ( " ++ taskId ++ " = " ++ blockSize ++ " * " ++ threadId ++ "; "
                 ++ taskId ++ " < " ++ blockSize ++ " * (" ++ threadId ++ " + 1) && "
                 ++ taskId ++ " < " ++ show n ++ "; "
                 ++ taskId ++ "++ ) {"
      ] ++
      map ("  " ++) (showP 0 (pf (var taskId))) ++
      [ "}"
      ]
    | Par n pf <- map parify (thread p)
    ]
-}

--------------------------------------------------------------------------------

ex1 :: Program
ex1 = Par (num 10) $ \i ->
      Assign (Ar c i) i :||
      (Par (num 100) $ \j ->
        Assign (Ar a j) (i .+ j) :|| Assign (Ar b i) i)
 where
  a = "a"
  b = "b"
  c = "c"

ex2 :: Program
ex2 = (Par (num 10) $ \i ->
        Assign (Ar a i) (i .+ num 1)) :>>
      (Par (num 11) $ \j ->
        Assign (Ar a (num 10 .+ j)) (j .+ num 1))
 where
  a = "a"
  b = "b"

--------------------------------------------------------------------------------


