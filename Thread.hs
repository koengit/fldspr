module Thread where

import Expr

--------------------------------------------------------------------------------

data Program
  = Alloc (E Int) ((E Int -> Loc) -> ForLoop)

-- for now, all blocks of memory are converted into one gigantic block.
-- TODO: do something more sophisticated here

(=:) :: Loc -> E Int -> Program
x =: e = Alloc (num 0) (\_ -> assignFr x e)

iff :: E Bool -> Program -> Program
iff c (Alloc a p) = Alloc a (ifFr c . p)

(.>>) :: Program -> Program -> Program
Alloc a1 p1 .>> Alloc a2 p2 =
  Alloc (a1 .^ a2) $ \loc ->
    p1 loc :>> p2 loc

(.||) :: Program -> Program -> Program
Alloc a1 p1 .|| Alloc a2 p2 =
  Alloc (a1 .+ a2) $ \loc ->
    p1 loc `parlFr` p2 (\i -> loc (i .+ a1))

alloc :: E Int -> ((E Int -> Loc) -> Program) -> Program
alloc m f =
  Alloc (m .+ a) $ \loc ->
    let Alloc _ p = f (\i -> loc (i .+ a)) in p loc
 where
  Alloc a _ = f (error "?")

for :: E Int -> (E Int -> Program) -> Program
for n f =
  Alloc a $ \loc ->
    forFr n $ \i -> 
      let Alloc _ p = f i in
        p loc
 where
  Alloc a _ = f (error "?")

par :: E Int -> (E Int -> Program) -> Program
par n f =
  Alloc (a .* n) $ \loc ->
    parFr n $ \i -> 
      let Alloc _ p = f i in
        p (\x -> loc (x .+ (i .* a)))
 where
  Alloc a _ = f (error "?")

--------------------------------------------------------------------------------

data ForLoop
  = FOR (E Int) (E Int -> ForLoop)
  | ForLoop :>> ForLoop
  | Par ParLoop

assignFr :: Loc -> E Int -> ForLoop
assignFr x e = Par (assignPr x e)

ifFr :: E Bool -> ForLoop -> ForLoop
ifFr c (FOR n f) = FOR n (\i -> ifFr c (f i))
ifFr c (p :>> q) = ifFr c p :>> ifFr c q
ifFr c (Par p)   = Par (ifPr c p)

forFr :: (E Int) -> (E Int -> ForLoop) -> ForLoop
forFr n f =
  case f (error "?") of
    Par (ParLoop m@(E (Num 1)) _) ->
      Par (ParLoop m (\t -> [For (num 0, n, num 1) (\i -> let Par (ParLoop _ p) = f i in p t)]))
    
    _ ->
      FOR n f

parlFr :: ForLoop -> ForLoop -> ForLoop
Par p     `parlFr` Par q            = Par (p `parlPr` q)
(p :>> q) `parlFr` r                = p :>> (q `parlFr` r)
p         `parlFr` (q :>> r)        = (p `parlFr` q) :>> r
FOR n p   `parlFr` FOR m q | n == m = FOR n (\i -> p i `parlFr` q i)
p         `parlFr` q                = p :>> q -- catch all default case

-- TODO: add more/less arbitrary cases!!

parFr :: E Int -> (E Int -> ForLoop) -> ForLoop
parFr n f =
  case f (error "?") of
    Par _ ->
      Par (parPr n (\i -> let Par p = f i in p))

    _ :>> _ ->
      parFr n (\i -> let p :>> _ = f i in p) :>>
      parFr n (\i -> let _ :>> q = f i in q)

    FOR m _ -> 
      FOR m (\j -> parFr n (\i -> let FOR _ p = f i in p j))

-- TODO: check if these are actually correct

--------------------------------------------------------------------------------

data ParLoop
  = ParLoop (E Int) (E Int -> [Command])

assignPr :: Loc -> E Int -> ParLoop
assignPr x e = ParLoop (num 1) (\_ -> [x := e])

ifPr :: E Bool -> ParLoop -> ParLoop
ifPr c (ParLoop n f) = ParLoop n (\i -> [If c (f i)])

parlPr :: ParLoop -> ParLoop -> ParLoop
ParLoop n1 f1 `parlPr` ParLoop n2 f2 | n1 == n2 =
  ParLoop n1 (\i -> f1 i ++ f2 i)

ParLoop n1 f1 `parlPr` ParLoop n2 f2 =
  ParLoop (n1 .^ n2) (\i -> [If (i .< n1) (f1 i), If (i .< n2) (f2 i)])

parPr :: E Int -> (E Int -> ParLoop) -> ParLoop
parPr n f =
  ParLoop (n .* m) $ \k ->
    let ParLoop _ p = f (k ./ m) in
      p (k .% m)
 where
  ParLoop m _ = f (error "?")

--------------------------------------------------------------------------------

data Command
  = Loc := E Int
  | If (E Bool) [Command]
  | For (E Int, E Int, E Int) (E Int -> [Command])

--------------------------------------------------------------------------------

showpThread :: String -> Program -> [String]
showpThread s prog =
  -- code for one thread
  [ "void *" ++ s ++ "_thread( void *arg )"
  , "{"
  , "  int " ++ tid ++ " = ((arg_struct*)arg)->tid;"
  , "  int " ++ numThreads ++ " = ((arg_struct*)arg)->numThreads;" 
  , "  int *" ++ mem ++ " = ((arg_struct*)arg)->mem;"
  , ""
  ] ++
  [ "  " ++ l | l <- showForLoop 1 p ] ++
  [ "}"
  , ""
  ] ++

  -- code for starting threads
  [ "void " ++ s ++ "_start( int " ++ numThreads ++ " )"
  , "{"
  , "  pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t)*" ++ numThreads ++ ");"
  , "  arg_struct *args = (arg_struct*)malloc(sizeof(arg_struct)*" ++ numThreads ++ ");"
  , "  int *" ++ mem ++ " = (int*)malloc(sizeof(int)*( " ++ show m ++ " ));"
  , "  int t, err = 0;"
  , ""
  , "  /* initialization */"
  , "  sync_init(" ++ numThreads ++ ");"
  , ""
  , "  /* starting threads */"
  , "  for ( t = 0; !err && t < " ++ numThreads ++ "; t++ ) {"
  , "    args[t].tid = t;"
  , "    args[t].numThreads = " ++ numThreads ++ ";"
  , "    args[t].mem = " ++ mem ++ ";"
  , "    err = pthread_create(&threads[t], NULL, " ++ s ++ "_thread, (void*)(&args[t]));"
  , "  }"
  , ""
  , "  /* waiting for threads */"
  , "  for ( t = 0; !err && t < " ++ numThreads ++ "; t++ )"
  , "    err = pthread_join(threads[t], NULL);"
  , ""
  , "  /* cleaning up */"
  , "  free(threads);"
  , "  free(args);"
  , "  free(mem);"
  , ""
  , "  if ( err ) {"
  , "    printf(\"FATAL: %d\\n\", err);"
  , "    exit(-1);"
  , "  }"
  , "}"
  ]
 where
  Alloc m f = blocks (E (Lc (Vr numThreads))) prog
  p         = f (\i -> Ar mem i)

blocks :: E Int -> Program -> Program
blocks numThreads (Alloc n p) = Alloc n (\loc -> blocksFr (p loc))
 where
  blocksFr (FOR n p) = FOR n (blocksFr . p)
  blocksFr (p :>> q) = blocksFr p :>> blocksFr q
  blocksFr (Par p)   = Par (blocksPr p)
  
  blocksPr (ParLoop n p) =
    ParLoop numThreads $ \t ->
      let base = ((n .+ numThreads) .- num 1) ./ numThreads in
        [For (t .* base, (t .* base .+ base) .\/ n, num 1) $ \i ->
          p i]

--

showProgram :: Program -> [String]
showProgram (Alloc m p) =
  [ "int *" ++ mem ++ " = (int*)malloc(sizeof(int)*( " ++ show m ++ " ));" ] ++
  showForLoop 1 (p (\i -> Ar mem i)) ++
  [ "free(" ++ mem ++ ");" ]

--

showForLoop :: Int -> ForLoop -> [String]
showForLoop t (FOR n f) =
  [ "for ( int " ++ i ++ " = 0; " ++ i ++ " < " ++ show n ++ "; " ++ i ++ "++ ) {" ] ++
  [ "  " ++ l | l <- showForLoop (t+1) (f (E (Lc (Vr i)))) ] ++
  [ "}" ]
 where
  i = "i" ++ show t

showForLoop t (p :>> q) =
  showForLoop t p ++
  showForLoop t q

showForLoop t (Par p) =
  showParLoop p

--

showParLoop :: ParLoop -> [String]
showParLoop (ParLoop n f) =
  [ "if /* PAR */ ( " ++ show (vtid .< n) ++ " ) {"
  ] ++
  [ "  " ++ l | l <- showCommands 1 (f (vtid `atMost` n)) ] ++
  [ "}"
  , "sync();"
  ]
 where
  vtid = E (Lc (Vr tid)) `atMost` E (Lc (Vr numThreads))

--

showCommands :: Int -> [Command] -> [String]
showCommands t cs = concatMap (showCommand t) cs

showCommand :: Int -> Command -> [String]
showCommand t (x := e) =
  [ show x ++ " = " ++ show e ++ ";" ]

showCommand t (If c p) =
  [ "if ( " ++ show c ++ " ) {" ] ++
  [ "  " ++ l | l <- showCommands t p ] ++
  [ "}" ]

showCommand t (For (a0,an,inc) f) =
  [ "for ( int " ++ j ++ " = " ++ show a0 ++ "; " ++ j ++ " < " ++ show an ++ "; " ++ j ++ " += " ++ show inc ++ " ) {" ] ++
  [ "  " ++ l | l <- showCommands (t+1) (f (E (Lc (Vr j)))) ] ++
  [ "}" ]
 where
  j = "j" ++ show t

--

mem, tid, numThreads :: Name
mem        = "mem"
tid        = "tid"
numThreads = "numThreads"

--------------------------------------------------------------------------------

zipWith' :: (a -> c) -> (a -> b -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipWith' f g h []     []     = []
zipWith' f g h xs     []     = map f xs
zipWith' f g h []     ys     = map h ys
zipWith' f g h (x:xs) (y:ys) = g x y : zipWith' f g h xs ys

