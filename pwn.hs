{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
   ( Bool(..)
   , print
   , all
   , (+)
   , (-)
   , (==)
   , fmap
   )

id a = a

compose f g x = f (g x)

{- Y combinator
y = \f -> (\x -> f (x x)) (\x -> f (x x))
y f = g g where g x = f (x x)
-- simplest form
y f = (\x -> x x) (\x -> f (x x))
-}

{- Z combinator
z = \f -> (\x -> f (\y -> x x y)) (\x -> f (\y -> x x y))
z f = (\x -> x x) (\x -> f (\y -> x x y))
z f = g g where g x = f (\y -> x x y)
z f = g g where g x = f h where h y = x x y
-}

-- Haskell has recursion baked in
y f = x where x = f x

fals a b = b
true a b = a

const = true

not p a b = p b a
-- not p = p fals true

xor p q a b = p (q b a) (q a b)

and p q = p q fals
--ERR and p q = p q p

or p q = p true q
--ERR or p q = p p q

nor p q = not (or p q)
-- nor = compose not or

nand p q = not (and p q)
-- nand = compose not and

beq p q = not (xor p q)
-- beq = compose not xor

_bool p = p True False

-- conditional

if' p a b = p a b
-- if' = id

-- pair

pair a b f = f a b

fst t = t f where f a b = a
-- fst t = t true

snd t = t f where f a b = b
-- snd t = t fals

_pair t = (fst t,snd t)

pair_ (a,b) = pair a b

-- option

none n s = n
-- none = fals

some x n s = s x

isnone w = w true (\_ -> fals)

fromsome w = w none id  -- partial

-- list

cons a t f = f a t
-- cons a t = pair a t
-- cons = pair

head = fst
tail = snd

-- size l = if' (isnone l) zero (succ (size (tail (fromsome l))))

index l n = fst (n (compose fromsome snd) (fromsome l))

--ERR map f l = if' (isnone l) none (some (pair (f (fst (fromsome l))) (map f (snd (fromsome l)))))

--ERR foldl f x l = if' (isnone l) k (foldl f (f k (head (fromsome l))) (tail (fromsome l)))

--ERR foldr f x l = if' (isnone l) k (f (head (fromsome l)) (foldr f k (tail (fromsome l))))

--ERR _list and list_ assume possibly empty lists, so (SOME (PAIR...

--ERR _list l
--ERR    | _bool (isnone l) = []
--ERR    | True = head (fromsome l) : _list (tail (fromsome l))

--ERR list_ l
--ERR    | [] <- l = none
--ERR    | True = some (pair (head l) (list_ (tail l)))

-- natural numbers

zero f x = x

succ n f x = f (n f x)

pred n f x = n (\g h -> h (g f)) (\y -> x) (\y -> y)  -- wtf

_num n = n (1 +) 0

-- arithmetic

sum n m = n succ m

mul n m f = n (m f)
-- mul n m = n (sum m) zero

sub n m = m pred n

pow n m = m n
-- pow n m = m (mul n) (succ zero)

iszero n = n (\_ -> fals) true
--iszero n = n (const fals) true

lte n m = iszero (sub n m)

gt n m = not (lte n m)
-- gt = compose not lte

--ERR eq n m = and (lte n m) (lte m n)

--ERR mod n m = if' (lte m n) (mod (sub n m) n) m

-- util

num_ n
   | 0 <- n = zero
   | True = succ (num_ (n - 1))

main = print r
   where
   list = pair (num_ 9) (some (pair (num_ 8) (some (pair (num_ 7) none))))
   r
      | True  <- _bool (not fals)
      , False <- _bool (not true)

      , False <- _bool (xor fals fals)
      , True  <- _bool (xor true fals)
      , True  <- _bool (xor fals true)
      , False <- _bool (xor true true)

      , False <- _bool (and fals fals)
      , False <- _bool (and true fals)
      , False <- _bool (and fals true)
      , True  <- _bool (and true true)

      , True  <- _bool (nor fals fals)
      , False <- _bool (nor true fals)
      , False <- _bool (nor fals true)
      , False <- _bool (nor true true)

      , True  <- _bool (nand fals fals)
      , True  <- _bool (nand true fals)
      , True  <- _bool (nand fals true)
      , False <- _bool (nand true true)

      , True  <- _bool (beq fals fals)
      , False <- _bool (beq true fals)
      , False <- _bool (beq fals true)
      , True  <- _bool (beq true true)

      , 0 <- _num zero
      , 1 <- _num (succ zero)
      , 2 <- _num (succ (succ zero))
      , 72 <- _num (num_ 72)
      , 16 <- _num (sum (num_ 9) (num_ 7))
      , 63 <- _num (mul (num_ 9) (num_ 7))
      , 6 <- _num (pred (num_ 7))
      , 2 <- _num (sub (succ zero) (num_ 0))
--ERR , 2 <- _num (sub (num_ 9) (num_ 7))
--ERR , 0 <- _num (sub (num_ 7) (num_ 9))
      , 27 <- _num (pow (num_ 3) (num_ 3))

      , True  <- _bool (iszero zero)
      , False <- _bool (iszero (succ zero))
      , False <- _bool (iszero (num_ 7))
--ERR , True  <- _bool (lte (num_ 7) (num_ 9))
--ERR , True  <- _bool (lte (num_ 9) (num_ 9))
--ERR , False <- _bool (lte (num_ 9) (num_ 7))
--ERR , False <- _bool (gt  (num_ 7) (num_ 9))
--ERR , False <- _bool (gt  (num_ 9) (num_ 9))
--ERR , True  <- _bool (gt  (num_ 9) (num_ 7))
--ERR , False <- _bool (eq  (num_ 7) (num_ 9))
--ERR , True  <- _bool (eq  (num_ 9) (num_ 9))
--ERR , False <- _bool (eq  (num_ 9) (num_ 7))
--ERR , 2 <- _num (mod (num_ 9) (num_ 7))
--ERR , 0 <- _num (mod (num_ 7) (num_ 7))
--ERR , 7 <- _num (mod (num_ 7) (num_ 9))

--ERR , [9,8,7] <- _list (list_ [9,8,7])
--ERR , [9,8,7] <- _list (map _num (some list))
--ERR , [9,8,7] <- fmap _num (_list (some list))

      , 9 <- _num (head list)
      , 8 <- _num (head (fromsome (tail list)))
--ERR , 3 <- _num (size (some list))
--ERR , 0 <- _num (size none)
--ERR , 2 <- _num (size (tail list))
--ERR , 7 <- _num (index (some list) (num_ 2))

--ERR , 18 <- _num (head (fromsome (map (\x -> sum x x) (some list))))
--ERR , 0 <- _num (foldr sum zero none)
--ERR , 24 <- _num (foldr sum zero (some list))
--ERR , 0 <- _num (foldl sum zero none)
--ERR , 24 <- _num (foldl sum zero (some list))

      = True
      | True = False



-- ------------------------------------------------------------------
-- import Prelude ( (+) , (-) , Num , Eq, print )
--
-- -- church numeral zero
-- zero :: a -> b -> b
-- zero f x = x
--
-- -- successor function
-- succ :: ((a -> b) -> (c -> a)) -> (a -> b) -> c -> b
-- succ n f x = f (n f x)
--
-- -- predecessor function (subtraction)
-- pred :: (((f -> a) -> (a -> b) -> b) -> (y -> x) -> (z -> z) -> c) -> f -> x -> c
-- pred n f x = n (\g h -> h (g f)) (\y -> x) (\y -> y)
--
-- -- subtract n from m
-- sub :: t1 -> (((((f -> a) -> (a -> b) -> b) -> (y -> x) -> (u -> u) -> c) -> f -> x -> c) -> t1 -> t2) -> t2
-- sub m n = n pred m
--
-- -- convert from church
-- _num :: b -> c
-- _num n = n (1 +) 0
--
-- -- convert to church
-- num_ :: (Num a, Eq a) => a -> (b -> b) -> b -> b
-- num_ 0 = zero
-- num_ n = succ (num_ (n - 1))
--
-- main = do
--     print (_num (sub (num_ 3) (succ zero)))  -- 2
-- --  print (_num (sub (num_ 3) (num_ 0)))     -- ERRORS
