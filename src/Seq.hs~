module Seq where

import Prelude hiding ( null, head, tail, last, init, (++) )
import qualified Prelude as P

--------------------------------------------------------------------------------

-- FIFTH (AND LAST) TRY, FIXING AMORTIZED COMPLEXITY

data Seq a
  = Nil
  | Unit a
  | More (Some a) (Seq (Tuple a)) (Some a)
 deriving ( Show )

data Some a
  = One a
  | Two a a
  | Three a a a
 deriving ( Show )

fromSome :: Some a -> [a]
fromSome (One a)       = [a]
fromSome (Two a b)     = [a,b]
fromSome (Three a b c) = [a,b,c]

data Tuple a
  = Pair a a
  | Triple a a a
 deriving ( Show )

toTuples :: [a] -> [Tuple a]
toTuples []         = []
toTuples [a,b]      = [Pair a b]
toTuples [a,b,c,d]  = [Pair a b, Pair c d]
toTuples (a:b:c:as) = Triple a b c : toTuples as

instance Functor Some where
  fmap f (One x)       = One (f x)
  fmap f (Two x y)     = Two (f x) (f y)
  fmap f (Three x y z) = Three (f x) (f y) (f z)

instance Functor Tuple where
  fmap f (Pair x y)     = Pair (f x) (f y)
  fmap f (Triple x y z) = Triple (f x) (f y) (f z)

instance Functor Seq where
  fmap f Nil          = Nil
  fmap f (Unit x)     = Unit (f x)
  fmap f (More p s q) = More (fmap f p) (fmap (fmap f) s) (fmap f q)

--------------------------------------------------------------------------------

pot :: Seq a -> Int
pot Nil          = 0
pot (Unit _)     = 0
pot (More p s q) = pot' p + pot s + pot' q
 where
  pot' (One _)       = 1
  pot' (Two _ _)     = 0
  pot' (Three _ _ _) = 1

--------------------------------------------------------------------------------

null :: Seq a -> Bool
null Nil = True
null _   = False

head :: Seq a -> a
head (Unit x)                 = x
head (More (One x)       _ _) = x
head (More (Two x _)     _ _) = x
head (More (Three x _ _) _ _) = x

--------------------------------------------------------------------------------

cons :: a -> Seq a -> Seq a
cons x Nil                      = Unit x
cons x (Unit y)                 = More (One x) Nil (One y)
cons x (More (One y)       s z) = More (Two x y) s z
cons x (More (Two y u)     s z) = More (Three x y u) s z
cons x (More (Three y u v) s z) = More (Two x y) (cons (Pair u v) s) z
                                  --More (One x) (cons (Triple y u v) s) z

tcons :: a -> Seq a -> Int
tcons x Nil                      = 1
tcons x (Unit y)                 = 1
tcons x (More (One y)       s z) = 1
tcons x (More (Two y u)     s z) = 1
tcons x (More (Three y u v) s z) = 1 + tcons (Pair u v) s

--------------------------------------------------------------------------------

(++) :: Seq a -> Seq a -> Seq a
Nil        ++ s          = s
s          ++ Nil        = s
Unit x     ++ s          = cons x s
s          ++ Unit x     = snoc s x
More p s v ++ More w t q = More p (glue s (toTuples (fromSome v P.++ fromSome w)) t) q

glue :: Seq a -> [a]{- 1..3 -} -> Seq a -> Seq a
glue Nil          xs s            = addL xs s
glue s            xs Nil          = addR s xs
glue (Unit x)     xs s            = cons x (addL xs s)
glue s            xs (Unit x)     = snoc (addR s xs) x
glue (More p s v) xs (More w t q) =
  More p
       (glue s (toTuples (fromSome v P.++ xs P.++ fromSome w)) t)
       q

addL :: [a] -> Seq a -> Seq a
addL []     s = s
addL (x:xs) s = cons x (addL xs s)

addR :: Seq a -> [a] -> Seq a
addR s []     = s
addR s (x:xs) = addR (snoc s x) xs

--------------------------------------------------------------------------------

tail :: Seq a -> Seq a
tail (Unit _)                         = Nil
tail (More (Two _ y) s z)             = More (One y) s z
tail (More (Three _ y u) s z)         = More (Two y u) s z
tail (More (One _) Nil (One z))       = Unit z
tail (More (One _) Nil (Two y z))     = More (One y) Nil (One z)
tail (More (One _) Nil (Three x y z)) = More (One x) Nil (Two y z)
tail (More (One _) s   z)             =
  case head s of
    Pair x y     -> More (Two x y) (tail s) z
    Triple x _ _ -> More (One x) (chop s) z

ttail :: Seq a -> Int
ttail (Unit _)                         = 1
ttail (More (Two _ y) s z)             = 1
ttail (More (Three _ y u) s z)         = 1
ttail (More (One _) Nil (One z))       = 1
ttail (More (One _) Nil (Two y z))     = 1
ttail (More (One _) Nil (Three x y z)) = 1
ttail (More (One _) s   z)             =
  case head s of
    Pair x y     -> 1 + ttail s
    Triple x _ _ -> 1

chop :: Seq (Tuple a) -> Seq (Tuple a)
chop (Unit (Triple _ y z))                 = Unit (Pair y z)
chop (More (One   (Triple _ y z))     s w) = More (One (Pair y z)) s w
chop (More (Two   (Triple _ y z) v)   s w) = More (Two (Pair y z) v) s w
chop (More (Three (Triple _ y z) v u) s w) = More (Three (Pair y z) v u) s w

--------------------------------------------------------------------------------

snoc :: Seq a -> a -> Seq a
snoc Nil                      x = Unit x
snoc (Unit y)                 x = More (One y) Nil (One x)
snoc (More z s (One y))       x = More z s (Two y x)
snoc (More z s (Two y u))     x = More z s (Three y u x)
snoc (More z s (Three y u v)) x = More z (snoc s (Pair y u)) (Two v x)

--------------------------------------------------------------------------------

