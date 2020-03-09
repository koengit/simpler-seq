module Seq5 where

import Prelude hiding ( null, head, tail, last, init, (++) )

--------------------------------------------------------------------------------

-- FIFTH TRY, FIXING AMORTIZED COMPLEXITY

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

data Tuple a
  = Pair a a
  | Triple a a a
 deriving ( Show )

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
                               -- More (One x) (cons (Triple y u v) s) z

--------------------------------------------------------------------------------

(++) :: Seq a -> Seq a -> Seq a
Nil        ++ s          = s
s          ++ Nil        = s
Unit x     ++ s          = cons x s
s          ++ Unit x     = snoc s x
More p s v ++ More w t q =
  More p (s ++ case (v,w) of
                 (One x,       One z)       -> cons (Pair x z) t
                 (Two x y,     One z)       -> cons (Triple x y z) t
                 (Three x y u, One z)       -> cons (Pair x y) (cons (Pair u z) t)
                 (One x,       Two w z)     -> cons (Triple x w z) t
                 (Two x y,     Two w z)     -> cons (Pair x y) (cons (Pair w z) t)
                 (Three x y u, Two w z)     -> cons (Pair x y) (cons (Triple u w z) t)
                 (One x,       Three v w z) -> cons (Pair x v) (cons (Pair w z) t)
                 (Two x y,     Three v w z) -> cons (Pair x y) (cons (Triple v w z) t)
                 (Three x y u, Three v w z) -> cons (Triple x y u) (cons (Triple v w z) t)) q

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

