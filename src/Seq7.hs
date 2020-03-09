module Seq where

import Prelude hiding ( null, head, tail, last, init )
import qualified Prelude as P

import Test.QuickCheck
import Test.QuickCheck.Poly( A )

--------------------------------------------------------------------------------

-- SIXTH TRY, TESTING AMORTIZED COMPLEXITY

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

(++) :: Seq a -> Seq a -> Seq a
Nil        ++ s          = s
s          ++ Nil        = s
Unit x     ++ s          = cons x s
s          ++ Unit x     = snoc s x
More p s v ++ More w t q = More p (glue s (comb2 v w) t) q

glue :: Seq a -> Some a -> Seq a -> Seq a
glue Nil (One x)       s = cons x s
glue Nil (Two x y)     s = cons x (cons y s)
glue Nil (Three x y z) s = cons x (cons y (cons z s))

glue s (One x)       Nil = snoc s x
glue s (Two x y)     Nil = snoc (snoc s x) y
glue s (Three x y z) Nil = snoc (snoc (snoc s x) y) z

glue (Unit x) u s = cons x (glue Nil u s)
glue s u (Unit x) = snoc (glue s u Nil) x

glue (More p s v) u (More w t q) =
  More p
       (glue s (comb3 v u w) t)
       q

comb2 :: Some a -> Some a -> Some (Tuple a)
comb2 (One x) (One y)       = One (Pair x y)
comb2 (One x) (Two y z)     = One (Triple x y z)
comb2 (One x) (Three y z u) = Two (Pair x y) (Pair z u)

comb2 (Two x y) (One z)       = One (Triple x y z)
comb2 (Two x y) (Two z u)     = Two (Pair x y) (Pair z u)
comb2 (Two x y) (Three z u v) = Two (Pair x y) (Triple z u v)

comb2 (Three x y z) (One u)       = Two (Pair x y) (Pair z u)
comb2 (Three x y z) (Two u v)     = Two (Triple x y z) (Two u v)
comb2 (Three x y z) (Three u v w) = Two (Triple x y z) (Triple u v w)




--------------------------------------------------------------------------------

cons :: a -> Seq a -> Seq a
cons x Nil                      = Unit x
cons x (Unit y)                 = More (One x) Nil (One y)
cons x (More (One y)       s z) = More (Two x y) s z
cons x (More (Two y u)     s z) = More (Three x y u) s z
cons x (More (Three y u v) s z) = More (Two x y) (cons (Pair u v) s) z
                                  --More (One x) (cons (Triple y u v) s) z
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
