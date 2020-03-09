{-# LANGUAGE GADTs #-}
module Seq where

import Prelude hiding ( null, head, tail, last, init, (++) )
import qualified Prelude as P

--------------------------------------------------------------------------------

-- SEVENTH TRY, FIXING (++)

data Seq a
  = Nil
  | Unit a
  | More (Some a) (Seq (Tuple a)) (Some a)

type Some  a = Bunch N123 a
type Tuple a = Bunch N23  a

data N123
data N23

data Bunch t a where
  One   :: a           -> Bunch N123 a
  Two   :: a -> a      -> Bunch t    a
  Three :: a -> a -> a -> Bunch t    a

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
cons x (More (Three y u v) s z) = More (Two x y) (cons (Two u v) s) z

--------------------------------------------------------------------------------

(++) :: Seq a -> Seq a -> Seq a
Nil        ++ s          = s
s          ++ Nil        = s
Unit x     ++ s          = cons x s
s          ++ Unit x     = snoc s x
More p s v ++ More w t q = More p (glue s (comb2 v w) t) q

glue :: Seq a -> Some a -> Seq a -> Seq a
glue Nil          xs s            = addL xs s
glue s            xs Nil          = addR s xs
glue (Unit x)     xs s            = cons x (addL xs s)
glue s            xs (Unit x)     = snoc (addR s xs) x
glue (More p s v) xs (More w t q) =
  More p
       (glue s (switch (Three v xs w)) t)
       q

addL :: Some a -> Seq a -> Seq a
addL = undefined

addR :: Seq a -> Some a -> Seq a
addR = undefined

comb2 :: Some a -> Some a -> Some (Tuple a)
comb2 

switch :: Bunch N23 (Bunch s a) -> Bunch N123 (Bunch N23 a)
switch (Two (One x) (One y))       = One (Two x y)
switch (Two (One x) (Two y z))     = One (Three x y z)
switch (Two (One x) (Three y z u)) = Two (Two x y) (Two z u)

switch (Two (One x)       (One y)) = One (Two x y)
switch (Two (Two x y)     (One z)) = One (Three x y z)
switch (Two (Three x y z) (One u)) = Two (Two x y) (Two z u)

switch (Two s t)                   = Two s t

switch _ = undefined

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
    Two x y     -> More (Two x y) (tail s) z
    Three x _ _ -> More (One x) (chop s) z

chop :: Seq (Tuple a) -> Seq (Tuple a)
chop (Unit (Three _ y z))                 = Unit (Two y z)
chop (More (One   (Three _ y z))     s w) = More (One (Two y z)) s w
chop (More (Two   (Three _ y z) v)   s w) = More (Two (Two y z) v) s w
chop (More (Three (Three _ y z) v u) s w) = More (Three (Two y z) v u) s w

--------------------------------------------------------------------------------

snoc :: Seq a -> a -> Seq a
snoc Nil                      x = Unit x
snoc (Unit y)                 x = More (One y) Nil (One x)
snoc (More z s (One y))       x = More z s (Two y x)
snoc (More z s (Two y u))     x = More z s (Three y u x)
snoc (More z s (Three y u v)) x = More z (snoc s (Two y u)) (Two v x)

--------------------------------------------------------------------------------

