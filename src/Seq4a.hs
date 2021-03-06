
import Prelude hiding ( null, head, tail, last, init, (++) )

--------------------------------------------------------------------------------

-- FOURTH TRY, FIXING THINGS FOR (++)

data Seq a
  = Nil
  | Unit a
  | More (Some a) (Seq (Tuple a)) (Some a)

data Some a
  = One a
  | Two a a

data Tuple a
  = Pair a a
  | Triple a a a

--------------------------------------------------------------------------------

null :: Seq a -> Bool
null Nil = True
null _   = False

head :: Seq a -> a
head (Unit x)             = x
head (More (One x)   _ _) = x
head (More (Two x _) _ _) = x

--------------------------------------------------------------------------------

cons :: a -> Seq a -> Seq a
cons x Nil                  = Unit x
cons x (Unit y)             = More (One x) Nil (One y)
cons x (More (One y)   s z) = More (Two x y) s z
cons x (More (Two y u) s z) = More (One x) (cons (Pair y u) s) z   -- O(log n)

--------------------------------------------------------------------------------

(++) :: Seq a -> Seq a -> Seq a
Nil        ++ s          = s
s          ++ Nil        = s
Unit x     ++ s          = cons x s
s          ++ Unit x     = snoc s x
More p s v ++ More w t q =
  More p (s ++ case (v,w) of
                 (One x,   One z)   -> cons (Pair x z) t
                 (Two x y, One z)   -> cons (Triple x y z) t
                 (One x,   Two y z) -> cons (Triple x y z) t
                 (Two x y, Two u z) -> cons (Pair x y) (cons (Pair u z) t)) q
  
--------------------------------------------------------------------------------

tail :: Seq a -> Seq a
tail (Unit _)                     = Nil
tail (More (Two _ y) s z)         = More (One y) s z
tail (More (One _) Nil (One z))   = Unit z
tail (More (One _) Nil (Two y z)) = More (One y) Nil (One z)
tail (More (One _) s   z)         =
  case head s of
    Pair x y     -> More (Two x y) (tail s) z         -- O(log n)
    Triple x y z -> More ...

--------------------------------------------------------------------------------

snoc :: Seq a -> a -> Seq a
snoc = undefined

--------------------------------------------------------------------------------





