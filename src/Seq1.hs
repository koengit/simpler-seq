
import Prelude hiding ( null, head, tail, last, init, (++) )

--------------------------------------------------------------------------------

-- FIRST TRY, BASIC IDEA

data Seq a
  = Nil
  | Unit a
  | More a (Seq a) a

--------------------------------------------------------------------------------

null :: Seq a -> Bool
null Nil = True
null _   = False

head :: Seq a -> a
head (Unit x)     = x
head (More x _ _) = x

--------------------------------------------------------------------------------

cons :: a -> Seq a -> Seq a
cons x Nil          = Unit x
cons x (Unit y)     = More x Nil y
cons x (More y s z) = More x (cons y s) z      -- O(n) :-(

tail :: Seq a -> Seq a
tail (Unit x)       = Nil
tail (More _ Nil z) = Unit z
tail (More _ s   z) = More (head s) (tail s) z -- O(n) :-(

--------------------------------------------------------------------------------

-- (++) :: Seq a -> Seq a -> Seq a








