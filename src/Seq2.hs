
import Prelude hiding ( null, head, tail, last, init, (++) )

--------------------------------------------------------------------------------

-- SECOND TRY, THE REALLY COOL IDEA!

data Seq a
  = Nil
  | Unit a
  | More a (Seq (a,a)) a

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
cons x (More y s z) = error "??"

--------------------------------------------------------------------------------

-- tail :: Seq a -> Seq a
-- (++) :: Seq a -> Seq a -> Seq a

--------------------------------------------------------------------------------









