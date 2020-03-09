
import Prelude hiding ( null, head, tail, last, init, (++) )

--------------------------------------------------------------------------------

-- THIRD TRY, FIXING THE COOL IDEA FOR cons / tail

data Seq a
  = Nil
  | Unit a
  | More (Some a) (Seq (a,a)) (Some a)

data Some a
  = One a
  | Two a a

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
cons x (More (Two y u) s z) = More (One x) (cons (y,u) s) z        -- O(log n)

--------------------------------------------------------------------------------

tail :: Seq a -> Seq a
tail (Unit _)                       = Nil
tail (More (One _)   Nil (One z))   = Unit z
tail (More (One _)   Nil (Two y z)) = More (One y) Nil (One z)
tail (More (One _)   s   z)         = More (Two x y) (tail s) z where (x,y) = head s -- O(log n)
tail (More (Two _ y) s   z)         = More (One y) s z

--------------------------------------------------------------------------------

(++) :: Seq a -> Seq a -> Seq a
Nil    ++ s      = s
s      ++ Nil    = s
Unit x ++ s      = cons x s
s      ++ Unit x = snoc s x

More p s v ++ More w t q = error "??"

--------------------------------------------------------------------------------

snoc :: Seq a -> a -> Seq a
snoc = undefined

--------------------------------------------------------------------------------






