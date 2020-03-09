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

pot :: Seq a -> Int
pot Nil          = 0
pot (Unit _)     = 0
pot (More p s q) = pot' p + pot s + pot' q
 where
  pot' (One _)       = 1
  pot' (Two _ _)     = 0
  pot' (Three _ _ _) = 1

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

prop_ConsAmortized :: A -> Seq A -> Bool
prop_ConsAmortized x s =
  tcons x s + pot (cons x s) - pot s <= 3

prop_TailAmortized :: Seq A -> Property
prop_TailAmortized s =
  not (null s) ==>
    ttail s + pot (tail s) - pot s <= 2

--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Some a) where
  arbitrary =
    oneof
    [ do x <- arbitrary
         return (One x)
    , do x <- arbitrary
         y <- arbitrary
         return (Two x y)
    , do x <- arbitrary
         y <- arbitrary
         z <- arbitrary
         return (Three x y z)
    ] 

  shrink (Two x y)     = [ One x ]
  shrink (Three x y z) = [ One x, Two x y ]
  shrink _             = []

instance Arbitrary a => Arbitrary (Tuple a) where
  arbitrary =
    oneof
    [ do x <- arbitrary
         y <- arbitrary
         return (Pair x y)
    , do x <- arbitrary
         y <- arbitrary
         z <- arbitrary
         return (Triple x y z)
    ] 

  shrink (Triple x y z) = [ Pair x y ]
  shrink _              = []

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = sized arb
   where
    arb :: Arbitrary b => Int -> Gen (Seq b)
    arb n = frequency
      [ (1, do return Nil)
      , (1, do x <- arbitrary
               return (Unit x))
      , (n, do p <- arbitrary
               s <- arb (n `div` 2)
               q <- arbitrary
               return (More p s q))
      ]

  shrink (Unit _)     = [ Nil ]
  shrink (More p s q) = [ Nil, Unit (head (More p s q)) ]
                     ++ [ More p' s q | p' <- shrink p ]
                     ++ [ More p s' q | s' <- shrink s ]
                     ++ [ More p s q' | q' <- shrink q ]
  shrink _            = []                     

--------------------------------------------------------------------------------

--(++) :: Seq a -> Seq a -> Seq a

--------------------------------------------------------------------------------

