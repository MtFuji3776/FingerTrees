module Fingertrees where

class Reduce f where
    reducer :: (a -> b -> b) -> (f a -> b -> b)
    reducel :: (b -> a -> b) -> (b -> f a -> b)

instance Reduce [] where
    reducer f x z = foldr f z x
    reducel f x z = foldl f x z

toList :: (Reduce f) => f a -> [a]
toList s = reducer (:) s []

data Tree a = Zero a | Succ (Tree (Node a))
data Node a = Node2 a a | Node3 a a a

type Digit a = [a]

data FingerTree a = Empty | Single a | Deep (Digit a) (FingerTree (Node a)) (Digit a)

instance Reduce Node where
    reducer f (Node2 a b) z   = a `f` (b `f` z)
    reducer f (Node3 a b c) z = a `f` (b `f` (c `f` z))
    reducel g z (Node2 b a) = (z `g` b) `g` a
    reducel g z (Node3 c b a) = ((z `g` c) `g` b) `g` a

instance Reduce FingerTree where
    reducer r Empty          z = z
    reducer r (Single x)     z = x `r` z
    reducer r (Deep pr m sf) z = pr `r'` (m `r''` (sf `r'` z))
        where r'  = reducer r
              r'' = reducer (reducer r)
    reducel l z Empty = z
    reducel l z (Single x) = z `l` x
    reducel l z (Deep pr m sf) = ((z `l'` pr) `l''` m) `l'` sf
        where l'  = reducel l
              l'' = reducel (reducel l)

infixr 5 <|
(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| (Single b) = Deep [a] Empty [b]
a <| (Deep [b,c,d,e] m sf) = Deep [a,b] (Node3 c d e <| m) sf -- polymorphic recursion
a <| (Deep pr m sf) = Deep ([a] ++ pr) m sf


