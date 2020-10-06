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
data Node a = Node2 a a | Node3 a a a deriving(Show)

type Digit a = [a]

data FingerTree a = Empty | Single a | Deep (Digit a) (FingerTree (Node a)) (Digit a) 
    deriving(Show)
-- 節点がリストを二つ抱え込みつつさらにRoseTree構造を持っているということか？
    -- ちょっと違うか。Deepの部分木のFingerTreeはNode aを成分に持つ
    -- 仮にFingerTree (Node a)がDeepコンストラクタで構成されていた場合、その部分木はFingerTree (Node (Node a))
    -- なるほど確かにNodeを多重に再帰させた構造を取っている
    -- Nodeは2分木または3分木構造だったので、Deepが直列した回数だけNodeの入れ子が深まる
    -- 型構造によって、木構造の階層の深さは確かに均一にならざるを得ない。巧妙。

-- Nodeの畳み込み
instance Reduce Node where
    reducer f (Node2 a b) z   = a `f` (b `f` z)
    reducer f (Node3 a b c) z = a `f` (b `f` (c `f` z))
    reducel g z (Node2 b a) = (z `g` b) `g` a
    reducel g z (Node3 c b a) = ((z `g` c) `g` b) `g` a

-- 畳み込み
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


-- 左Cons
infixr 5 <|
(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| (Single b) = Deep [a] Empty [b]
a <| (Deep [b,c,d,e] m sf) = Deep [a,b] (Node3 c d e <| m) sf -- polymorphic recursion.いわゆる繰り上がり演算。
a <| (Deep pr m sf) = Deep ([a] ++ pr) m sf

-- 右Cons
infixl 5 |>
(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
(Single b) |> a = Deep [b] Empty [a]
Deep pr m [e,d,c,b] |> a = Deep pr (m |> Node3 e d c) [b,a] -- 定員オーバーすると3要素を深層に繰り上げ。新しく入った要素は上層に残る。
Deep pr m sf |> a = Deep pr m (sf ++ [a]) -- 右側に格納

-- Reduceインスタンスを一気に格納
(<|^) :: (Reduce f) => f a -> FingerTree a -> FingerTree a
(<|^) = reducer (<|)

(|>^) :: (Reduce f) => FingerTree a -> f a -> FingerTree a
(|>^) = reducel (|>)

-- ReduceインスタンスからFingerTree生成
toTree :: (Reduce f) => f a -> FingerTree a
toTree s = s <|^ Empty

-- 木の左側要素を並べたリストデータ
data ViewL s a = NilL | ConsL a (s a)

viewL :: FingerTree a -> ViewL FingerTree a
viewL          Empty = NilL
viewL (Single x)     = ConsL x Empty
viewL (Deep pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

-- 左側のDigitでパターン分けする補助コンストラクタ
deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL [] m sf = case viewL m of
                NilL -> toTree sf;
                ConsL a m' -> Deep (toList a) m' sf
deepL pr m sf = Deep pr m sf

isEmpty :: FingerTree a -> Bool
isEmpty x = case viewL x of NilL -> True;
                            ConsL _ _ -> False
headL :: FingerTree a -> a
headL x = case viewL x of ConsL a _ -> a -- partial definition

tailL :: FingerTree a -> FingerTree a
tailL x = case viewL x of ConsL _ x' -> x' -- partial definition
-- 遅延評価のおかげで、viewLのtailの部分は使う時まで計算されない



