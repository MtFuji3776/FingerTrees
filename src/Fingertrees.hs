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

-- type Digit a = [a]
-- Exercise1より、Digitをリストから改良せよとのこと

data Digit a = One a -- 論文ではZeroが含まれていないが、Digitとしての意味を考えるならばZeroはあって然るべきである
             | Two a a
             | Three a a a
             | Four a a a a
             deriving(Show)
-- おそらく改良後のDigitはReduceのインスタンスにする必要がある
    -- 最初の定義ではDigitがリストであり、すでにReduceのインスタンスにしていたのだった
instance Reduce Digit where
    reducer f (One x) z        = f x z
    reducer f (Two x y) z      = x `f` (y `f` z)
    reducer f (Three x y w) z  = x `f` (y `f` (w `f` z))
    reducer f (Four x y w s) z = x `f` (y `f` (w `f` (s `f` z))) 
    reducel g z (One x)        = g z x
    reducel g z (Two x y)      = (z `g` x) `g` y
    reducel g z (Three x y w)  = ((z `g` x) `g` y) `g` w
    reducel g z (Four x y w s) = (((z `g` x) `g` y) `g` w) `g` s


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
a <| (Single b) = Deep (One a) Empty (One b)
a <| (Deep (Four b c d e) m sf) = Deep (Two a b) (Node3 c d e <| m) sf -- polymorphic recursion.いわゆる繰り上がり演算。
a <| (Deep pr m sf) = case pr of One b       -> Deep (Two a b) m sf
                                 Two b c     -> Deep (Three a b c) m sf
                                 Three b c d -> Deep (Four a b c d) m sf


-- 右Cons
infixl 5 |>
(|>) :: FingerTree a -> a -> FingerTree a
Empty |> a = Single a
(Single b) |> a = Deep (One b) Empty (One a)
Deep pr m (Four e d c b) |> a = Deep pr (m |> Node3 e d c) (Two b a) -- 定員オーバーすると3要素を深層に繰り上げ。新しく入った要素は上層に残る。
Deep pr m sf |> a = 
    case sf of One b       -> Deep pr m (Two b a) -- 右側に格納
               Two c b     -> Deep pr m (Three c b a)
               Three d c b -> Deep pr m (Four d c b a)

-- Reduceインスタンスを一気に格納
(<|^) :: (Reduce f) => f a -> FingerTree a -> FingerTree a
(<|^) = reducer (<|)

(|>^) :: (Reduce f) => FingerTree a -> f a -> FingerTree a
(|>^) = reducel (|>)

-- ReduceインスタンスからFingerTree生成
toTree :: (Reduce f) => f a -> FingerTree a
toTree s = s <|^ Empty

-- 木の左末端から順に要素を並べたリストデータ
data ViewL s a = NilL | ConsL a (s a)

viewL :: FingerTree a -> ViewL FingerTree a
viewL          Empty = NilL
viewL (Single x)     = ConsL x Empty
viewL (Deep pr m sf) = 
    case pr of One x -> ConsL x (deepL (One x) m sf)
               Two x y -> ConsL x (Deep (One y) m sf)
               Three x y z -> ConsL x (Deep (Two y z) m sf)
               Four x y z w -> ConsL x (Deep (Three y z w) m sf)

-- ConsL (head pr) (deepL (tail pr) m sf)

-- 左側のDigitでパターン分けする補助コンストラクタ
deepL :: Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL d m sf =  
    case viewL m of
        NilL -> toTree sf
        ConsL a m' -> 
            case a of Node2 x y   -> Deep (Two x y) m' sf
                      Node3 x y z -> Deep (Three x y z) m' sf

isEmpty :: FingerTree a -> Bool
isEmpty x = case viewL x of NilL -> True;
                            ConsL _ _ -> False
headL :: FingerTree a -> a
headL x = case viewL x of ConsL a _ -> a -- partial definition

tailL :: FingerTree a -> FingerTree a
tailL x = case viewL x of ConsL _ x' -> x' -- partial definition
-- 遅延評価のおかげで、viewLのtailの部分は使う時まで計算されない

