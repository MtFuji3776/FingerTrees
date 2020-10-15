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

-- 左側のDigitがOne _の場合の補助コンストラクタ
    -- 深層から適切にDigitを取り出して上層に配置する
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

-- 見よう見まねでViewR作成
data ViewR s a = NilR | ConsR (s a) a

-- コンストラクタ
viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m sf) = 
    case sf of One x -> ConsR (deepR pr m (One x)) x -- 本当ならdeepRの第3引数にはZeroが適するのだが
               Two x y -> ConsR (Deep pr m (One x)) y
               Three x y z -> ConsR (Deep pr m (Two x y)) z
               Four x y z w -> ConsR (Deep pr m (Three x y z)) w

-- 補助コンストラクタ
deepR :: Digit a -> (FingerTree (Node a)) -> Digit a -> FingerTree a
deepR pr m d = -- dがシカトされているのは、ここにはZeroが入っていると見做しているから
    case viewR m of
        NilR -> toTree pr
        ConsR m' a -> 
            case a of Node2 x y   -> Deep pr m' (Two x y)
                      Node3 x y z -> Deep pr m' (Three x y z)

headR :: FingerTree a -> a
headR x = case viewR x of ConsR _ a -> a

tailR :: FingerTree a -> FingerTree a
tailR x = case viewR x of ConsR m _ -> m


-- FingerTree同士のAppend

-- 結合関数の準備
    -- パターンマッチが4^3 = 64通りもあるので、そのプログラムを生成するプログラムを書く


-- Digit a値を、要素の並びを保ちつつリスト化
digitToList :: Digit a -> [a]
digitToList d =
    case d of One a        -> [a]
              Two a b      -> [a,b]
              Three a b c  -> [a,b,c]
              Four a b c d -> [a,b,c,d]

-- Digit aの三つ組を一つのリストに
    -- 全体の要素の並びが保存されてるのがミソ。
    -- Digit aの順列の情報を無くして、Nodeに切り替えやすくしている
digitsToList :: (Digit a,Digit a,Digit a) -> [a]
digitsToList (d1,d2,d3) = concatMap digitToList [d1,d2,d3]

-- 要素数2~12のリストをDigitにする部分関数
    -- Digitが1~4,Nodeが2または3個の値だけ保持することに基づいている
listToDigitNode :: [a] -> Digit (Node a)
listToDigitNode xs = 
    case xs of [a,b]                     -> One (Node2 a b)
               [a,b,c]                   -> One (Node3 a b c)
               [a,b,c,d]                 -> Two (Node2 a b) (Node2 c d)
               [a,b,c,d,e]               -> Two (Node3 a b c) (Node2 d e)
               [a,b,c,d,e,f]             -> Two (Node3 a b c) (Node3 d e f)
               [a,b,c,d,e,f,g]           -> Three (Node3 a b c) (Node2 d e) (Node2 f g)
               [a,b,c,d,e,f,g,h]         -> Three (Node3 a b c) (Node3 d e f) (Node2 g h)
               [a,b,c,d,e,f,g,h,i]       -> Three (Node3 a b c) (Node3 d e f) (Node3 g h i)
               [a,b,c,d,e,f,g,h,i,j]     -> Four (Node3 a b c) (Node3 d e f) (Node2 g h) (Node2 i j)
               [a,b,c,d,e,f,g,h,i,j,k]   -> Four (Node3 a b c) (Node3 d e f) (Node3 g h i) (Node2 j k)
               [a,b,c,d,e,f,g,h,i,j,k,l] -> Four (Node3 a b c) (Node3 d e f) (Node3 g h i) (Node3 j k l)

-- 関数を合成して、Digit aの三つ組を一つのDigit (Node a)値に変換
digitsToDigitNode :: (Digit a,Digit a,Digit a) -> Digit (Node a)
digitsToDigitNode = listToDigitNode . digitsToList

printPattern :: Show a => (Digit a,Digit a, Digit a) -> String
printPattern (d1,d2,d3) = 
    let dn = digitsToDigitNode (d1,d2,d3) 
    in show (d1,d2,d3) ++ " -> " ++ show dn ++ "\n"

-- Digit aの三つ組全体へのパターンマッチを出力
    -- 変数記号をとりあえずCharで表し、出力結果からTurtleあたりの正規表現関数でシングルクォテーションを除去すれば良い
printAllPatterns :: String
printAllPatterns = 
    let xs1 = [One "x1",Two "x2" "y2",Three "x3" "y3" "z3", Four "x4" "y4" "z4" "w4"] 
        xs2 = [One "x1'",Two "x2'" "y2'",Three "x3'" "y3'" "z3'", Four "x4'" "y4'" "z4'" "w4'"] 
        xs3 = [One "x1''",Two "x2''" "y2''",Three "x3''" "y3''" "z3''", Four "x4''" "y4''" "z4''" "w4''"] 
    in concatMap printPattern [(x,y,z) | x <- xs1, y<- xs2, z <- xs3]

-- 構成したパターンマッチを使ってconcat用関数を定義
app3 :: FingerTree a -> Digit a -> FingerTree a -> FingerTree a
app3 Empty dg xs = dg <|^ xs
app3 xs dg Empty = xs |>^ dg
app3 (Single x) dg xs = x <| (dg <|^ xs)
app3 xs dg (Single x) = (xs |>^ dg) |> x
app3 (Deep pr1 m1 sf1) dg (Deep pr2 m2 sf2) = Deep pr1 (app3 m1 (nodes (sf1, dg, pr2)) m2) sf2

-- 全パターン網羅のパターンマッチ。printAllPatternsに出力させたプログラム。
-- 二つのFingerTree xs,ysのうち、xsの右側のDigitとysの左側のDigitをまとめて一つのDigit (Node)にする関数。
-- これにより再帰的に部分木をまとめ上げる関数が定義できている
nodes :: (Digit a, Digit a, Digit a) -> Digit (Node a)
nodes dgs = 
    case dgs of (One x1,One x1',One x1'') -> One (Node3 x1 x1' x1'')
                (One x1,One x1',Two x2'' y2'') -> Two (Node2 x1 x1') (Node2 x2'' y2'')
                (One x1,One x1',Three x3'' y3'' z3'') -> Two (Node3 x1 x1' x3'') (Node2 y3'' z3'')
                (One x1,One x1',Four x4'' y4'' z4'' w4'') -> Two (Node3 x1 x1' x4'') (Node3 y4'' z4'' w4'')
                (One x1,Two x2' y2',One x1'') -> Two (Node2 x1 x2') (Node2 y2' x1'')
                (One x1,Two x2' y2',Two x2'' y2'') -> Two (Node3 x1 x2' y2') (Node2 x2'' y2'')
                (One x1,Two x2' y2',Three x3'' y3'' z3'') -> Two (Node3 x1 x2' y2') (Node3 x3'' y3'' z3'')
                (One x1,Two x2' y2',Four x4'' y4'' z4'' w4'') -> Three (Node3 x1 x2' y2') (Node2 x4'' y4'') (Node2 z4'' w4'')
                (One x1,Three x3' y3' z3',One x1'') -> Two (Node3 x1 x3' y3') (Node2 z3' x1'')
                (One x1,Three x3' y3' z3',Two x2'' y2'') -> Two (Node3 x1 x3' y3') (Node3 z3' x2'' y2'')
                (One x1,Three x3' y3' z3',Three x3'' y3'' z3'') -> Three (Node3 x1 x3' y3') (Node2 z3' x3'') (Node2 y3'' z3'')
                (One x1,Three x3' y3' z3',Four x4'' y4'' z4'' w4'') -> Three (Node3 x1 x3' y3') (Node3 z3' x4'' y4'') (Node2 z4'' w4'')
                (One x1,Four x4' y4' z4' w4',One x1'') -> Two (Node3 x1 x4' y4') (Node3 z4' w4' x1'')
                (One x1,Four x4' y4' z4' w4',Two x2'' y2'') -> Three (Node3 x1 x4' y4') (Node2 z4' w4') (Node2 x2'' y2'')
                (One x1,Four x4' y4' z4' w4',Three x3'' y3'' z3'') -> Three (Node3 x1 x4' y4') (Node3 z4' w4' x3'') (Node2 y3'' z3'')
                (One x1,Four x4' y4' z4' w4',Four x4'' y4'' z4'' w4'') -> Three (Node3 x1 x4' y4') (Node3 z4' w4' x4'') (Node3 y4'' z4'' w4'')
                (Two x2 y2,One x1',One x1'') -> Two (Node2 x2 y2) (Node2 x1' x1'')
                (Two x2 y2,One x1',Two x2'' y2'') -> Two (Node3 x2 y2 x1') (Node2 x2'' y2'')
                (Two x2 y2,One x1',Three x3'' y3'' z3'') -> Two (Node3 x2 y2 x1') (Node3 x3'' y3'' z3'')
                (Two x2 y2,One x1',Four x4'' y4'' z4'' w4'') -> Three (Node3 x2 y2 x1') (Node2 x4'' y4'') (Node2 z4'' w4'')
                (Two x2 y2,Two x2' y2',One x1'') -> Two (Node3 x2 y2 x2') (Node2 y2' x1'')
                (Two x2 y2,Two x2' y2',Two x2'' y2'') -> Two (Node3 x2 y2 x2') (Node3 y2' x2'' y2'')
                (Two x2 y2,Two x2' y2',Three x3'' y3'' z3'') -> Three (Node3 x2 y2 x2') (Node2 y2' x3'') (Node2 y3'' z3'')
                (Two x2 y2,Two x2' y2',Four x4'' y4'' z4'' w4'') -> Three (Node3 x2 y2 x2') (Node3 y2' x4'' y4'') (Node2 z4'' w4'')
                (Two x2 y2,Three x3' y3' z3',One x1'') -> Two (Node3 x2 y2 x3') (Node3 y3' z3' x1'')
                (Two x2 y2,Three x3' y3' z3',Two x2'' y2'') -> Three (Node3 x2 y2 x3') (Node2 y3' z3') (Node2 x2'' y2'')
                (Two x2 y2,Three x3' y3' z3',Three x3'' y3'' z3'') -> Three (Node3 x2 y2 x3') (Node3 y3' z3' x3'') (Node2 y3'' z3'')
                (Two x2 y2,Three x3' y3' z3',Four x4'' y4'' z4'' w4'') -> Three (Node3 x2 y2 x3') (Node3 y3' z3' x4'') (Node3 y4'' z4'' w4'')
                (Two x2 y2,Four x4' y4' z4' w4',One x1'') -> Three (Node3 x2 y2 x4') (Node2 y4' z4') (Node2 w4' x1'')
                (Two x2 y2,Four x4' y4' z4' w4',Two x2'' y2'') -> Three (Node3 x2 y2 x4') (Node3 y4' z4' w4') (Node2 x2'' y2'')
                (Two x2 y2,Four x4' y4' z4' w4',Three x3'' y3'' z3'') -> Three (Node3 x2 y2 x4') (Node3 y4' z4' w4') (Node3 x3'' y3'' z3'')
                (Two x2 y2,Four x4' y4' z4' w4',Four x4'' y4'' z4'' w4'') -> Four (Node3 x2 y2 x4') (Node3 y4' z4' w4') (Node2 x4'' y4'') (Node2 z4'' w4'')
                (Three x3 y3 z3,One x1',One x1'') -> Two (Node3 x3 y3 z3) (Node2 x1' x1'')
                (Three x3 y3 z3,One x1',Two x2'' y2'') -> Two (Node3 x3 y3 z3) (Node3 x1' x2'' y2'')
                (Three x3 y3 z3,One x1',Three x3'' y3'' z3'') -> Three (Node3 x3 y3 z3) (Node2 x1' x3'') (Node2 y3'' z3'')
                (Three x3 y3 z3,One x1',Four x4'' y4'' z4'' w4'') -> Three (Node3 x3 y3 z3) (Node3 x1' x4'' y4'') (Node2 z4'' w4'')
                (Three x3 y3 z3,Two x2' y2',One x1'') -> Two (Node3 x3 y3 z3) (Node3 x2' y2' x1'')
                (Three x3 y3 z3,Two x2' y2',Two x2'' y2'') -> Three (Node3 x3 y3 z3) (Node2 x2' y2') (Node2 x2'' y2'')
                (Three x3 y3 z3,Two x2' y2',Three x3'' y3'' z3'') -> Three (Node3 x3 y3 z3) (Node3 x2' y2' x3'') (Node2 y3'' z3'')
                (Three x3 y3 z3,Two x2' y2',Four x4'' y4'' z4'' w4'') -> Three (Node3 x3 y3 z3) (Node3 x2' y2' x4'') (Node3 y4'' z4'' w4'')
                (Three x3 y3 z3,Three x3' y3' z3',One x1'') -> Three (Node3 x3 y3 z3) (Node2 x3' y3') (Node2 z3' x1'')
                (Three x3 y3 z3,Three x3' y3' z3',Two x2'' y2'') -> Three (Node3 x3 y3 z3) (Node3 x3' y3' z3') (Node2 x2'' y2'')
                (Three x3 y3 z3,Three x3' y3' z3',Three x3'' y3'' z3'') -> Three (Node3 x3 y3 z3) (Node3 x3' y3' z3') (Node3 x3'' y3'' z3'')
                (Three x3 y3 z3,Three x3' y3' z3',Four x4'' y4'' z4'' w4'') -> Four (Node3 x3 y3 z3) (Node3 x3' y3' z3') (Node2 x4'' y4'') (Node2 z4'' w4'')
                (Three x3 y3 z3,Four x4' y4' z4' w4',One x1'') -> Three (Node3 x3 y3 z3) (Node3 x4' y4' z4') (Node2 w4' x1'')
                (Three x3 y3 z3,Four x4' y4' z4' w4',Two x2'' y2'') -> Three (Node3 x3 y3 z3) (Node3 x4' y4' z4') (Node3 w4' x2'' y2'')
                (Three x3 y3 z3,Four x4' y4' z4' w4',Three x3'' y3'' z3'') -> Four (Node3 x3 y3 z3) (Node3 x4' y4' z4') (Node2 w4' x3'') (Node2 y3'' z3'')
                (Three x3 y3 z3,Four x4' y4' z4' w4',Four x4'' y4'' z4'' w4'') -> Four (Node3 x3 y3 z3) (Node3 x4' y4' z4') (Node3 w4' x4'' y4'') (Node2 z4'' w4'')
                (Four x4 y4 z4 w4,One x1',One x1'') -> Two (Node3 x4 y4 z4) (Node3 w4 x1' x1'')
                (Four x4 y4 z4 w4,One x1',Two x2'' y2'') -> Three (Node3 x4 y4 z4) (Node2 w4 x1') (Node2 x2'' y2'')
                (Four x4 y4 z4 w4,One x1',Three x3'' y3'' z3'') -> Three (Node3 x4 y4 z4) (Node3 w4 x1' x3'') (Node2 y3'' z3'')
                (Four x4 y4 z4 w4,One x1',Four x4'' y4'' z4'' w4'') -> Three (Node3 x4 y4 z4) (Node3 w4 x1' x4'') (Node3 y4'' z4'' w4'')
                (Four x4 y4 z4 w4,Two x2' y2',One x1'') -> Three (Node3 x4 y4 z4) (Node2 w4 x2') (Node2 y2' x1'')
                (Four x4 y4 z4 w4,Two x2' y2',Two x2'' y2'') -> Three (Node3 x4 y4 z4) (Node3 w4 x2' y2') (Node2 x2'' y2'')
                (Four x4 y4 z4 w4,Two x2' y2',Three x3'' y3'' z3'') -> Three (Node3 x4 y4 z4) (Node3 w4 x2' y2') (Node3 x3'' y3'' z3'')
                (Four x4 y4 z4 w4,Two x2' y2',Four x4'' y4'' z4'' w4'') -> Four (Node3 x4 y4 z4) (Node3 w4 x2' y2') (Node2 x4'' y4'') (Node2 z4'' w4'')
                (Four x4 y4 z4 w4,Three x3' y3' z3',One x1'') -> Three (Node3 x4 y4 z4) (Node3 w4 x3' y3') (Node2 z3' x1'')
                (Four x4 y4 z4 w4,Three x3' y3' z3',Two x2'' y2'') -> Three (Node3 x4 y4 z4) (Node3 w4 x3' y3') (Node3 z3' x2'' y2'')
                (Four x4 y4 z4 w4,Three x3' y3' z3',Three x3'' y3'' z3'') -> Four (Node3 x4 y4 z4) (Node3 w4 x3' y3') (Node2 z3' x3'') (Node2 y3'' z3'')
                (Four x4 y4 z4 w4,Three x3' y3' z3',Four x4'' y4'' z4'' w4'') -> Four (Node3 x4 y4 z4) (Node3 w4 x3' y3') (Node3 z3' x4'' y4'') (Node2 z4'' w4'')
                (Four x4 y4 z4 w4,Four x4' y4' z4' w4',One x1'') -> Three (Node3 x4 y4 z4) (Node3 w4 x4' y4') (Node3 z4' w4' x1'')
                (Four x4 y4 z4 w4,Four x4' y4' z4' w4',Two x2'' y2'') -> Four (Node3 x4 y4 z4) (Node3 w4 x4' y4') (Node2 z4' w4') (Node2 x2'' y2'')
                (Four x4 y4 z4 w4,Four x4' y4' z4' w4',Three x3'' y3'' z3'') -> Four (Node3 x4 y4 z4) (Node3 w4 x4' y4') (Node3 z4' w4' x3'') (Node2 y3'' z3'')
                (Four x4 y4 z4 w4,Four x4' y4' z4' w4',Four x4'' y4'' z4'' w4'') -> Four (Node3 x4 y4 z4) (Node3 w4 x4' y4') (Node3 z4' w4' x4'') (Node3 y4'' z4'' w4'')


-- タプル版Digitに基づいてapp3を計算するための工夫関数
    -- これもDigitにZeroを含めれば解決するんだが。
-- 左端のDigitを取り出す
headDigitL :: FingerTree a -> Digit a
headDigitL (Single x)     = One x
headDigitL (Deep pr m sf) = pr
-- 右端のDigit
headDigitR :: FingerTree a -> Digit a
headDigitR (Single x)     = One x
headDigitR (Deep pr m sf) = sf

-- tailDigitの計算に必要
nodeToDigit :: Node a -> Digit a
nodeToDigit (Node2 x y) = Two x y
nodeToDigit (Node3 x y z) = Three x y z

-- 
tailDigitL :: FingerTree a -> FingerTree a
tailDigitL (Single x) = Empty
tailDigitL (Deep pr m sf) = 
    case m of Empty -> toTree sf
              Single x -> x <|^ (toTree sf)
              Deep pr' m' sf' -> Deep (nodeToDigit $ headL m) (tailL m) sf -- headDigitLとtailDigitLを合わせれば全体が復元されることを利用して再帰

tailDigitR :: FingerTree a -> FingerTree a
tailDigitR (Single x) = Empty
tailDigitR (Deep pr m sf) = 
    case m of Empty -> toTree pr
              Single x -> (toTree sf) |>^ x
              Deep pr' m' sf' -> Deep pr (tailR m) (nodeToDigit $ headR m) -- 再帰。

-- FingerTree同士のappend演算
-- これでconcatも計算できるようになる
-- 驚くべきは計算効率で、引数のサイズ（要素数）をそれぞれn1,n2とするとき、appendの実行時間はlog(min{n1,n2})
(><) :: FingerTree a -> FingerTree a -> FingerTree a
xs >< ys = app3 (tailDigitR xs) (headDigitR xs) ys