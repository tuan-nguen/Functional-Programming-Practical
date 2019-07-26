> data Tree a = Nil | Node (Tree a) a (Tree a)
>  deriving (Show, Eq)

> isBalanced :: Tree a -> Bool
> isBalanced = and . treeToBalanceSize

> treeToBalanceSize :: Tree a -> BalanceSize
> treeToBalanceSize Nil      = []
> treeToBalanceSize (Node l n r) = True : mergeBalanceSizes (treeToBalanceSize l) (treeToBalanceSize r)

> mergeBalanceSizes :: BalanceSize -> BalanceSize -> BalanceSize
> mergeBalanceSizes []       []       = []
> mergeBalanceSizes [x]      []       = [x]
> mergeBalanceSizes []       [y]      = [y]
> mergeBalanceSizes (x : xs) (y : ys) = (x && y) : mergeBalanceSizes xs ys
> mergeBalanceSizes _        _        = [False]

> type BalanceSize = [Bool]

> list2tree [] = Nil
> list2tree [x] = Node Nil x Nil 
> list2tree list = Node  (list2tree ltx) x (list2tree gtx)
>                  where 
>                  m = length list `div` 2
>                  x = list !! m
>                  ltx = take m list
>                  gtx = drop (m+1) list

> member :: (Ord a, Eq a) => a -> Tree a -> Bool
> member x Nil = False
> member x (Node xt y yt)
>  | (x < y)  = member x xt
>  | (x == y) = True
>  | (x > y)  = member x yt

> contains :: (Ord a) => (Tree a) -> a -> Bool
> contains Nil _ = False
> contains (Node t1 v t2) x 
>	| x == v = True
>	| x  < v = contains t1 x 
>	| x  > v = contains t2 x