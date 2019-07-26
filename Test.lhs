> module Test (
>   Maze, 
>   makeMaze,
>   hasWall,
>   sizeOf
> )
> where

> import Geography
> import Data.List

> data Tree a = Nil | Node (Tree a) a (Tree a) 
>  deriving (Show, Eq)

> list2tree [] = Nil
> list2tree [x] = Node Nil x Nil 
> list2tree list = Node  (list2tree ltx) x (list2tree gtx)
>                  where 
>                  m = length list `div` 2
>                  x = list !! m
>                  ltx = take m list
>                  gtx = drop (m+1) list

> contains :: (Ord a) => (Tree a) -> a -> Bool
> contains Nil _ = False
> contains (Node t1 v t2) x 
>	| x == v = True
>	| x  < v = contains t1 x 
>	| x  > v = contains t2 x

> data Maze = Maze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)
>  deriving Show

> isNorth :: Wall -> Bool
> isNorth w = snd w == N

> isSouth :: Wall -> Bool
> isSouth w = snd w == S

> isEast :: Wall -> Bool
> isEast w = snd w == E

> isWest :: Wall -> Bool
> isWest w = snd w == W

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x, y) walls = 
>     Maze (x, y) (list2tree (sort allSortedNorth))
>                 (list2tree (sort allSortedSouth))
>                 (list2tree (sort allSortedEast))
>                 (list2tree (sort allSortedWest))
>  where allSortedNorth =  map fst (filter isNorth walls) ++ [(i, y - 1) | i <- [0..x-1]] ++ map (reflect S) (map fst (filter isSouth walls) ++ [(i, 0) | i <- [0..x-1]])
>        allSortedSouth =  map fst (filter isSouth walls) ++ [(i, 0) | i <- [0..x-1]] ++ map (reflect N) (map fst (filter isNorth walls) ++ [(i, y - 1) | i <- [0..x-1]])
>        allSortedEast  =  map fst (filter isEast walls) ++ [(x - 1, j) | j <- [0..y-1]] ++ map (reflect W) (map fst (filter isWest walls) ++ [(0, j) | j <- [0..y-1]])
>        allSortedWest  =  map fst (filter isWest walls) ++ [(0, j) | j <- [0..y-1]] ++ map (reflect E) (map fst (filter isEast walls) ++ [(x - 1, j) | j <- [0..y-1]])

> reflect :: Direction -> Place -> Place
> reflect d (i, j) = move d (i, j)

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ n s e w) pos d
>  | d == N = contains n pos
>  | d == S = contains s pos
>  | d == E = contains e pos
>  | d == W = contains w pos

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size