> module MyMaze1 (
>   Maze, 
>   makeMaze,
>   hasWall,
>   sizeOf
> )
> where

> import Geography
> import Data.List

We import Data.List so we can use the "sort" function.

> data Tree a = Leaf | Node (Tree a) a (Tree a)
>  deriving (Show, Eq)

> list2Tree :: [a] -> Tree a
> list2Tree [] = Leaf
> list2Tree [x] = Node Leaf x Leaf
> list2Tree xs = Node (list2Tree xt) x (list2Tree yt)
>  where m = length xs `div` 2
>        x = xs !! m
>        xt = take m xs
>        yt = drop (m + 1) xs

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

 makeMaze :: Size -> [Wall] -> Maze
 makeMaze (x, y) walls = 
     Maze (x, y) (list2Tree allSortedNorth)
                 (list2Tree allSortedSouth)
                 (list2Tree allSortedEast)
                 (list2Tree allSortedWest)
  where allSortedNorth = sort (map fst (filter isNorth walls) ++ [(i, y - 1) | i <- [0..x-1]] ++ map (reflect S) (map fst (filter isSouth walls) ++ [(i, 0) | i <- [0..x-1]]))
        allSortedSouth = sort (map fst (filter isSouth walls) ++ [(i, 0) | i <- [0..x-1]] ++ map (reflect N) (map fst (filter isNorth walls) ++ [(i, y - 1) | i <- [0..x-1]]))
        allSortedEast  = sort (map fst (filter isEast walls) ++ [(x - 1, j) | j <- [0..y-1]] ++ map (reflect W) (map fst (filter isWest walls) ++ [(0, j) | j <- [0..y-1]]))
        allSortedWest  = sort (map fst (filter isWest walls) ++ [(0, j) | j <- [0..y-1]] ++ map (reflect E) (map fst (filter isEast walls) ++ [(x - 1, j) | j <- [0..y-1]]))

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x, y) walls = 
>     Maze (x, y) (list2Tree (sort allSortedNorth))
>                 (list2Tree (sort allSortedSouth))
>                 (list2Tree (sort allSortedEast))
>                 (list2Tree (sort allSortedWest))
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

"member" is a function than sees whether a node is in the augmented binary tree

> member :: (Ord a, Eq a) => a -> Tree a -> Bool
> member x Leaf = False
> member x (Node xt y yt)
>  | (x < y)  = member x xt
>  | (x == y) = True
>  | (x > y)  = member x yt

> contains :: (Ord a) => (Tree a) -> a -> Bool
> contains Leaf _ = False
> contains (Node t1 v t2) x 
>	| x == v = True
>	| x  < v = contains t1 x 
>	| x  > v = contains t2 x

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size

LargeMaze to test code 

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls