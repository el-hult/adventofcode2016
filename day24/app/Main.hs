{-

Day 24 is -- at the surface of it -- a travelling saleman problem.
We can do it in two steps:
 1) compute a distance matrix between all nodes
 2) solve the TSP with any solution.

For part A, I did a brute force solution. That worked out just fine.
For part B, I did a brute force solution too. That was fine.
-}
import Data.Array
import Debug.Trace (trace)
import Data.Char (isDigit)
import Data.List (nub, permutations)

type DuctMap = Array (Int, Int) Char

parseInput :: String -> DuctMap
parseInput s = 
  let ls = lines s
      h = length ls
      w = length (head ls)
  in listArray ((0, 0), (h - 1, w - 1)) (concat ls)


-- | neighbourDists val coord ductMap
-- Starting at `coord' where node `val' is located, compute the distances to all other nodes
-- To compute this, we do a BFS search.
-- When we find a wall, we dont progress
-- When we find a node with a digit value, we add the distance to it in the results and continue
-- when we find an open space (a '.' charater), we continue the flood fill by considering its neighbours
neighbourDists :: Char -> (Int, Int) -> DuctMap -> [(Char, Char, Int)]
neighbourDists val coord ductMap = go [(coord,0)] [] []
  where
    neighbours (x,y) d = map (\c -> (c,d+1)) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    go [] _ acc = acc
    go ((c,d):xs) visited acc
          | c `elem` visited =  go xs visited acc
          | otherwise = case ductMap ! c of
              '#' -> go xs visited acc
              '.' -> go (xs ++ (neighbours c d)) (c:visited) acc
              n | n == val -> go (xs ++ (neighbours c d)) (c:visited) acc
              n | isDigit n && n /= val -> go (xs ++ (neighbours c d)) (c:visited) ((val,n,d):acc)
              _ -> error ("Unexpected character in duct map: " ++ show (ductMap ! c) ++ " at " ++ show c)
            

-- | Compute the distances between all nodes in the duct map
-- A node is a integer character, such as '5'
-- the output is on format (fromNode, toNode, distance)
-- We compute it by flood filling
computeDistances :: DuctMap -> [(Char, Char, Int)]
computeDistances ductMap = 
  let 
      foo = filter (\(coord, val) -> isDigit val) .  assocs $ ductMap
      (nodeCoords, nodeVals) = unzip foo
  in
     trace ("Found nodes " ++ show nodeVals) $
     (trace ("The coords are " ++ show nodeCoords) )$
     concat [ neighbourDists v c ductMap | (c,v) <-  foo  ]

-- | shortestTour distances
-- Given a list of distances between nodes, compute the shortest tour that visits all nodes
-- The tour must start at 0, and it can end anywhere.
-- We compute this by brute force! Create all permutations of the nodes that start with 0,
-- compute the distance for each tour, and return the shortest one.
shortestTour1 :: [(Char, Char, Int)] -> Int
shortestTour1 distances =
  let 
    nodes = nub $ concat [[a, b] | (a, b, _) <- distances]
    tours = filter (\tour -> head tour == '0') (permutations nodes)
    tourDistance tour = sum [d | (a, b, d) <- distances, (a, b) `elem` zip tour (tail tour)]
  in
    minimum [tourDistance tour | tour <- tours]


-- | shortestTour2 distances
-- like the first one, but also ends at 0
shortestTour2 :: [(Char, Char, Int)] -> Int
shortestTour2 distances =
  let 
    nodes = nub $ concat [[a, b] | (a, b, _) <- distances]
    tours = filter (\tour -> head tour == '0' && last tour == '0') (permutations ('0':nodes))
    tourDistance tour = sum [d | (a, b, d) <- distances, (a, b) `elem` zip tour (tail tour)]
  in
    minimum [tourDistance tour | tour <- tours]



main :: IO ()
main = do
  input <- readFile "inputs/day24.txt"
  let theMap = parseInput input
  print theMap
  print $ theMap ! (0, 0) -- just to check that the parsing works
  let distances = computeDistances theMap
  print distances -- this should be a list of (fromNode, toNode, distance) tuples
  let ansOne = shortestTour1 distances
  print ansOne
  let ansTwo = shortestTour2 distances
  print ansTwo