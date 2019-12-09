import Data.List

main :: IO ()
main = do 

  putStrLn("____________________________________________________________________________________________________________________________________________________________________________________________")
  putStrLn("")
  putStrLn("line")
  putStrLn("Input:")
  putStrLn("")
  -- let input2 = [[1, 0.5, 0], [0, 0.5, 1], [0.5, 0, 0.5], [1, 0.5, 0.5]]
  -- contents <- readFile "test input crime values.txt"
  contents <- readFile "test_funkcnosti_programu.txt"
  let l = lines contents
  let w = map words l
  print w
  let input2 = matrixToDoubles w
  print input2
  -- print input2
  -- putStrLn("| 1.0 | 0.5 | 0.0 |")
  -- putStrLn("-------------------")
  -- putStrLn("| 0.0 | 0.5 | 1.0 |")
  -- putStrLn("-------------------")
  -- putStrLn("| 0.5 | 0.0 | 0.5 |")
  -- putStrLn("-------------------")
  -- putStrLn("| 1.0 | 0.5 | 0.5 |")
  putStrLn("")
  let inputClusters = dClusters input2
  putStrLn("Vstupné clustre pre algoritmus:")
  print inputClusters
  let vzdialenosti = allDistances inputClusters input2
  putStrLn("")
  putStrLn("Všetky vzdialenosti:")
  print vzdialenosti
  putStrLn("")
  let vzdialenostiHodnoty = zoznamVzdial vzdialenosti
  putStrLn("Hodnoty vzdialeností:")
  print vzdialenostiHodnoty
  putStrLn("")
  putStrLn("Minimum:")
  let m = minimum vzdialenostiHodnoty 
  print m
  putStrLn("")
  let clustreVzdial = clustersByMinimum vzdialenosti m
  putStrLn("Clustre s mininálnou vzdialenosťou:")
  print clustreVzdial
  putStrLn("")
  let clustersWithoutDistance = tuplesWithoutDistance clustreVzdial
  putStrLn("Epsilon:")
  print clustersWithoutDistance
  putStrLn("")
  let listClusters = listOfClusters clustersWithoutDistance 
  putStrLn("List of clusters:")
  print listClusters
  putStrLn("")
  let v = nub listClusters 
  putStrLn("V:")
  print v 
  putStrLn("")
  let u = unionClusters v
  let n = downArrow input2 $ upArrow input2 u
  putStrLn("N:")
  print n
  putStrLn("")
  let oldD = inputClusters
  putStrLn("Pôvodné D:")
  print oldD
  putStrLn("")
  putStrLn("Nové D:")
  let newD = union (oldD \\ v) [n] 
  print newD
  putStrLn("")
  let c = union oldD [n]
  putStrLn("Nové C:")
  print c
  putStrLn("")
  putStrLn("============================================================================================================================================================================================")

----------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------
  
toDouble :: String -> Double
toDouble = read
  
listToDoubles xs = map toDouble xs
matrixToDoubles input = [ listToDoubles l | l <- input]

-- Returns a column of a matrix specified by index idx
columnOnIndex [] _ = []
columnOnIndex (x:xs) idx = (x !! idx) : columnOnIndex xs idx

-- Returns columns of a matrix on specified indices idxs
columnsOnIndices (x:xs) idxs = [columnOnIndex (x:xs) y | y <- idxs]

-- Returns only rows of a matrix, which are specified by a list idxs
filterRows (x:xs) idxs = [(x:xs) !! y | y <- idxs] 

-- Returns lists of all columns of a given matrix - transposes a matrix
allColumns (x:xs) = columnsOnIndices (x:xs) [0..(length ((x:xs) !! 0)) - 1]

-- Returns a list of minima of columns of a matrix
minimaOfColumns (x:xs) = [minimum ys | ys <- allColumns (x:xs)]

-- Returns minima of columns of a matrix only from rows, which are specified by idxs
upArrow (x:xs) idxs = minimaOfColumns (filterRows (x:xs) idxs) 

-- Compares two lists whether all elements of list (x:xs) are greater or equal to elements of list (up:ups)
compareLists [] _ = True
compareLists (x:xs) (up:ups) = if x >= up
                                then compareLists xs ups
                               else False

-- Returns a list of indeces of rows, in which values in all columns exceed or are equal to values in a given list  
-- downArrow [[1, 0.5, 0], [0, 0.5, 1], [0.5, 0, 0.5], [1, 0.5, 0.5]] $ upArrow [[1, 0.5, 0], [0, 0.5, 1], [0.5, 0, 0.5], [1, 0.5, 0.5]] [0] 
downArrow xs up = [y | y <- [0..(length xs) - 1], compareLists (xs !! y) up]

-- Returns a list of indices of objects, which are in the same cluster
-- [downArrow input2 (upArrow input2 [0])] ++ [downArrow input2 (upArrow input2 [1])]
dClusters input = [dCluster | idx <- [0..(length input) - 1], dCluster <- [downArrow input (upArrow input [idx])]]

-- Returns a list of minima of upArrows of two clusters
minimaUpArrowsList x1 x2 input = [m | idx <- [0..(length (input !! 0)) - 1], m <- [min ((upArrow input x1) !! idx) ((upArrow input x2) !! idx)]]

-- Returns a list of maxima of upArrows of two clusters
maximaUpArrowsList x1 x2 input = [m | idx <- [0..(length (input !! 0)) - 1], m <- [max ((upArrow input x1) !! idx) ((upArrow input x2) !! idx)]]

-- Returns a distance between two clusters, i.e. how similar they are
-- distance [0,3] [2,3] [[1, 0.5, 0], [0, 0.5, 1], [0.5, 0, 0.5], [1, 0.5, 0.5]
distance :: (Fractional a, Ord a) => [Int] -> [Int] -> [[a]] -> a
distance x1 x2 input = 1 - (sum (minimaUpArrowsList x1 x2 input)) / (sum (maximaUpArrowsList x1 x2 input))

allDistances dClusters input = [(x1, x2, d) | x1 <- dClusters, x2 <- dClusters, x1 /= x2, d <- [distance x1 x2 input]]

first (value, _, _) = value
second (_, value, _) = value
third (_, _, value) = value

zoznamVzdial allDistances = [d | idx <- [0..((length allDistances) - 1)], d <- [third (allDistances !! idx)]]

clustersByMinimum allDistances m = [x | x <- allDistances, third x == m]

tuplesWithoutDistance minimalDistanceClusters = [(a, b) | idx <- [0..((length minimalDistanceClusters) - 1)], a <- [first (minimalDistanceClusters !! idx)], b <- [second (minimalDistanceClusters !! idx)]]

listFromTuplesFirst tuples = [ fst x | x <- tuples]
listFromTuplesSecond tuples = [ snd x | x <- tuples]

listOfClusters tuples = listFromTuplesFirst tuples ++ listFromTuplesSecond tuples

unionClustersWithDuplicates [] = []
unionClustersWithDuplicates (x:xs) = x ++ (unionClusters xs) 

unionClusters vClusters = nub $ unionClustersWithDuplicates vClusters