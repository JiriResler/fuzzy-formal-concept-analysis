import Data.List
import System.IO

main :: IO ()
main = do 
---------------------------------------------------
  let inputTextFile = "input/crimes_2016.txt"
  let outputTextFile = "output/clusters_2016.txt"
---------------------------------------------------
  inputFile <- readFile inputTextFile
  let linesInput = separateLines inputFile
  let inputMatrix = matrixToDoubles $ map firstTwoOut linesInput
  let indecesBoroughs = indexAndBoroughList linesInput
  let inputClusters = dClusters inputMatrix
  riceSiffAlgorithm inputMatrix inputClusters inputClusters indecesBoroughs outputTextFile

riceSiffAlgorithm input inputClusters outputClusters indecesAndBoroughs outputTextFile =
  if (length inputClusters) > 1 
    then do
        let vzdialenosti = allDistances inputClusters input
        let vzdialenostiHodnoty = zoznamVzdial vzdialenosti
        let m = minimum vzdialenostiHodnoty 
        let clustreVzdial = clustersByMinimum vzdialenosti m
        let clustersWithoutDistance = tuplesWithoutDistance clustreVzdial
        let listClusters = listOfClusters clustersWithoutDistance 
        let v = nub listClusters 
        let u = unionClusters v
        let n = downArrow input $ upArrow input u
        let oldD = inputClusters
        let newD = unionLists (oldD \\ v) [n] 
        let c = unionLists outputClusters [n]
        riceSiffAlgorithm input newD c indecesAndBoroughs outputTextFile
    else do
        let cVerbose = indecesToBoroughsMatrix outputClusters indecesAndBoroughs
        outputFile <- openFile outputTextFile WriteMode
        hPrint outputFile cVerbose
        hClose outputFile
        putStrLn("") 
        putStrLn("Done. Check the output text file:") 
        putStrLn("") 
        print outputTextFile
        putStrLn("") 
        
separateLines input = map (wordsWhen (=='\t')) (lines input)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

stringToDouble :: String -> Double
stringToDouble = read
  
stringToInteger :: String -> Integer
stringToInteger = read

listToDoubles xs = map stringToDouble xs
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
-- [downArrow input (upArrow input [0])] ++ [downArrow input (upArrow input [1])]
dClusters input = [dCluster | idx <- [0..(length input) - 1], dCluster <- [downArrow input (upArrow input [idx])]]

-- Returns a list of minima of upArrows of two clusters
minimaUpArrowsList x1 x2 input = [m | idx <- [0..(length (input !! 0)) - 1], m <- [min ((upArrow input x1) !! idx) ((upArrow input x2) !! idx)]]

-- Returns a list of maxima of upArrows of two clusters
maximaUpArrowsList x1 x2 input = [m | idx <- [0..(length (input !! 0)) - 1], m <- [max ((upArrow input x1) !! idx) ((upArrow input x2) !! idx)]]

-- Returns a distance between two clusters, i.e. how similar they are
-- distance [0,3] [2,3] [[1, 0.5, 0], [0, 0.5, 1], [0.5, 0, 0.5], [1, 0.5, 0.5]
distance :: (Fractional a, Ord a) => [Int] -> [Int] -> [[a]] -> a
distance x1 x2 input = 1 - (sum (minimaUpArrowsList x1 x2 input)) / (sum (maximaUpArrowsList x1 x2 input))

allDistances dClusters input = [(x1, x2, d) | x1 <- dClusters, x2 <- dClusters, x1 /= x2, x1 <= x2,  d <- [distance x1 x2 input]]

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

unionLists xs ys = nub $ xs ++ ys

firstTwoOut (x:y:tail) = tail

indexAndBoroughList input = [ (stringToInteger index, borough) | idx <- [0..((length input) - 1)], index <- [(input !! idx) !! 0], borough <- [(input !! idx) !! 1]]

indecesToBoroughsList list indecesBoroughs = [ borough | idx <- [0..((length list) - 1)], borough <- [snd (indecesBoroughs !! (list !! idx))]]
indecesToBoroughsMatrix cluster indecesBoroughs = [ indecesToBoroughsList list indecesBoroughs | list <- cluster] 