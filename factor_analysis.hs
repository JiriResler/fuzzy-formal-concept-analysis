main :: IO ()
main = do 
    let inputTextFile = "input/crimes_2008.txt"
    inputFile <- readFile inputTextFile
    let linesInput = separateLines inputFile
    let crimesMatrix = matrixToDoubles $ map firstTwoOut linesInput
    let cOutputClusters = [[1,5,7,9,11,12,17,20,26,29,31],[0,2,6,9,12,15,17,23,29],[3,20,26,31]]
    let elem = [1,5,7,9,11,12,17,20,26,29,31]
    let upArrowOneCluster = upArrow crimesMatrix elem
    print upArrowOneCluster
    -- let upArrowsAllClusters = map (upArrow crimesMatrix) cOutputClusters
    -- -- print upArrowsAllClusters
    let newMatrixForCluster = createMatrix [0..31] elem upArrowOneCluster
    print newMatrixForCluster



createMatrix idxs cluster upArrow = [ createList idx cluster upArrow | idx <- idxs ]

zeros 0 = []
zeros n = [0] ++ zeros (n - 1)

createList idx cluster upArrow = if idx `elem` cluster
                                   then upArrow
                                 else zeros $ length upArrow

separateLines input = map (wordsWhen (=='\t')) (lines input)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Returns minima of columns of a matrix only from rows, which are specified by idxs
upArrow (x:xs) idxs = minimaOfColumns (filterRows (x:xs) idxs)

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

listToDoubles xs = map stringToDouble xs
matrixToDoubles input = [ listToDoubles l | l <- input]

firstTwoOut (x:y:tail) = tail

stringToDouble :: String -> Double
stringToDouble = read