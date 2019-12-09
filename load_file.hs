import Data.List

main :: IO ()
main = do
    contents <- readFile "input/Crimes_2008.txt"
    let linesInput = separateLines contents
    -- print linesInput

    let indecesBoroughs = indexAndBorough linesInput
    -- print indecesBoroughs
    
    let inputMatrix = matrixToDoubles $ map firstTwoOut linesInput
    -- print inputMatrix

    let c = [[1],[2,3],[0,3],[0,1,2,3]]
    print "C:"
    -- print c
    let cVerbose = indexToBoroughMatrix c indecesBoroughs
    print "C with boroughs"
    -- print cVerbose
    print "output.txt" c

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

unionLists xs ys = nub $ xs ++ ys

function :: (Show a, Num a) => a -> IO ()
function x = do
    putStrLn("Input:")
    print x
    let a = 2 * x 
    print a

firstTwoOut (x:y:tail) = tail

indexAndBorough input = [ (stringToInteger index, borough) | idx <- [0..((length input) - 1)], index <- [(input !! idx) !! 0], borough <- [(input !! idx) !! 1]]

indextoBoroughList list indecesBoroughs = [ borough | idx <- [0..((length list) - 1)], borough <- [snd (indecesBoroughs !! (list !! idx))]]
indexToBoroughMatrix cluster indecesBoroughs = [ indextoBoroughList l indecesBoroughs | l <- cluster ] 