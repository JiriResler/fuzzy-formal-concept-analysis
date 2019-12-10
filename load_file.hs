import Data.List
import System.IO

main :: IO ()
main = do
    contents <- readFile "input/Crimes_2008.txt"
    let linesInput = separateLines contents
    let indecesBoroughs = indexAndBoroughList linesInput
    let inputMatrix = matrixToDoubles $ map firstTwoOut linesInput
    let c = [[1],[2,3],[0,3],[0,1,2,3]]
    let cVerbose = indecesToBoroughsMatrix c indecesBoroughs
    let outputTextFile = "output/testOutput.txt"
    out <- openFile outputTextFile WriteMode
    hPrint out cVerbose
    hClose out

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

indexAndBoroughList input = [ (stringToInteger index, borough) | idx <- [0..((length input) - 1)], index <- [(input !! idx) !! 0], borough <- [(input !! idx) !! 1]]

indecesToBoroughsList list indecesBoroughs = [ borough | idx <- [0..((length list) - 1)], borough <- [snd (indecesBoroughs !! (list !! idx))]]
indecesToBoroughsMatrix cluster indecesBoroughs = [ indecesToBoroughsList list indecesBoroughs | list <- cluster] 