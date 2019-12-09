import Data.List

main :: IO ()
main = do
    contents <- readFile "Crimes_2008.txt"
    let l = lines contents
    let w = map words l
    -- print w
    let len = length $ w !! 1
    print len
    -- let inputMatrix = matrixToDoubles w
    -- print inputMatrix

toDouble :: String -> Double
toDouble = read

listToDoubles xs = map toDouble xs
matrixToDoubles input = [ listToDoubles l | l <- input]

-- poslednych 7 prvkov v zozname su hodnoty zlocinov

unionLists xs ys = nub $ xs ++ ys

function x = do
    putStrLn("Input:")
    print x
    let a = 2 * x 
    print a