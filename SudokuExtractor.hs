import Data.List (intersperse, intercalate)

main = do
    contents <- readFile "3.txt"
    writeFile "30numSudokus.txt" (sudokuFormat . getRows [] . take 820 $ contents)

getRows :: [String] -> String -> [String]
getRows acc ""        = acc
getRows acc ('\n':ls) = getRows (acc++["\n"]) ls
getRows acc lines     = getRows (acc++[(take 9 lines)]) (drop 9 lines)

sudokuFormat :: [String] -> String
sudokuFormat = intercalate "\n" . map (intersperse ' ')