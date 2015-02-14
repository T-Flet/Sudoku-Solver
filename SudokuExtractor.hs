-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.1 - 13-14/02/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Sudoku-Solver
--
--   Description:
--      This program extracts raw strings from a Sudoku containing file and
--      rewrites them in a human (and other program) readable format in another
--      file. It was created sometime during August 2014 and uploaded to GitHub
--      in January 2015.
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Main Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------

import Data.List (intersperse, intercalate)


---- 2 - MAIN FUNCTIONS --------------------------------------------------------

    -- IO Function: extracts strings from raw file and writes nice ones in another
main = do
    contents <- readFile "3.txt"
    writeFile "30numSudokus.txt" (sudokuFormat . getRows [] . take 820 $ contents)

    -- Split raw string into nice 9 Ints lists (Sudoku rows)
getRows :: [String] -> String -> [String]
getRows acc ""        = acc
getRows acc ('\n':ls) = getRows (acc++["\n"]) ls
getRows acc lines     = getRows (acc++[(take 9 lines)]) (drop 9 lines)

    -- Add spaces between Ints and newlines between rows
sudokuFormat :: [String] -> String
sudokuFormat = intercalate "\n" . map (intersperse ' ')
