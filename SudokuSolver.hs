-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.1 - 11-12/02/2015
--
--   Repository:
--       https://github.com/Dr-Lord/Sudoku-Solver
--
--   Description:
--      This program takes incomplete sudokus (0s for empty cells) from a file
--      and returns its solution in another file.
--      It was created sometime during August 2014 and uploaded to GitHub in
--      January 2015.
--
--   Sections:
--       1 - Imports and Type declarations
--       2 - Testing Stuff
--       3 - To Do
--       4 - Main Functions
--       5 - Other Functions

---- 1 - IMPORTS AND TYPE DECLARATIONS -----------------------------------------


import Data.List (intersect, union, delete, (\\), nub, sort)

type Group      = [Int]
type Sudoku     = [Group]
type SudokuPoss = [[(Int, Group)]]
type Coords     = (Int, Int)
data Status     = Solved | Simple | Groups | SubGroups | AnyGroups | Unsolved deriving (Eq, Ord, Show)

---- 2 - TESTING STUFF ---------------------------------------------------------

-- Infinite Loop for sudokus of file at lines: 45, 89

testSudoku = "4 0 0 0 0 0 0 2 0\n\
             \0 6 3 4 0 7 1 0 0\n\
             \5 0 0 0 8 3 0 6 0\n\
             \0 0 8 0 0 0 9 4 0\n\
             \0 3 0 9 0 0 0 0 8\n\
             \6 0 0 8 0 2 0 3 0\n\
             \0 0 0 7 0 0 5 0 4\n\
             \0 0 5 0 0 0 0 0 0\n\
             \8 0 7 5 1 0 2 0 0"

afterSimple = "4 0 0 0 0 0 3 2 0\n\
              \0 6 3 4 0 7 1 8 0\n\
              \5 0 0 0 8 3 4 6 0\n\
              \0 0 8 0 0 0 9 4 2\n\
              \0 3 0 9 0 0 6 5 8\n\
              \6 0 0 8 0 2 7 3 1\n\
              \0 0 0 7 0 0 5 1 4\n\
              \0 0 5 0 0 0 8 7 6\n\
              \8 4 7 5 1 6 2 9 3"

afterUnique = "4 8 0 0 0 0 3 2 7\n\
              \0 6 3 4 0 7 1 8 5\n\
              \5 7 0 0 8 3 4 6 9\n\
              \0 0 8 0 0 0 9 4 2\n\
              \0 3 0 9 0 0 6 5 8\n\
              \6 0 0 8 0 2 7 3 1\n\
              \0 0 6 7 0 8 5 1 4\n\
              \0 0 5 0 0 0 8 7 6\n\
              \8 4 7 5 1 6 2 9 3"


tSud = sudokise testSudoku
tAft = sudokise afterSimple
tAUn = sudokise afterUnique

stSud = mapSudoku (\sp (r,c) -> fst (sp!!r!!c)) tSud
stAft = mapSudoku (\sp (r,c) -> fst (sp!!r!!c)) tAft
stAUn = mapSudoku (\sp (r,c) -> fst (sp!!r!!c)) tAUn

pp :: (Show a) => [[a]] -> IO ()
pp  = mapM_ print
ppf, pps :: SudokuPoss -> IO ()
ppf = pp . fs
pps = pp . sn
fs = mapSudoku (\s (r,c)-> fst $ (s!!r!!c))
sn = mapSudoku (\s (r,c)-> snd $ (s!!r!!c))


---- 3 - TO DO -----------------------------------------------------------------

-- ADD: NON-DETERMINISTIC SOLUTION
--      I/O OF LISTS OF SUDOKUS, BOTH IN SINGLE ROWS OR SQUARE FORMATS
--      DIFFICULTY RATING BASED ON MAX STATUS REACHED
--      REVISE THE SOLVING PROCEDURE DIFFICULTY INCREASE: CONSIDER GOING DOWN IN SOME POSITIVE CASES


---- 4 - MAIN FUNCTIONS --------------------------------------------------------

-- Data Flow Functions

main = do
    contents <- readFile "Sudoku.txt"
    writeFile "Solution.txt" (stringify . solve Simple . sudokise $ contents)

sudokise :: String -> SudokuPoss
sudokise = mapSudoku addPoss . sudGrid
    where sudGrid = map (map (read :: String -> Int) . words) . lines
          addPoss s (r,c)
              | val == 0  = (val, posNums s (r,c))
              | otherwise = (val, [])
                where val = s!!r!!c

stringify :: SudokuPoss -> String
stringify = unlines . map unwords . mapSudoku (\s (r,c)-> show . fst $ (s!!r!!c))


-- Solving Functions

solve :: Status -> SudokuPoss -> SudokuPoss
solve status sps
    | status == Solved   = sps
    | status == Unsolved = [[(0,[])]]
    | otherwise          = solve newStatus newSps
    where (newStatus, newSps) = case revisedStatus of
            Solved    -> (Solved, sps)
            Simple    -> uniqueChoices  allCoords sps
            Groups    -> uniqueInGroup  allCoords sps sps
            SubGroups -> justInSubGroup allCoords sps sps
            -- AnyGroups -> justInAnyGroup allCoords sps sps
            Unsolved  -> (Unsolved, sps)
          revisedStatus
            | solved sps = Solved
            | simple sps = Simple
            | otherwise  = status

    -- Fill in cells with only one possibility
uniqueChoices :: [Coords] -> SudokuPoss -> (Status,SudokuPoss)
uniqueChoices [] sps = (Groups, sps)
uniqueChoices ((r,c):rcs) sps
    | length p == 1 = uniqueChoices rcs (updateGrids (head p) (r,c) sps)
    | otherwise     = uniqueChoices rcs sps
        where (s,p) = sps!!r!!c

    -- Check whether a number can only be in one cell in any group and act, otherwise proceed to further deductions
uniqueInGroup :: [Coords] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
uniqueInGroup [] prevSps sps
    | sps == prevSps = (SubGroups, sps)
    | otherwise      = (Groups,    sps)
uniqueInGroup ((r,c):rcs) prevSps sps
    | not . null $ unRow = updateWith unRow
    | not . null $ unCol = updateWith unCol
    | not . null $ unSqu = updateWith unSqu
    | otherwise = uniqueInGroup rcs prevSps sps
        where updateWith unique = uniqueInGroup rcs prevSps (updateGrids (head unique) (r,c) sps)
              [unRow, unCol, unSqu] = map (uniquePoss . (delete (r,c))) $ getGroupsCoords (r,c)
              uniquePoss :: [Coords] -> [Int]
              uniquePoss groupCoords = cellPosses \\ (otherPosses groupCoords)
              cellPosses = snd (sps!!r!!c)
              otherPosses = concat . map (\(row,col)-> snd (sps!!row!!col))

    -- Check whether a number can only be in a single sub-group of any one group and then remove them from the other group, otherwise proceed to further deductions
justInSubGroup :: [Coords] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
justInSubGroup [] prevSps sps
    | sps == prevSps = (Unsolved, sps)--(AnyGroups, sps)
    | otherwise      = (SubGroups, sps)
justInSubGroup ((r,c):rcs) prevSps sps
    | or $ map (not . null) allOnlysP = justInSubGroup rcs prevSps newSps
    | otherwise                       = justInSubGroup rcs prevSps sps
        where newSps = foldr actOnGrid sps $ zip allOnlysP allRestsC
              actOnGrid :: (Group,[Coords]) -> SudokuPoss -> SudokuPoss
              actOnGrid (onlyInSG, rests) acc = mapSudoku (remove onlyInSG rests) acc

                    -- Suffixes:  S:Sub, G:Group, C:Coordinates, P:Possibilities

              allOnlysP = [srSGNotInRowP, srSGNotInSquP, scSGNotInColP, scSGNotInSquP] -- Correct associations
              allRestsC = [srSquRestC,    srRowRestC,    scSquRestC,    scColRestC]    -- Think about it, XD

                -- Possibilities only in SubGroups with respect to rest of rows/cols and squares
              [srSGNotInRowP, srSGNotInSquP] = map (srSGP \\) [srRowRestP, srSquRestP]
              [scSGNotInColP, scSGNotInSquP] = map (scSGP \\) [scColRestP, scSquRestP]

                -- From Coordinates to nubbed possibilities
              [srSGP, scSGP, srRowRestP, srSquRestP, scColRestP, scSquRestP] = map (getPosses sps) [srSGC, scSGC, srRowRestC, srSquRestC, scColRestC, scSquRestC]

                -- Acquire Coordinates
              [rowGC, colGC, squGC] = getGroupsCoords (r,c)
              [srSGC, scSGC] = map (intersect squGC) [rowGC, colGC]
              [srRowRestC, srSquRestC] = map (\\ srSGC) [rowGC, squGC]
              [scColRestC, scSquRestC] = map (\\ scSGC) [colGC, squGC]

    -- Check whether a set of between 1 and 6 numbers in a group can only be in as many cells in it and act, otherwise declare Unsolved (Also covers the uniqueInGroup function: case of 1 num in 1 cell)
-- justInAnyGroup :: [Coords] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
-- justInAnyGroup [] prevSps sps
    -- | sps == prevSps = (Unsolved, sps)
    -- | otherwise      = (AnyGroups, sps)
-- justInAnyGroup ((r,c):rcs) prevSps sps
    -- | or $ map (not . null) allSetsP = justInAnyGroup rcs prevSps newSps
    -- | otherwise                      = justInAnyGroup rcs prevSps sps
        -- where


                ---- Acquire Coordinates
              -- [rowGC, colGC, squGC] = getGroupsCoords (r,c)


-- PROCEED TO MORE COMPLEX DEDUCTIONS: GOTO www.sudokudragon.com/sudokustrategy.htm OR SIMILAR: Sub-group exclusion rule, General permutation rule, then others
-- http://sudoku-solutions.com/ For hints



---- 5 - OTHER FUNCTIONS -------------------------------------------------------

    -- Map any Sudoku type to another one
mapSudoku :: ([[a]] -> Coords -> b) -> [[a]] -> [[b]]
mapSudoku f anySudokuType = map (\row -> map (\col -> f anySudokuType (row, col)) [0..8]) [0..8]

    -- Map any Sudoku type just on some coordinates to a list of results
mapCoords :: (a -> b) -> [[a]] -> [Coords] -> [b]
mapCoords f anySudokuType = map (\(r,c)-> f $ anySudokuType!!r!!c)

    -- All Coordinates
allCoords = [(r,c) | r <- [0..8], c <- [0..8]] :: [Coords]

    -- Sudoku checks
simple, solved :: SudokuPoss -> Bool
simple = or  . map (any ((\p->length p == 1) . snd))
solved = and . map (all ((/=0) . fst))

    -- Write and remove a number
updateGrids :: Int -> Coords -> SudokuPoss -> SudokuPoss
updateGrids num rc@(row,col) sps = mapSudoku (writeAndRemove (unitedCoords rc)) sps
    where writeAndRemove :: [Coords] -> (SudokuPoss -> Coords -> (Int, [Int]))
          writeAndRemove rcs sps (r,c)
            | (r,c) == rc                      = (num, [])
            | (r,c) `elem` rcs && num `elem` p = if length p' == 1 then (head p', p') else (s, p')
            | otherwise                        = val
                where p' = delete num p
                      val@(s, p) = sps!!r!!c

    -- Just remove a list of numbers
remove :: [Int] -> [Coords] -> (SudokuPoss -> Coords -> (Int, Group))
remove ns rcs anySudokuType (r,c)
    | (r,c) `elem` rcs = if length p' == 1 then (head p', p') else (s, p')
    | otherwise        = val
        where p' = (p \\ ns)-- If any of ns is in p they will be removed, otherwise they will not be.
              val@(s, p) = anySudokuType!!r!!c

    -- Find set of n numbers in n cells in a group
-- findSets :: SudokuPoss -> [Coords] -> [(Group,[Coords])]
-- findSets sps groupCoords = zip setsList coordsList -- DO: get all posses->indicise nums by cells in which they are->try all n-subsets to be in n cells
    -- where (setsList, coordsList) = foldr step ([],[]) [1..maxNs]
          -- step numsNumber (sets, coords)
            -- | cellsWithOnlySet =
            -- | inNoOtherCells   =
            -- | otherwise        =
                -- where




          -- maxNs = length posses - 3 -- (sets of 4 can only be present in 7+ unfilled cells, of 5 in 8+, of 6 in 9)

          -- posses = mapCoords snd sps groupCoords

          -- lookup'' n = numsIndex!!(n-1)
          -- numsIndex = map (find.(:[])) [1..9]
          -- find nums = filter (\(r,c)->null $ nums \\ (snd $ sps!!r!!c)) groupCoords


    -- Possible numbers functions
getPosses :: SudokuPoss -> [Coords] -> Group
getPosses sps = nub . concat . mapCoords snd sps

posNums :: Sudoku -> Coords -> Group
posNums sud (row, col) = groupPossesIntersect $ getGroups sud (row, col)

groupPossesIntersect :: [Group] -> Group
groupPossesIntersect = ([1..9] \\) . foldr step []
    where step group []  = group
          step group acc = union acc group

    -- Groups Coordinates Functions
unitedCoords :: Coords -> [Coords]
unitedCoords = foldr step [] . getGroupsCoords
    where step coords []  = coords
          step coords acc = union coords acc

getGroups :: Sudoku -> Coords -> [Group]
getGroups sud = map (mapCoords id sud) . getGroupsCoords

getGroupsCoords :: Coords -> [[Coords]]
getGroupsCoords (row, col) = [getRow, getCol, getSqu]
    where getRow = [(row,c) | c <- [0..8]]
          getCol = [(r,col) | r <- [0..8]]
          getSqu = [(3*sqRow+n,3*sqCol+m) | n <- [0..2], m <- [0..2]]
            where sqRow = row `div` 3 :: Int
                  sqCol = col `div` 3 :: Int
