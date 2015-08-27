-- INTRODUCTION ---------------------------------------------------------------
--
--   Author:
--       Dr-Lord
--   Version:
--       0.4 26-27/08/2015
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


import Data.List (intersect, union, delete, (\\), nub, sort, filter)
import Control.Applicative ((<$>), (<*>))

type Group      = [Int]
type Sudoku     = [Group]
type SudokuPoss = [[(Int, Group)]]
type Coords     = (Int, Int)
data Status     = Solved | Simple | Groups | SubGroups | NPlets | Unsolved deriving (Eq, Ord, Show)

---- 2 - TESTING STUFF ---------------------------------------------------------

-- Infinite Loop for sudokus of file at lines: 45, 89

testSudokuSimple = "0 0 0 6 8 9 1 0 0\n\
                   \8 0 0 0 0 0 0 2 9\n\
                   \1 5 0 0 0 0 0 0 8\n\
                   \4 0 3 0 0 0 0 5 0\n\
                   \2 0 0 0 0 5 0 0 0\n\
                   \0 9 0 2 4 0 8 0 1\n\
                   \0 8 4 7 0 0 9 1 0\n\
                   \5 0 0 0 0 0 0 6 0\n\
                   \0 6 0 4 1 0 0 0 0"

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


tSimple = sudokise testSudokuSimple

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
fs = mapSudoku (\s (r,c)-> fst (s!!r!!c))
sn = mapSudoku (\s (r,c)-> snd (s!!r!!c))


---- 3 - TO DO -----------------------------------------------------------------

-- ADD: NON-DETERMINISTIC SOLUTION
--      I/O OF LISTS OF SUDOKUS, BOTH IN SINGLE ROWS OR SQUARE FORMATS
--      DIFFICULTY RATING BASED ON MAX STATUS REACHED
--      REVISE THE SOLVING PROCEDURE DIFFICULTY INCREASE: CONSIDER GOING DOWN IN SOME POSITIVE CASES

-- PROCEED TO MORE COMPLEX DEDUCTIONS: GOTO www.sudokudragon.com/sudokustrategy.htm OR SIMILAR: Sub-group exclusion rule, General permutation rule, then others
-- http://sudoku-solutions.com/ For hints


---- 4 - MAIN FUNCTIONS --------------------------------------------------------

-- Data Flow Functions

    -- IO Function: Read from file and write solution to other file
main = do
    contents <- readFile "Sudoku.txt"
    writeFile "Solution.txt" (stringify . solve Simple . sudokise $ contents)

    -- Transform Sudoku String into SudokuPoss data structure
sudokise :: String -> SudokuPoss
sudokise = mapSudoku addPoss . sudGrid
    where sudGrid = map (map (read :: String -> Int) . words) . lines
          addPoss s (r,c)
              | val == 0  = (val, posNums s (r,c))
              | otherwise = (val, [])
                where val = s!!r!!c

    -- Transform SudokuPoss data structure into String
stringify :: SudokuPoss -> String
stringify [[(0,[])]] = "UNSOLVED"
stringify sps = unlines . map unwords $ mapSudoku (\s (r,c)-> show . fst $ (s!!r!!c)) sps


-- Solving Functions

    -- Actual solving function, regulating solving algorithms flow
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
            NPlets    -> nPletsInNCells allGroups sps sps
            Unsolved  -> (Unsolved, sps)
          revisedStatus
            | solved sps = Solved
            | simple sps = Simple
            | otherwise  = status

    -- Fill in cells with only one possibility
uniqueChoices :: [Coords] -> SudokuPoss -> (Status,SudokuPoss)
uniqueChoices [] sps = (Groups, sps)
uniqueChoices ((r,c):rcs) sps
    | length p == 1 = uniqueChoices rcs (writeAndUpdate [(head p,(r,c))] sps)
    | otherwise     = uniqueChoices rcs sps
        where (s,p) = sps!!r!!c

    -- Check whether a number can only be in one cell in any group and act, otherwise proceed to further deductions
uniqueInGroup :: [Coords] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
uniqueInGroup [] prevSps sps
    | sps == prevSps = (SubGroups, sps)
    | otherwise      = (Groups,    sps)
uniqueInGroup ((r,c):rcs) prevSps sps
    | not $ null unRow = updateWith unRow
    | not $ null unCol = updateWith unCol
    | not $ null unSqu = updateWith unSqu
    | otherwise = uniqueInGroup rcs prevSps sps
        where updateWith unique = uniqueInGroup rcs prevSps (writeAndUpdate [(head unique,(r,c))] sps)
              [unRow, unCol, unSqu] = map (uniquePoss . delete (r,c)) $ getGroupsCoords (r,c)
              uniquePoss :: [Coords] -> [Int]
              uniquePoss groupCoords = cellPosses \\ otherPosses groupCoords
              cellPosses = snd (sps!!r!!c)
              otherPosses = concatMap (\(row,col)-> snd (sps!!row!!col))

    -- Check whether some numbers can only be in a single sub-group of any one group and then remove them from the other group, otherwise proceed to further deductions
justInSubGroup :: [Coords] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
justInSubGroup [] prevSps sps
    | sps == prevSps = (NPlets, sps)
    | otherwise      = (SubGroups, sps)
justInSubGroup ((r,c):rcs) prevSps sps
    | any (not . null) allOnlysP = justInSubGroup rcs prevSps newSps
    | otherwise                       = justInSubGroup rcs prevSps sps
        where newSps = foldr actOnGrid sps $ zip allOnlysP allRestsC
              actOnGrid :: (Group,[Coords]) -> SudokuPoss -> SudokuPoss
              actOnGrid (onlyInSG, rests) sps' = removeAndUpdate onlyInSG rests sps'

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


    -- Check whether a set of between 1 and 6 numbers in any group can only be in as many cells in it and act, otherwise declare Unsolved (Also covers the uniqueInGroup function: case of 1 num in 1 cell)
nPletsInNCells :: [[Coords]] -> SudokuPoss -> SudokuPoss -> (Status,SudokuPoss)
nPletsInNCells [] prevSps sps
  | sps == prevSps = (Unsolved, sps)
  | otherwise      = (NPlets, sps)
nPletsInNCells (rcs:rcss) prevSps sps = nPletsInNCells rcss prevSps newSps
  where newSps = actOnNPlets sps rcs


    -- Find set of n numbers in n cells in a group and remove those numbers from the other cells in the group
actOnNPlets :: SudokuPoss -> [Coords] -> SudokuPoss
actOnNPlets sps groupCoords = foldl remAndUpd sps nSetsTuples
    where remAndUpd sps' (rcs,nums) = removeAndUpdate nums (groupCoords \\ rcs) sps'
          nSetsTuples = filter ((==) <$> length . fst <*> length . snd) allCombsTuples
          allCombsTuples = map ((,) <$> map fst <*> foldr union [] . map (snd . snd)) allCombs
          allCombs = concatMap (`combinations` emptyCells) [1.. length emptyCells - 1]
          emptyCells = filter ((==0) . fst . snd) groupCells
          groupCells = map (\rc@(r,c) -> (rc, sps!!r!!c)) groupCoords



---- 5 - OTHER FUNCTIONS -------------------------------------------------------

  -- All Coordinates
allCoords = [(r,c) | r <- [0..8], c <- [0..8]] :: [Coords]

  -- All Groups' Coordinates
  -- NOTE: NOT by rows, cols or squs, but the three lists interspersed
allGroups = concatMap (\n->getGroupsCoords (n,n)) [0..8]


-- Sudoku checks

simple, solved :: SudokuPoss -> Bool
simple = any (any ((\p->length p == 1) . snd))
solved = all (all ((/=0) . fst))


-- Sudoku Transversal Functions

    -- Map any Sudoku type to another one
mapSudoku :: ([[a]] -> Coords -> b) -> [[a]] -> [[b]]
mapSudoku f anySudokuType = map (\row -> map (\col -> f anySudokuType (row, col)) [0..8]) [0..8]

    -- Map any Sudoku type just on some coordinates to a list of results
mapCoords :: (a -> b) -> [[a]] -> [Coords] -> [b]
mapCoords f anySudokuType = map (\(r,c)-> f $ anySudokuType!!r!!c)

  -- Map any Sudoku type just on some coordinates to "update" it: same type of output
mapSudokuOn :: (a -> a) -> [[a]] -> [Coords] -> [[a]]
mapSudokuOn f anySudokuType rcs = mapSudoku checkCell anySudokuType
  where checkCell anyS rc@(r,c)
          | rc `elem` rcs = f $ anySudokuType!!r!!c
          | otherwise     =     anySudokuType!!r!!c

 -- Same as above but with a distinction for a specific cell
 -- NOTE: The cell does not need to be in the Coords list as well
mapSudokuOnBut :: (a -> a) -> (a -> a) -> [[a]] -> [Coords] -> Coords -> [[a]]
mapSudokuOnBut f fCell anySudokuType rcs rcCell = mapSudoku checkCell anySudokuType
  where checkCell anyS rc@(r,c)
          | rc == rcCell  = fCell val
          | rc `elem` rcs = f val
          | otherwise     = val
            where val = anySudokuType!!r!!c


-- Sudoku Updating Functions

  -- Write values in specific cells and recursively cascade the grid's update
writeAndUpdate :: [(Int,Coords)] -> SudokuPoss -> SudokuPoss
writeAndUpdate []            sps = sps
writeAndUpdate ((n,rcC):cis) sps = case findUnique sps' rcs of
    []  -> sps'
    ucs -> writeAndUpdate (cis ++ ucs) sps'
  where sps' = mapSudokuOnBut (\(s,ps)->(s, delete n ps)) (const (n,[])) sps rcs rcC
        rcs = unitedCoords rcC

  -- Remove some values from some cells and recursively cascade the grid's update
removeAndUpdate :: [Int] -> [Coords] -> SudokuPoss -> SudokuPoss
removeAndUpdate nums rcs sps = case findUnique sps' rcs of
    []  -> sps'
    ucs -> writeAndUpdate ucs sps'
  where sps' = mapSudokuOn (\(s,ps)->(s, ps \\ nums)) sps rcs

  -- Produce a list of tuples of values and coordinates of cells which have a unique choice among the given coordinates
findUnique :: SudokuPoss -> [Coords] -> [(Int,Coords)]
findUnique sps = foldr unique []
  where unique rc@(r,c) acc = case snd $ sps!!r!!c of
          [newNum] -> (newNum,rc):acc
          _        -> acc


-- Possiblities and Groups Functions

    -- Possible numbers functions
getPosses :: SudokuPoss -> [Coords] -> Group
getPosses sps = nub . concat . mapCoords snd sps

    -- Given a Sudoku grid and a cell's coordinates, return its possible numbers
posNums :: Sudoku -> Coords -> Group
posNums sud (row, col) = groupPossesIntersect $ getGroups sud (row, col)

    -- Intersect all entries of a list of groups and return the missing numbers
groupPossesIntersect :: [Group] -> Group
groupPossesIntersect = ([1..9] \\) . foldr step []
    where step group []  = group
          step group acc = acc `union` group

    -- Given a cell's coordinates, return the union of the coordinates of its groups
unitedCoords :: Coords -> [Coords]
unitedCoords = foldr union [] . getGroupsCoords

    -- Given a Sudoku grid and a cell's coordinates, return its groups' contents
getGroups :: Sudoku -> Coords -> [Group]
getGroups sud = map (mapCoords id sud) . getGroupsCoords

    -- Given a cell's coordinates, return the coordinates lists of its groups
getGroupsCoords :: Coords -> [[Coords]]
getGroupsCoords (row, col) = [getRow, getCol, getSqu]
    where getRow = [(row,c) | c <- [0..8]]
          getCol = [(r,col) | r <- [0..8]]
          getSqu = [(3*sqRow+n,3*sqCol+m) | n <- [0..2], m <- [0..2]]
            where sqRow = row `div` 3 :: Int
                  sqCol = col `div` 3 :: Int


-- Other Functions

    -- Same function as in my GeneralFunctions package
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' _ _  [] = []
        combinations' n k' yys@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [yys]
          | otherwise = map (y:) nkMinus1 ++ nMinus1
            where nkMinus1 = combinations' (n-1) (k'-1) ys
                  nMinus1  = combinations' (n-1)  k'    ys
