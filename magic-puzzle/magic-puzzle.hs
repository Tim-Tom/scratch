data Action
  = Choose
  | Decide [Int]
  | Validate [Int]
  | Solution
  deriving Show

goal :: Int
goal = 21

width :: Int
width = 3

wm :: Int
wm = width - 1

wp :: Int
wp = width + 1

size :: Int
size = width * width

choices :: [Int]
choices = [3,4 .. 11] -- [1,2 .. width*width]

foo :: Int -> Int
foo x = x + 5
perform :: Action -> [Action] -> [Int] -> [[Int]] -> [[Int]]
perform Choose (nextAction:rest) picked so_far =
  let
    available x = not $ any (==x) picked
    available_choices = filter available choices
  in foldr (\chosen so_far -> perform nextAction rest (chosen:picked) so_far) so_far available_choices

perform (Decide indexes) (nextAction:rest) picked so_far =
  let
    available x = not $ any (==x) picked
    vals = map (\x -> picked !! x) indexes
    chosen = goal - (sum vals)
    good = any (==chosen) choices && available chosen
  in
    if good then perform nextAction rest (chosen:picked) so_far else so_far

perform (Validate indexes) (nextAction:rest) picked so_far =
  let
    vals = map (\x -> picked !! x) indexes
    calculated = sum vals
  in
    if calculated == goal then perform nextAction rest picked so_far else so_far

perform Solution [] picked so_far =
  picked:so_far

main :: IO ()
main =
  let
    -- actions = [
    --     Choose,                 --  0 = 0
    --     Choose,                 --  1 = 1
    --     Choose,                 --  2 = 2
    --     Decide (ind 3 [0,1,2]),   --  3 = 3 -> [0,1,2]
    --     Choose,                 --  4 = 5
    --     Choose,                 --  5 = 9
    --     Decide (ind 6 [1,4,5]),   --  6 = 13 -> [1,5,9]
    --     Choose,                 --  7 = 10
    --     Decide (ind 8 [0,4,7]),   --  8 = 15 -> [0,5,10]
    --     Choose,                 --  9 = 6
    --     Decide (ind 10 [2,9,7]),  -- 10 = 14 -> [2,6,10]
    --     Decide (ind 11 [6,10,8]), -- 11 = 12 -> [13,14,15]
    --     Choose,                 -- 12 = 4
    --     Decide (ind 13 [4,5,6]),  -- 13 = 7 -> [4,5,6]
    --     Decide (ind 14 [0,4,12]), -- 14 = 8 -> [0,4,12]
    --     Decide (ind 15 [8,9,10]), -- 15 = 11 -> [8,9,10]
    --     Validate (ind 16 [wm, wm+width .. size]), -- 16 = -> [3,7,11,15]
    --     Validate (ind 17 [wm, wm+wm .. size - width]), -- 17 -> [3,6,9,12]
    --     Solution]
    
    -- action_map is the action that assigns the given normal array index.
    action_map = [0,1,2,6,3,5,7,8,4]
    ind x lst = map (\y -> x - 1 - (action_map !! y)) lst
    actions = [
      Choose,                    --                  [0]
      Choose,                    --                [1,0]
      Decide   (ind 2 [0,1]),    --              [2,1,0]
      Choose,                    --            [4,2,1,0]
      Decide   (ind 4 [0,4]),    --          [8,4,2,1,0]
      Decide   (ind 5 [2,8]),    --        [5,8,4,2,1,0]
      Decide   (ind 6 [4,5]),    --      [3,5,8,4,2,1,0]
      Decide   (ind 7 [0,3]),    --    [6,3,5,8,4,2,1,0]
      Decide   (ind 8 [6,8]),    --  [7,6,3,5,8,4,2,1,0]
      Validate (ind 9 [1,4,7]),  --  [7,6,3,5,8,4,2,1,0]
      Validate (ind 9 [2,4,6]),  --  [7,6,3,5,8,4,2,1,0]
      Solution]
    (h:t) = actions
    -- solution_map is the mapping from the picked index back to normal form
    solution_map = [8,7,6,2,5,3,1,0,4]
    solutions = perform h t [] []
    mapped_solutions = map (\solution -> map (\i -> solution !! i) solution_map) solutions
    mapi :: (a -> Int -> b) -> [a] -> [b]
    mapi f l = zipWith f l [1..]
    chunked_print (a:b:c:rst) so_far =
      let
        df :: Int -> String
        df d = let str = show d in if (length str) == 1 then " " ++ str else str
        fmted = (df a) ++ " " ++ (df b) ++ " " ++ (df c) ++ "\n"
        result = so_far ++ fmted
      in case rst of
        [] -> result
        rst -> chunked_print rst result
  in mapM_ putStr (mapi (\solution index -> chunked_print solution ("--- Solution " ++ (show index) ++ " ---\n")) mapped_solutions)
