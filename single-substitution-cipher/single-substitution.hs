import qualified Data.Map.Strict as Map;
import qualified Data.List as List;
import qualified Data.Char as Char;
import Text.Regex.PCRE

data State = State {
  candidates :: [String],
  characters :: Map.Map Char [Char],
  possibleWords :: [[String]],
  inclusion :: Map.Map Char [Bool]
} deriving Show;

updateWords :: State -> [[String]] -> State
updateWords s words = State {
  candidates = (candidates s),
  characters = (characters s),
  possibleWords = words,
  inclusion = (inclusion s)
}
updateCharacters :: State -> Map.Map Char [Char] -> State
updateCharacters s characters = State {
  candidates = (candidates s),
  characters = characters,
  possibleWords = (possibleWords s),
  inclusion = (inclusion s)
}

getItem :: Ord k => k -> Map.Map k [v] -> [v]
getItem k m =
  Map.findWithDefault [] k m

makePatternRec :: String -> [Char] -> Int -> String -> String
makePatternRec [] _ _ p = reverse p
makePatternRec (c:s) pi len p =
  let (i, npi, nlen) =
        case List.findIndex (== c) pi of
          Just n -> ((len - n), pi, len)
          Nothing -> (len, (c:pi), (len + 1))
  in let l = (Char.chr ((Char.ord 'a') + i))
  in makePatternRec s npi nlen (l:p)

makePattern :: String -> String
makePattern s =
  makePatternRec s [] 0 ""

toLower :: String -> String
toLower s = map Char.toLower s

getWordsRec :: String -> [String] -> [String]
getWordsRec "" ("":words) = reverse words
getWordsRec "" words = reverse words
getWordsRec str words =
  let (before, match, after) = str =~ "[a-zA-Z]+" :: (String, String, String)
  in
    getWordsRec after (match:words)

getWords :: String -> [String]
getWords s = List.nub (map toLower (getWordsRec s []))

getPossibilities :: [String] -> [String] -> Map.Map String [String]
getPossibilities words patterns =
  let
    patterned = map (\w -> ((makePattern w), w)) words
    filtered = filter (\(p, _) -> elem p patterns) patterned
    emptySets = Map.fromList (map (\p -> (p, [])) patterns)
  in foldl (\ result (p,e) -> Map.adjust (\soFar -> e:soFar) p result) emptySets filtered

makeUnique :: State -> Char -> Char -> (Bool, [Bool], State)
makeUnique s c mapped =
  let
    remove (success, altered) k v
      | k == c = ((success, altered), [mapped])
      | otherwise =
        let
          (removed, filtered) = List.partition (\e -> e == mapped) v
          success' = success && (not (null filtered))
        in case (null removed) of
          True  -> ((success', altered), filtered)
          False -> ((success', k:altered), filtered)
    ((success, altered), newCharacters) = Map.mapAccumWithKey remove (True, []) (characters s)
    constrained = updateCharacters s newCharacters
    makeFurtherUnique (False, _, _) _ = (False, [], s)
    makeFurtherUnique (True, inc, s') (from, to:[]) =
      let
        (success, newInc, s'') = makeUnique s' from to
      in (success, (zipWith (||) inc newInc), s'')
  in List.foldl' makeFurtherUnique (success, (getItem c (inclusion constrained)), constrained) (filter (\(_, v) -> null (tail v)) (map (\k -> (k, getItem k newCharacters)) altered))

checkConstraint :: Bool -> (Bool, String, [String]) -> State -> (Bool, [String])
checkConstraint False _ s = (False, [])
checkConstraint True (False, _, words) s = (True, words)
checkConstraint True (True, candidate, words) s =
  let
    possibilities = map (\c -> (getItem c (characters s))) candidate
    passes word = all (\(l, possible) -> elem l possible) (zip word possibilities)
    filtered = filter passes words
  in ((not (null filtered)), filtered)

checkConstraints :: (Bool, [Bool], State) -> (Bool, State)
checkConstraints (False, _, s) = (False, s)
checkConstraints (True, altered, s) =
  let
    reducer :: (Bool, [[String]]) -> (Bool, String, [String]) -> (Bool, [[String]])
    reducer (success, soFar) stuff =
      let
        (success', newPossible) = checkConstraint success stuff s
      in
        (success', newPossible:soFar)
    (success, newPossibleWords) = foldl reducer (True, [])(zip3 altered (candidates s) (possibleWords s))
    s' = updateWords s newPossibleWords
  in (success, s')
  
guess :: State -> Char -> [Char] -> [Char] -> [Map.Map Char Char] -> [Map.Map Char Char]
guess s c [] _ solutions = solutions
guess s c (choice:remain) cs solutions =
  let
    newSolutions = case checkConstraints (makeUnique s c choice) of
      (True, constrained) -> (solveRec constrained cs solutions)
      (False, _) -> solutions
  in guess s c remain cs newSolutions;

solveRec :: State -> [Char] -> [Map.Map Char Char] -> [Map.Map Char Char]
solveRec s [] solutions =
  -- Lazy here. I should put better error messages in for debug
  -- assert (all character lists have one element)
  -- assert (all words have one possibilitiy)
  (Map.map (\(e:[]) -> e) (characters s)) : solutions
solveRec s (c:cs) solutions
  | length (getItem c (characters s)) == 1 =
    solveRec s cs solutions
  | otherwise =
    guess s c (getItem c (characters s)) cs solutions

solve :: State -> [Map.Map Char Char]
solve  s = solveRec s (Map.keys (characters s)) []

-- For interactive purposes
makeState :: IO State
makeState =
  do
    encrypted <- readFile "test.txt"
    allPossibilities <- readFile "words.txt"
    let
      words = getWords encrypted
      patterns = List.nub (List.map makePattern words)
      letters = List.nub (concat words)
      inclusion = Map.fromList (map (\l -> (l, map (elem l) words)) letters)
      possible = getPossibilities (lines allPossibilities) patterns
      state = State {
        candidates = words,
        characters = Map.fromList (map (\l -> (l, ['a' ..'z'])) letters),
        possibleWords = map (\w -> getItem (makePattern w) possible) words,
        inclusion = inclusion
      }
    return state

main :: IO ()
main =
  do
    encrypted <- readFile "test.txt"
    allPossibilities <- readFile "words.txt"
    let
      words = getWords encrypted
      patterns = List.nub (List.map makePattern words)
      letters = List.nub (concat words)
      inclusion = Map.fromList (map (\l -> (l, map (elem l) words)) letters)
      possible = getPossibilities (lines allPossibilities) patterns
      state = State {
        candidates = words,
        characters = Map.fromList (map (\l -> (l, ['a' ..'z'])) letters),
        possibleWords = map (\w -> getItem (makePattern w) possible) words,
        inclusion = inclusion
      }
    putStr (show (solve state))
