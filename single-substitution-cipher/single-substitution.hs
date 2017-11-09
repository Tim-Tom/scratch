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

baseInclusion :: State -> [Bool]
baseInclusion s =
  (map (\_ -> False) (candidates s))

mergeInclusion :: [Bool] -> [Bool] -> [Bool]
mergeInclusion = zipWith (||)

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
          (filtered, removed) = List.partition (\e -> e /= mapped) v
          success' = success && (not (null filtered))
        in case (null removed) of
          True  -> ((success', altered), filtered)
          False -> ((success', k:altered), filtered)
    ((success, altered), newCharacters) = Map.mapAccumWithKey remove (True, []) (characters s)
  in case success of
    False -> (False, [], s)
    True ->
      let
        constrained = updateCharacters s newCharacters
        alteredCharacters = map (\l -> (l, getItem l newCharacters)) altered
        (newUnique, justAltered) = List.partition (\(k, v) -> null (tail v)) alteredCharacters
        baseInc = foldl mergeInclusion (getItem c (inclusion constrained)) (map (\(k, _) -> (getItem k (inclusion constrained))) justAltered)
        makeFurtherUnique (False, _, _) _ = (False, [], s)
        makeFurtherUnique (True, inc, s') (from, to:[]) =
          let
            (success, newInc, s'') = makeUnique s' from to
          in (success, (mergeInclusion inc newInc), s'')
      in List.foldl' makeFurtherUnique (success, baseInc, constrained) newUnique

constrainLetter :: State -> [Bool] -> Char -> [Char] -> (Bool, State, [Bool])
constrainLetter s inc c possible =
  let
    previous = (getItem c (characters s))
    intersection = List.intersect previous possible
    size = length intersection
    shrunk = size < (length previous)
  in case (size, shrunk) of
    (0, _) -> (False, s, inc)
    (_, False) -> (True, s, inc)
    (1, True)  ->
      let
        (success, inc', s') = makeUnique s c (head intersection)
        inc'' = mergeInclusion inc inc'
      in (success, s', inc'')
    (_, True)  ->
      let
        newCharacters = Map.adjust (\_ -> intersection) c (characters s)
        s' = updateCharacters s newCharacters
        inc' = mergeInclusion inc (getItem c (inclusion s))
      in (True, s', inc')

constrainLetters :: State -> String -> [String] -> (Bool, State, [Bool])
constrainLetters s candidate words =
  let
    updateSeenForLetter :: (Char, (Char, [Char])) -> (Char, [Char])
    updateSeenForLetter (l, (c, seen)) = if (elem l seen) then (c, seen) else (c, l:seen)
    updateSeenForWord :: [(Char, [Char])] -> String -> [(Char, [Char])];
    updateSeenForWord seen word = map updateSeenForLetter (zip word seen)
    seen = foldl updateSeenForWord (map (\c -> (c, [])) candidate) words
    constrainLetterWrap (success, s, inc) (c, possible)
      | success == False = (False, s, inc)
      | otherwise = constrainLetter s inc c possible
  in
    foldl constrainLetterWrap (True, s, baseInclusion s) seen

filterPossibleWords :: State -> String -> [String] -> (Bool, Bool, [String])
filterPossibleWords s candidate words =
  let
    possibilities = map (\c -> (getItem c (characters s))) candidate
    passes word = all (\(l, possible) -> elem l possible) (zip word possibilities)
    (filtered, removed) = List.partition passes words
  in ((not (null filtered)), (not (null removed)), filtered)

checkConstraint :: State -> [Bool] -> (Bool, String, [String]) -> (Bool, State, [String], [Bool])
checkConstraint s inc (False, candidate, words) = (True, s, words, inc)
checkConstraint s inc (True, candidate, words) =
  let
    (success, changed, newWords) = filterPossibleWords s candidate words
  in case (success, changed) of
    (False, _) -> (False, s, [], inc)
    (True, False) -> (True, s, newWords, inc)
    (True, True) ->
      let
        (success', s', inc') = constrainLetters s candidate newWords
      in (success', s', newWords, (mergeInclusion inc inc'))

checkConstraints :: (Bool, [Bool], State) -> (Bool, State)
checkConstraints (False, _, s) = (False, s)
checkConstraints (True, altered, s) =
  let
    reducer :: (Bool, String, [String]) -> (Bool, State, [[String]], [Bool]) -> (Bool, State, [[String]], [Bool])
    reducer _ (False, s, possible, inc) = (False, s, possible, inc)
    reducer pkg (True, s, possible, inc) =
      let
        (success, s', newPossible, inc') = checkConstraint s inc pkg
      in
        (success, s', newPossible:possible, inc')
    (success, s', newPossibleWords, altered') = foldr reducer (True, s, [], (baseInclusion s)) (zip3 altered (candidates s) (possibleWords s))
    s'' = updateWords s' newPossibleWords
  in if any (id) altered' then checkConstraints (success, altered', s'') else (success, s'')

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
