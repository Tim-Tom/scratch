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
    let
      s2 = State {
        candidates = (candidates s),
        inclusion = (inclusion s),
        possibleWords = (possibleWords s),
        characters = (Map.adjust (\_ -> [c]) c (characters s))
      } in solveRec s2 cs solutions

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
      };
    putStr (show (solve state))
