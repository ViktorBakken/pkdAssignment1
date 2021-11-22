-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing(wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import Test.HUnit -- provides testing framework
import PandP      -- provide sample text to play with (variable austin)
import Data.List
-- DO NOT CHANGE THESE TYPES
type Sentence = [String]
type Document = [Sentence]
type WordTally = [(String, Int)]
type Pairs = [(String, String)]
type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS
-- Funktion 1

{- wordCountAux k lst
Creates a wordTally for a sentence.
   PRE: k == 1, k :: Int, lst is sorted, lst :: String
   RETURNS: WordTally for a sentence
   EXAMPLES: wordCountAux 1 (sort ["a", "rose", "is", "a", "rose"]) == [("a",2),("is",1),("rose",2)]
             wordCountAux 1 (sort ["but", "so", "is", "a", "rose"]) == [("a",1),("but",1),("is",1),("rose",1),("so",1)]
-}
wordCountAux :: Int -> [String] -> WordTally
wordCountAux k [] = []
wordCountAux k [x] = [(x, 1)]
--VARIANT: lst length
wordCountAux k (x:y:xs) 
  | x == y = 
    if xs == [] then [(x, k + 1)] 
      else wordCountAux (k + 1) (y:xs)
  | otherwise = (x, k) :wordCountAux 1 (y:xs)

{- wordCount Document
Creates a wordTally for a document.
   RETURNS: WordTally for a document
   EXAMPLES: wordCount [["a", "rose", "is", "a", "rose"],["but", "so", "is", "a", "rose"]] == [("a",3),("but",1),("is",2),("rose",3),("so",1)]
-}

wordCount :: Document -> WordTally
wordCount doc = let lst = sort (concat doc)
                in wordCountAux 1 lst



-- Funktion 2

sentenceToPairs :: Sentence -> Pairs
sentenceToPairs [] = []
sentenceToPairs [x] = []
--VARIANT 
sentenceToPairs (x:y:xs) = [(x, y)] ++ sentenceToPairs (y:xs)

adjacentPairs :: Document -> Pairs
adjacentPairs doc 
  | null (tail doc) = sentenceToPairs (head doc)
--VARIANT 
  | otherwise = sentenceToPairs (head doc) ++ adjacentPairs (tail doc)
                  


{- 

   PRE: 
   RETURNS: 
   EXAMPLES:
-}
initialPairs :: Document -> Pairs
initialPairs = undefined  -- remove "undefined" and write your function here

{- 

   PRE: 
   RETURNS: 
   EXAMPLES:
-}
finalPairs :: Document -> Pairs
finalPairs = undefined  -- remove "undefined" and write your function here


{- 

   PRE: 
   RETURNS: 
   EXAMPLES:
-}
pairsCount :: Pairs -> PairsTally
pairsCount = undefined  -- remove "undefined" and write your function here


{- 

   PRE: 
   RETURNS: 
   EXAMPLES:
-}
neighbours :: PairsTally -> String -> WordTally
neighbours = undefined  -- remove "undefined" and write your function here


{- 

   PRE: 
   RETURNS: 
   EXAMPLES:
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour = undefined  -- remove "undefined" and write your function here



-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])
test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a",2) (wordCount [["a","b"],["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"],["bar"]]) 

test3a = TestCase $ assertEqual "initialPairs" [("a","b")] (initialPairs [["a","b","a"],["c"]])
                      
test3b = TestCase $ assertEqual "finalPairs" [("b","a")] (finalPairs [["a","b","a"],["c"]])
                      

-- pairsCount
test4 = TestCase $ assertBool "pairsCount simple" 
            (elem (("a","b"), 2) (pairsCount [("a","b"),("c","d"),("a","b")]))
test5 = TestCase $ assertBool "pairsCount tricky" 
             (let x = pairsCount (adjacentPairs [["a","b","a"],["c"]]) in 
                      elem (("a","b"), 2) x || elem (("b","a"), 2) x)

-- neighbours
test6 = TestCase $ assertEqual "neighbours left" [("b",2)] 
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "a") 

test7 = TestCase $ assertEqual "neighbours left" [("a",2)]
                                                 (neighbours [(("a","b"),2),(("c","d"),1)] "b") 

-- mostCommonNeighbour
test8 = TestCase $ assertEqual "mostCommonNeighbour text \"the\"" (Just "fun") 
                                                                  (mostCommonNeighbour input "the") 
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

test9 = TestCase $ assertEqual "mostCommonNeighbour text \"spam\"" 
                      Nothing (mostCommonNeighbour input "spam")
  where input = [(("the", "fun"),4),(("the","foot"),3),(("dog","power"),2)]

-- testing the PandP.austin text
test10 = TestCase $ assertEqual "mostCommonNeighbour of \"bennet\"" 
            (Just "mr") (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet") 

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7,test8,test9,test10]




