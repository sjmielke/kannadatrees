module ParseHamburgTB where

import Control.DeepSeq (($!!))
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import System.Directory (getDirectoryContents)
import System.IO.Unsafe (unsafeInterleaveIO)

import CoNLLOutput

main = do
    files <- fmap (take 3000 . drop 2) $ getDirectoryContents hamburgTBPath -- skip "." and ".."
    -- ^ This file list is literally the only thing that is continuously
    -- occupying memory during the program.
    coNLLTB <- sequence_lazy $ map fileCruncher files
    generateTrainAndTestFiles (Just $ length files) "../data/Hamburg" coNLLTB
      where
        hamburgTBPath = "../data/hamburg_dependency_treebank/part_A/"
        fileCruncher name = do 
            contents <- readFile (hamburgTBPath ++ name)
            return $! parseSentence $!! contents

parseSentence :: String -> CoNLLSentence
parseSentence
  = map convert
  . splitOn [","]
  . tail -- kick head containing sentence id and stuff
  . lines
    where
      convert (word : attrs)
        = let splitattrs = map (splitOn " / ") attrs
              attrassocs = map (\[k, v] -> (unquote k, unquote v))
                         $ filter ((== 2) . length) splitattrs
              headinfo = words
                       $ fromJust
                       $ find ((== "'SYN' -> ") . take 9) attrs
              
              form = unquote $ (words word) !! 2
              postag = fromJust $ lookup "cat" attrassocs
          in CoNLLWord
               { getId = read $ (words word) !! 1
               , getForm = form
               , getLemma = fromMaybe form $ lookup "base" attrassocs
               , getCPosTag = postag
               , getPosTag = postag
               , getFeats = drop 1
                          $ foldl (\acc k -> case lookup k attrassocs of
                                               Nothing -> acc
                                               Just v -> acc ++ "|" ++ v
                                  ) ""
                          $ ["case", "degree", "gender", "number", "flexion", "person", "tense"]
               , getHead = read $ headinfo !! 4
               , getDepRel = unquote $ headinfo !! 2
               , getPHead = ""
               , getPDepRel = ""
               }
      unquote = tail . init

-- taken from http://stackoverflow.com/questions/12674709/can-i-read-n-files-lazily-as-a-single-io-operation-in-haskell
sequence_lazy :: [IO a] -> IO [a]
sequence_lazy [] = return []
sequence_lazy (x:xs) = do
    r <- x
    rs <- unsafeInterleaveIO (sequence_lazy xs)
    return (r:rs)
