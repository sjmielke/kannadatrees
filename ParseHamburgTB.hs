module ParseHamburgTB where

import Control.DeepSeq (($!!))
import Data.List (find, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import System.Directory (getDirectoryContents)
import System.IO.Unsafe (unsafeInterleaveIO)

import CoNLLOutput

main = do
    files <- fmap (take 3000 . drop 2) $ getDirectoryContents hamburgTBPath -- skip "." and ".."
    -- ^ This file list is literally the only thing that is continuously
    -- occupying memory during the program.
    coNLLTB <- sequence_lazy $ map (fmap ((,) Nothing) . fileCruncher) files
    let hamburgOpts = stdCoNLLExportOptions{ getOutputPrefix = "../data/Hamburg/sentences/"
                                           , getNoOfSentences = Just $ length files
                                           , getRootRelation = "S"
                                           }
    generateTrainAndTestFiles hamburgOpts coNLLTB
      where
        hamburgTBPath = "../data/datasources/hamburg_dependency_treebank/part_A/"
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
              
              form = unquote $ (unwords $ drop 2 $ words word)
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
                                               Just v -> acc ++ "|"
                                                             ++ k ++ "="
                                                             ++ v
                                  ) ""
                          $ ["case", "degree", "gender", "number", "flexion", "person", "tense"]
               , getHead = read $ headinfo !! 4
               , getDepRel = unquote $ headinfo !! 2
               , getPHead = -1
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
