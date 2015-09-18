module CoNLLOutput
( generateTrainAndTestFiles
, CoNLLExportOptions (..)
, stdCoNLLExportOptions
, CoNLLTreebank
, CoNLLSentence
, CoNLLWord (..)
) where

import Control.Exception.Base (assert)
import Control.Monad (forM, forM_)
import Data.Array.IO
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random

import Debug.Trace

data CoNLLExportOptions = CoNLLExportOptions
    { getOutputPrefix :: FilePath -- ^ output prefix (full path, no extension)
    , getNoOfSentences :: Maybe Int -- ^ number of sentences (to control memory! - doesn't happen anymore, but we still use this)
    , getRootRelation :: String -- ^ name of the root relation
    , getVisibleFeats :: Bool -- ^ visiblity of lemma, coarse and full postag and features in train and test sets
    , getVisibleDep :: Bool -- ^ visiblity of dependency information in train and test sets
    }

stdCoNLLExportOptions = CoNLLExportOptions
    { getOutputPrefix = error "Please specify export filename!"
    , getNoOfSentences = Nothing
    , getRootRelation = "ROOT"
    , getVisibleFeats = True
    , getVisibleDep = False
    }

type CoNLLTreebank = [(Maybe String, CoNLLSentence)] -- ^ sentence with possible (string) id
type CoNLLSentence = [CoNLLWord]
data CoNLLWord = CoNLLWord
    { getId :: Int -- starting with 1
    , getForm :: String
    , getLemma :: String
    , getCPosTag :: String
    , getPosTag :: String
    , getFeats :: String
    , getHead :: Int -- 0 means no head
    , getDepRel :: String
    , getPHead :: Int
    , getPDepRel :: String
    }
    deriving (Show)

stringifyCoNLLTreebank
  :: CoNLLExportOptions
  -> Bool -- ^ training (cleaning) mode 
  -> CoNLLTreebank
  -> String
stringifyCoNLLTreebank opts clean ss = unlines $ map stringifyCoNLLSentence ss
  where
    stringifyCoNLLSentence :: (Maybe String, CoNLLSentence) -> String
    stringifyCoNLLSentence (msid, ws) = unlines $ map stringifyCoNLLWord ws
      where
        -- | Generates one TSV-line.
        stringifyCoNLLWord :: CoNLLWord -> String
        stringifyCoNLLWord w@(CoNLLWord i fr l c p fe h d ph pd)
          = intercalate "\t"
          $ map encodeEmpty
          $ [ show i
            , nospaces fr
            , nospaces l
            , c
            , p
            , fe
            , showPos h
            , rootify d
            , showPos ph
            , pd
            ]
          where
            showPos x = if x < 0 then "0" else show x -- we're forced to give a valid head
            encodeEmpty "" = "_"
            encodeEmpty s = s
            rootify ""
             | clean && not (getVisibleDep opts) = ""
             | h == 0 = getRootRelation opts
             | otherwise = error $ sentenceInfo
                                   ++ "Empty non-root deprel in word: "
                                   ++ show w
            rootify s = s
            -- | We have to do this because MaltOptimizer fails on tokens with spaces.
            -- All it would take is 6 more characters in one line of sourcecode. Sad.
            nospaces = intercalate "_" . splitOn " "
            sentenceInfo = case msid of
                             Nothing -> ""
                             Just sid -> "Sentence " ++ sid ++ ": "

cleanUnwantedFields
  :: CoNLLExportOptions
  -> CoNLLWord
  -> CoNLLWord
cleanUnwantedFields opts w
  = let noFeats = if getVisibleFeats opts
                    then w
                    else w{getLemma = "", getCPosTag = "", getPosTag = "", getFeats = ""}
        noDep = if getVisibleDep opts
                  then noFeats
                  else noFeats{getHead = -1, getDepRel = "", getPHead = -1, getPDepRel = ""}
    in noDep

generateTrainAndTestFiles
  :: CoNLLExportOptions
  -> CoNLLTreebank
  -> IO ()
generateTrainAndTestFiles opts coNLLTB = do
    let nfolds = 5 -- also hardcoded in eval.sh
    let l = case getNoOfSentences opts of
              Nothing -> length coNLLTB
              Just l' -> l'
    
    -- The data in the treebank is of course not really uniformly distributed.
    -- let rndCoNLLTB = coNLLTB
    -- Also, we only take as many elements as specified (to control training set size)
    rndCoNLLTB <- fmap (take l) $ shuffle coNLLTB
    
    let wFile name = writeFile (getOutputPrefix opts ++ name ++ ".conll")
                   . stringifyCoNLLTreebank opts False
    
    wFile "full" rndCoNLLTB
    
    forM_ [1..nfolds] $ \part -> do
        let (testset, trainset) = cutOutPart part nfolds rndCoNLLTB l
            wF' n = wFile (n ++ show part)
        wF' "test" testset
        wF' "gold" testset
        wF' "train" trainset

cutOutPart
  :: Int -- ^ which part
  -> Int -- ^ of how many
  -> [a] -- ^ input list
  -> Int -- ^ its length
  -> ([a], [a]) -- ^ cutout, remains
cutOutPart part total xs l
  = let firstCut = (part - 1) * l `div` total
        secondCut = l `div` total
        (remains1, tmp) = splitAt firstCut xs
        (cutout, remains2) = splitAt secondCut tmp
    in (cutout, remains1 ++ remains2)

-- | Randomly shuffle a list
--   /O(N)/
-- https://wiki.haskell.org/Random_shuffle
-- made deterministic with setStdGen
shuffle :: [a] -> IO [a]
shuffle xs = do
    setStdGen $ mkStdGen 42
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
