module CoNLLOutput
( generateTrainAndTestFiles
, CoNLLExportOptions (..)
, stdCoNLLExportOptions
, CoNLLTreebank
, CoNLLSentence
, CoNLLWord (..)
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafeInterleaveIO)

data CoNLLExportOptions = CoNLLExportOptions
    { getOutputPrefix :: FilePath -- ^ output prefix (full path, no extension)
    , getNoOfSentences :: Maybe Int -- ^ number of sentences (to control memory!)
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
            showPos x = if x < 0 then "0" else show x -- TODO: Why am I forced to give a valid head? This. doesn't make any sense.
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
    let l = case getNoOfSentences opts of
              Nothing -> length coNLLTB
              Just l' -> l'
        splitPoint = 9 * (l `div` 10)
        stringifyClean False = stringifyCoNLLTreebank opts False
        stringifyClean True  = stringifyCoNLLTreebank opts True
                             . map (\(i, ws) -> (i, map (cleanUnwantedFields opts) ws))
        wTrain, wTest, wGold :: CoNLLTreebank -> IO ()
        wTrain = writeFile (getOutputPrefix opts ++ "_train.conll")
               . stringifyClean False
        wTest  = writeFile (getOutputPrefix opts ++ "_test.conll")
               . stringifyClean True
        wGold  = writeFile (getOutputPrefix opts ++ "_gold.conll")
               . stringifyClean False
    
    firstPart <- performFunctionAfterServing splitPoint
                                             (\ss -> wTest ss >> wGold ss)
                                             coNLLTB
    wTrain firstPart

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' 1 (x:xs) = ([x], xs)
splitAt' i (x:xs) = let (xs', xs'') = splitAt' (i - 1) xs
                    in (x:xs', xs'')

-- | The problem is that I want to IO-crunch the first half of a list and then
-- crunch the second half with another function. Using splitAt seems to retain
-- some references to the very beginning of the list whily crunching it.
-- This is not necessary and causes huge memory consumption.
-- I'm not sure this convoluted unsafeInterleaveIO-powered solution is
-- necessary, but at least it works: the full list is being returned lazily and
-- once the splitpoint is reached the lazy computation instead feeds the whole
-- rest of the list to the given f2. It is still up to the user to feed the list
-- (the first half) that this function is essentially serving to f1, see above.
performFunctionAfterServing
  :: Int -- ^ splitpoint
  -> ([a] -> IO ()) -- ^ f2
  -> [a] -- ^ all data
  -> IO [a] -- ^ rest for f1
performFunctionAfterServing 0 f2 xs = f2 xs >> return []
performFunctionAfterServing i f2 (x:xs) = do
    xs' <- unsafeInterleaveIO $ performFunctionAfterServing (i-1) f2 xs
    return (x:xs')
