module CoNLLOutput
( writeCoNLLTreebankTo
, generateTrainAndTestFiles
, CoNLLTreebank
, CoNLLSentence
, CoNLLWord (..)
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafeInterleaveIO)

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
    , getPHead :: String
    , getPDepRel :: String
    }
    deriving (Show)

stringifyCoNLLTreebank :: String -> CoNLLTreebank -> String
stringifyCoNLLTreebank rootrelname ss = unlines $ map stringifyCoNLLSentence ss
  where
    stringifyCoNLLSentence :: (Maybe String, CoNLLSentence) -> String
    stringifyCoNLLSentence (msid, ws) = unlines $ map stringifyCoNLLWord ws
      where
        -- | Generates one TSV-line.
        stringifyCoNLLWord :: CoNLLWord -> String
        stringifyCoNLLWord w@(CoNLLWord i fr l c p fe h d ph pd)
          = intercalate "\t"
          $ map encodeEmpty
          $ [show i, nospaces fr, nospaces l, c, p, fe, show h, rootify d, ph, pd]
          where
            encodeEmpty "" = "_"
            encodeEmpty s = s
            rootify "" = if h == 0
                         then rootrelname
                         else error $ sentenceInfo
                                      ++ "Empty non-root deprel in word: "
                                      ++ show w
            rootify s = s
            -- | We have to do this because MaltOptimizer fails on tokens with spaces.
            -- All it would take is 6 more characters in one line of sourcecode. Sad.
            nospaces = intercalate "_" . splitOn " "
            sentenceInfo = case msid of
                             Nothing -> ""
                             Just sid -> "Sentence " ++ sid ++ ": "


writeCoNLLTreebankTo :: String -> FilePath -> CoNLLTreebank -> IO ()
writeCoNLLTreebankTo rootrelname p ss
  = writeFile p
  $ stringifyCoNLLTreebank rootrelname ss

generateTrainAndTestFiles
  :: Maybe Int -- ^ number of sentences (to control memory!)
  -> String -- ^ name of the root relation
  -> FilePath -- ^ output prefix (full path, no extension)
  -> CoNLLTreebank -- ^ input data
  -> IO ()
generateTrainAndTestFiles ml rootrelname path coNLLTB = do
    let l = case ml of
              Nothing -> length coNLLTB
              Just l' -> l'
        splitPoint = 9 * (l `div` 10)
    
    let f1 = writeCoNLLTreebankTo rootrelname (path ++ "_train.conll")
    let f2 = writeCoNLLTreebankTo rootrelname (path ++ "_test.conll")
    
    performFunctionAfterServing splitPoint f2 coNLLTB >>= f1

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
