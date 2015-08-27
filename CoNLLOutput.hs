module CoNLLOutput
( writeCoNLLTreebankTo
, generateTrainAndTestFiles
, CoNLLTreebank
, CoNLLSentence
, CoNLLWord (..)
) where

import Data.List (intercalate)
import System.IO.Unsafe (unsafeInterleaveIO)

type CoNLLTreebank = [CoNLLSentence]
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

-- | Generates one TSV-line.
stringifyCoNLLWord :: CoNLLWord -> String
stringifyCoNLLWord w@(CoNLLWord i fr l c p fe h d ph pd)
  = intercalate "\t"
  $ map encodeEmpty
  $ [show i, fr, l, c, p, fe, show h, rootify d, ph, pd]
  where
    encodeEmpty "" = "_"
    encodeEmpty s = s
    rootify "" = if h == 0 then "ROOT" else error ("Empty non-root deprel in word: " ++ show w)
    rootify s = s

stringifyCoNLLSentence :: CoNLLSentence -> String
stringifyCoNLLSentence ws = unlines $ map stringifyCoNLLWord ws

stringifyCoNLLTreebank :: CoNLLTreebank -> String
stringifyCoNLLTreebank ss = unlines $ map stringifyCoNLLSentence ss

writeCoNLLTreebankTo :: FilePath -> CoNLLTreebank -> IO ()
writeCoNLLTreebankTo p ss = writeFile p $ stringifyCoNLLTreebank ss

generateTrainAndTestFiles
  :: Maybe Int -- ^ number of sentences (to control memory!)
  -> FilePath -- ^ output prefix (full path, no extension)
  -> CoNLLTreebank -- ^ input data
  -> IO ()
generateTrainAndTestFiles ml path coNLLTB = do
    let l = case ml of
              Nothing -> length coNLLTB
              Just l' -> l'
        splitPoint = 19 * (l `div` 20)
    
    let f1 = writeCoNLLTreebankTo (path ++ "_train.conll")
    let f2 = writeCoNLLTreebankTo (path ++ "_test.conll")
    
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
