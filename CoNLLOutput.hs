module CoNLLOutput
( writeCoNLLTreebankTo
, CoNLLTreebank
, CoNLLSentence
, CoNLLWord (..)
) where

import Data.List
import Data.Hashable

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

-- | Generates one TSV-line.
stringifyCoNLLWord :: CoNLLWord -> String
stringifyCoNLLWord (CoNLLWord i fr l c p fe h d ph pd)
  = intercalate "\t"
  $ map encodeEmpty
  $ [show i, glyphs fr, glyphs l, c, p, fe, show h, d, ph, pd]
  where
    encodeEmpty "" = "_"
    encodeEmpty s = s
    -- This is what we would do in a perfect world.
    glyphs = id
    -- This is what we might do because in 2015 there are still people
    -- who cannot deal with strange unicode letters.
    -- glyphs = init . tail . show
    -- This is most readable in the tree viewer, but doesn't make any sense for parsing.
    -- glyphs _ = 'W' : show i
    -- So highly ambiguous hashes it is.
    -- glyphs w = show $ hash w `mod` 1000000 -- I would like to have them short and readable.

stringifyCoNLLSentence :: CoNLLSentence -> String
stringifyCoNLLSentence ws = unlines $ map stringifyCoNLLWord ws

stringifyCoNLLTreebank :: CoNLLTreebank -> String
stringifyCoNLLTreebank ss = unlines $ map stringifyCoNLLSentence ss

writeCoNLLTreebankTo :: FilePath -> CoNLLTreebank -> IO ()
writeCoNLLTreebankTo p ss = writeFile p $ stringifyCoNLLTreebank ss
