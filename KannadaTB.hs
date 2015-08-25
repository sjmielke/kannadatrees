module KannadaTB
( -- data structures
  KannadaTreebank
, KannadaSentence
, KannadaChunk (..)
, KannadaWord (..)
, ChunkFeatureSet (..)
, WordFeatureSet (..)
-- transformations
, transformKannadaTBToCoNLL
) where

import Data.List
import Data.Maybe (fromJust)
import CoNLLOutput

-- | These data structures contain the information encoded in the input file
-- (including the chunking and feature-set pseudo-tag)
-- The only new synthesized annotations here are the IDs of words
-- (which are now global per sentence instead of relative per chunk).
type KannadaTreebank = [KannadaSentence]
type KannadaSentence = [KannadaChunk]
data KannadaChunk = KannadaChunk
    { getChunkTag :: String
    , getChunkFS :: ChunkFeatureSet
    , getWords :: [KannadaWord]
    }
    deriving (Eq, Show)
data KannadaWord = KannadaWord
    { getWordId :: Int -- global inside the sentence
    , getWord :: String
    , getFullTag :: String
    , getWordFS :: WordFeatureSet
    }
    deriving (Eq, Show)
data ChunkFeatureSet = ChunkFeatureSet
    { getAddress :: String -- originally called name
    , getDRel :: String
    , getDRelHead :: String -- if empty, getDRel == ROOT
    } -- af is partially present, but always empty
    deriving (Eq, Show)
data WordFeatureSet = WordFeatureSet
    { getLemma :: String
    , getCoarseTag :: String
    , getFeatures :: [String]
    } -- omit "name", redundant to word itself
    deriving (Eq, Show)

transformKannadaTBToCoNLL :: KannadaTreebank -> CoNLLTreebank
transformKannadaTBToCoNLL ss = map transformSentence ss

transformSentence :: KannadaSentence -> CoNLLSentence
transformSentence chunks
  = uncurry shiftAddressesBack -- since filterNull has left some "holes"
  $ filterNull []
  $ concatMap transformChunk chunks
  where
    -- Basic assumption here: all words of a chunk are siblings
    -- sharing the same dependency to one head!
    transformChunk (KannadaChunk tag chunkfs ws) = map transformWord ws
      where
        transformWord (KannadaWord i w fulltag (WordFeatureSet lemma coarsetag fs))
          = CoNLLWord i w lemma coarsetag fulltag
                      (intercalate "|" $ filter (not . null) fs)
                      (findIdForAddress $ getDRelHead chunkfs)
                      (getDRel chunkfs)
                      "" ""
    
    -- If one the other side a chunk is the head of a dependency
    -- the *leftmost* word will play that role.
    findIdForAddress :: String -> Int
    findIdForAddress "" = 0
    -- Caution: instead of pointing to a "NULL" node, we point to the ROOT instead.
    findIdForAddress a =
      if take 4 (getChunkTag targetChunk) == "NULL" -- just starts with "NULL"
        then 0
        else getWordId $ head {- <- leftmost -} $ getWords $ targetChunk
      where targetChunk = fromJust
                        $ find ((== a) . getAddress . getChunkFS)
                        $ chunks
    
    filterNull
      :: [Int] -- ^ word ids of NULL in old list
      -> [CoNLLWord] -- ^ old word list
      -> ([Int], [CoNLLWord]) -- ^ clean word list
    filterNull nulls [] = (nulls, [])
    filterNull nulls (cword@CoNLLWord{getId = oldid} : ws)
      | getForm cword == "NULL" = filterNull (oldid : nulls) ws
      | otherwise = let (indices, cleanRest) = filterNull nulls ws
                    in ( indices
                       , cword{getId = oldid - length nulls} : cleanRest
                       )
    
    shiftAddressesBack
      :: [Int] -- ^ word ids of NULL in old list
      -> [CoNLLWord] -- ^ word list with new indices but old drel addresses
      -> [CoNLLWord] -- ^ finally clean wordlist
    shiftAddressesBack nulls = map correct
      where
        correct cword@CoNLLWord{getHead = oldhead}
          = cword{getHead = oldhead - length (dropWhile (>oldhead) nulls)}
