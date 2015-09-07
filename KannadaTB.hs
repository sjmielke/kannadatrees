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
-- helpers
, findIdForAddress
) where

import Data.Char (isLatin1)
import Data.Hashable
import Data.List
import Data.Maybe (fromJust)
import Control.Exception.Base (assert)

import CoNLLOutput

-- | These data structures contain the information encoded in the input file
-- (including the chunking and feature-set pseudo-tag)
-- The only new synthesized annotations here are the IDs of words
-- (which are now global per sentence instead of relative per chunk).
type KannadaTreebank = [(String, KannadaSentence)] -- ^ a sentence and its (string) id
type KannadaSentence = [KannadaChunk]
data KannadaChunk = KannadaChunk -- ASSUMPTION: no nested chunking
    { getChunkTag :: String
    , getChunkFS :: ChunkFeatureSet -- implied with no attributes, if non-existent
    , getWords :: [KannadaWord]
    }
    deriving (Eq, Show)
data KannadaWord = KannadaWord
    { getWordId :: Int -- global inside the sentence, computed by parser
    , getWord :: String
    , getFullTag :: String
    , getWordFS :: WordFeatureSet -- implied with no attributes, if non-existent
    }
    deriving (Eq, Show)
data ChunkFeatureSet = ChunkFeatureSet
    { getAddress :: Maybe String -- originally called name
    , getDRel :: String
    , getDRelHead :: String -- if empty, getDRel == ROOT
    } -- af is partially present, but always empty
    deriving (Eq, Show)
data WordFeatureSet = WordFeatureSet
    { getLemma :: String
    , getCoarseTag :: String
    , getAFeatures :: [String]
    } -- omit "name", redundant to word itself
    deriving (Eq, Show)

transformKannadaTBToCoNLL :: KannadaTreebank -> CoNLLTreebank
transformKannadaTBToCoNLL ss = map transformSentence ss

transformSentence :: (String, KannadaSentence) -> (Maybe String, CoNLLSentence)
transformSentence (sid, chunks)
  = (,) (Just sid)
  $ uncurry shiftAddressesBack -- since filterNull has left some "holes"
  $ filterNull []
  $ concatMap transformChunk
  $ chunks
  where
    -- Basic assumption here: all words of a chunk are siblings
    -- sharing the same dependency to one head!
    transformChunk (KannadaChunk tag chunkfs ws)
      = map (transformWord tag) ws
      where
        transformWord chunkTag
                      (KannadaWord i form fulltag
                                   (WordFeatureSet lemma coarsetag fs))
          = CoNLLWord i
                      (enrichNULL form)
                      (enrichNULL lemma)
                      coarsetag
                      fulltag
                      (intercalate "|" $ map (uncurry (++))
                                       $ filter (not . null . snd)
                                       $ zip (zipWith (:) ['a'..] $ repeat "=") fs) -- TODO: choose proper names
                      (fromJust . findIdForAddress chunks $ getDRelHead chunkfs)
                      (getDRel chunkfs)
                      ""
                      ""
            where
              enrichNULL "NULL" = chunkTag
              enrichNULL s = s
    
    filterNull
      :: [Int] -- ^ word ids of NULL in old list
      -> [CoNLLWord] -- ^ old word list
      -> ([Int], [CoNLLWord]) -- ^ clean word list
    filterNull nulls [] = (nulls, [])
    filterNull nulls (cword@CoNLLWord{getId = oldid} : ws)
      | False && getForm cword == "NULL" = filterNull (oldid : nulls) ws
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

-- If a chunk is the head of a dependency
-- the *leftmost* word will play that role.
findIdForAddress :: [KannadaChunk] -> String -> Maybe Int
findIdForAddress chunks "" = Just 0
-- Caution: instead of pointing to a "NULL" node, we point to the ROOT instead.
findIdForAddress chunks a = do
  targetChunk <- find ((== (Just True)). fmap (== a) . getAddress . getChunkFS)
               $ chunks
  return $ getWordId
         $ head {- <- leftmost -}
         $ getWords
         $ targetChunk
