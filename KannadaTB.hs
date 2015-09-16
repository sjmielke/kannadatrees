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
transformKannadaTBToCoNLL ss = map coNLLifySentence ss

{-
-- TODO for this: renormalize word indices
glueChunksIn :: (String, KannadaSentence) -> (String, KannadaSentence)
glueChunksIn (sid, chunks) = (sid, map glue chunks)
  where
    glue c@(KannadaChunk tag fs (w:ws)) = c{getWords = [w{getWord = intercalate "_" $ map getWord ws}]}
-}

coNLLifySentence :: (String, KannadaSentence) -> (Maybe String, CoNLLSentence)
coNLLifySentence (sid, chunks)
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
                                       $ zip afKeys fs)
                      (fromJust . findIdForAddress chunks $ getDRelHead chunkfs)
                      (getDRel chunkfs)
                      (-1)
                      ""
            where
              afNames = ["root", "category", "gender", "number", "pers", "case"]
              afKeys = map (++"=") afNames
              -- This might be deleted, as it should no longer have any effect:
              -- (actually it makes checking for NULL in filterNull more
              -- difficult, since now nodes no longer have exactly "NULL" as form!)
              enrichNULL "NULL" = chunkTag
              enrichNULL s = s
    
    -- | Already adjusts the ids of words, but not the DepHeads
    -- (that is what shiftAdressesBack is for)
    filterNull
      :: [Int] -- ^ word ids of NULL in old list
      -> [CoNLLWord] -- ^ old word list
      -> ([Int], [CoNLLWord]) -- ^ clean word list
    filterNull nulls [] = (nulls, [])
    filterNull nulls (cword@CoNLLWord{getId = oldid} : ws)
      | take 4 (getForm cword) == "NULL" = filterNull (oldid : nulls) ws
      | otherwise = let (indices, cleanRest) = filterNull nulls ws
                    in ( indices
                       , cword{getId = oldid - length nulls} : cleanRest
                       )
    
    -- | This function updates only the DepHeads of words
    -- after the ids have already been shifted in `filterNull`.
    shiftAddressesBack
      :: [Int] -- ^ word ids of NULL in old list
      -> [CoNLLWord] -- ^ word list with new indices but old drel addresses
      -> [CoNLLWord] -- ^ finally clean wordlist
    shiftAddressesBack nulls = map correct
      where
        correct cword@CoNLLWord{getHead = oldhead}
          = cword{getHead = oldhead - offsetAt oldhead}
        offsetAt somehead = length (dropWhile (>somehead) nulls)

-- If a chunk is the head of a dependency
-- the *leftmost* word will play that role.
findIdForAddress :: [KannadaChunk] -> String -> Maybe Int
-- Empty head -> point to ROOT:
findIdForAddress chunks "" = Just 0
findIdForAddress chunks a = do
  targetChunk <- find ((== (Just True)). fmap (== a) . getAddress . getChunkFS)
               $ chunks
  case take 4 a of
    -- Head is a null node? Use its head instead!
    "NULL" -> findIdForAddress chunks $ getDRelHead . getChunkFS
                                      $ targetChunk
    _ -> return $ getWordId
                $ head {- <- leftmost -}
                $ getWords
                $ targetChunk
