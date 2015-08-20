import Text.HTML.TagSoup
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.State.Lazy

import Debug.Trace
import Control.Exception.Base (assert)

-- | These data structures contain the information encoded in the input file
-- (including the chunking and feature-set pseudo-tag)
-- The only new synthesized annotations here are the IDs of words
-- (which are now global per sentence instead of relative per chunk).
type KannadaSentence = [KannadaChunk]
data KannadaChunk = KannadaChunk
    { getChunkTag::String
    , getChunkFS::ChunkFeatureSet
    , getWords::[KannadaWord]
    }
    deriving (Eq, Show)
data KannadaWord = KannadaWord
    { getId::Int
    , getWord::String
    , getFullTag::String
    , getWordFS::WordFeatureSet
    }
    deriving (Eq, Show)
data ChunkFeatureSet = ChunkFeatureSet
    { getAddress::String -- originally called name
    , getDRel::String
    } -- af is partially present, but always empty
    deriving (Eq, Show)
data WordFeatureSet = WordFeatureSet
    { getLemma::String
    , getCoarseTag::String
    , removeme::String
    , getFeatures::[String]
    } -- omit "name", redundant to word itself
    deriving (Eq, Show)

main = do
    --let treebankPath = "treebank_small.xml"
    let treebankPath = "TREE 1-4150 (3.8 (1).15 CORRECTED)"
    dirtyTreebankFile <- readFile treebankPath
    
    -- This whole file is a dirty desaster on pretty much every level.
    -- Some cleaning is necessary before it touches any meaningful code.
    let replace old new = intercalate new . splitOn old
    let cleanTreebankFile = replace "af=''" "af='`"
                          $ replace "troot=''''" "troot=\"\""
                          $ replace "name='''" "name='`'"
                          $ replace "<fs\t<fs" "\t<fs"
                          $ replace "' ' name='" "' name='"
                          $ replace ",0'.' name='" ",0`.' name='"
                          $ replace "BLK\t<fs af=',,,,,,,'>" "BLK\t<fs af=',,,,,,,' name=''>"
                          $ replace "\n1.1\tಕೇಂದ್ರೀಕೃತವಾದ JJ<fs\taf='ಕೇಂದ್ರೀಕೃತ,adj,,,,o,ಆದ,Axa'\t\n" "\n1.1\tಕೇಂದ್ರೀಕೃತವಾದ JJ<fs\taf='ಕೇಂದ್ರೀಕೃತ,adj,,,,o,ಆದ,Axa'>\t\n"
                          $ dirtyTreebankFile
    
    let originalTaglist = parseTags cleanTreebankFile
        cleanTaglist = filter (\t -> not $ isTagText t
                                  && null (words $ fromTagText t))
                              originalTaglist
    let allSentencesTags = map ( \ ((TagOpen "Sentence" [("id", i)]) : content)
                                 -> (i, content) )
                         $ map (dropWhile (~/= TagOpen "Sentence" []))
                         $ init
                         $ splitWhen (~== TagClose "Sentence")
                         $ cleanTaglist
    
    -- These contain shit beyond all hope. What the actual fuck.
    let clusterfuckFreeSentencesTags = filter (not . (`elem` ["423", "427", "457"]) . fst) allSentencesTags
    
    print $ foldl' (+) 0 $ map (length . show . parseSentence) clusterfuckFreeSentencesTags

-- Possible future assertions:
-- * only one empty drel per sentence
-- * what the hell is troot and mtype?
-- * is gender really gender?

parseSentence
  :: (String, [Tag String]) -- ^ (id of sentence, tags)
  -> KannadaSentence
parseSentence (id, tags) = trace ("--< " ++ show id ++ " >--")
                         $ reverse
                         $ map (\c -> c{getWords = reverse $ getWords c})
                         $ evalState (chunkReader [] tags) 1
  where
    chunkReader :: [KannadaChunk] -> [Tag String] -> State Int [KannadaChunk]
    chunkReader chunks (TagText text : TagOpen "fs" attrs : ws)
      = do chunks' <- case words text of
              [      _, "((", chunkTag] -- open first chunk
                -> makeChunk chunkTag
                    >>= return . (:chunks)
              ["))", _, "((", chunkTag] -- open next chunk
                -> makeChunk chunkTag
                    >>= return . (:chunks)
              [_, word, fullTag] -- insert word into chunk
                -> updateChunk (head chunks) word fullTag
                    >>= return . (:(tail chunks))
              -- Seriously, what the hell. This is usually because some chunks don't have a fs tag. TODO.
              ws -> trace ("Failing at a line with " ++ show (length ws) ++ " words: " ++ unwords ws) $ return (junkChunk : chunks)
                        where junkChunk = KannadaChunk "JUNK" (ChunkFeatureSet "JUNK" "") []
           chunkReader chunks' ws
        where
          makeChunk :: String -> State Int KannadaChunk
          makeChunk chunkTag
            = fsMemberCheckChunk
            $ return (KannadaChunk chunkTag fs [])
              where fs = ChunkFeatureSet (fromJust $ lookup "name" attrs)
                                         (fromMaybe "" (lookup "drel" attrs))
          updateChunk :: KannadaChunk -> String -> String -> State Int KannadaChunk
          updateChunk chunk word fullTag
            = trace "word" $ fsMemberCheckWord
            $ do id <- get
                 put (id + 1)
                 return $ chunk{getWords = words' id}
              where
                words' i = (KannadaWord i word fullTag fs) : getWords chunk
                fs = fsFrom
                   $ splitOn ","
                   $ lookup' "af" attrs
                fsFrom (l:c:r:f) = if (not $ null r) then trace ("gender: " ++ r) $ WordFeatureSet l c r f
                                       else WordFeatureSet l c r f
          
          -- Sanity Checks
          fsMemberCheckChunk = flip (foldr ($))
            [ assert $ attrContains "name"
            , assert $ attrEqualsIfExists "af" ",,,,,,,"
            , assert $ attrAtMost ["name", "af", "drel"]
            ]
          fsMemberCheckWord = flip (foldr ($))
            [ assert $ attrContains "af" -- There are some who don't contain any. Infer it or what? TODO.
            , assert $ attrAtMost ["name", "af", "troot", "mtype"]
            , assert $ attrOnlyNullMay "troot"
            , assert $ attrOnlyNullMay "mtype"
            ] -- name should be there, too, but we don't need it
          attrContains key = if not $ lookup key attrs == Nothing then True else trace "ooooooooooooooooooooooooooooh" (key == "af")
          attrEqualsIfExists key val = case lookup key attrs of
            Nothing -> True
            Just v -> v == val
          attrAtMost keys = traceShow attrs $ all (\a -> (fst a) `elem` keys) attrs
          attrOnlyNullMay key = case lookup key attrs of
                                  Just v -> trace (key ++ ": " ++ v) $ (take 1 $ drop 1 $ words text) == ["NULL"]
                                  Nothing -> True
    chunkReader chunks [TagText "\n\t))\n"] -- end of final chunk
      = return chunks
    chunkReader _ ts = error $ "Unexpected XML Tag " ++ show (take 2 ts)
    
    lookup' key assocs
      = case lookup key assocs of
          Just val -> val
          Nothing -> trace ("Could not find key " ++ key ++ " in a <fs> tag!") ",,,,,,,"
