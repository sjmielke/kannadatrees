module ParseKannadaTB where

import Text.HTML.TagSoup
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.State.Lazy

import Debug.Trace
import Control.Exception.Base (assert)

import KannadaTB
import CoNLLOutput

getKannadaTB :: IO KannadaTreebank
getKannadaTB = do
    --let treebankPath = "treebank_small.xml"
    let treebankPath = "../data/TREE 1-4150 (3.8 (1).15 CORRECTED)"
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
    
    
    let clusterfuckFreeSentencesTags = takeWhile (\(i,_) -> i /= "513")
                                     -- This first filter is more restrictive, since right now I don't want even light clusterfucks.
                                     $ filter (not . (`elem` ["6", "23", "183", "186", "263", "266", "366", "413", "503"]) . fst)
                                     -- All A-sentences Are Bastards.
                                     $ filter ((/='A') . last . fst)
                                     -- These contain shit beyond all hope. What the actual fuck.
                                     $ filter (not . (`elem` ["423", "427", "457"]) . fst)
                                     $ allSentencesTags
    
    -- print $ length clusterfuckFreeSentencesTags
    
    -- The first 504 sentences that don't fail basic assertions. Let's just work with them for now.
    let fineSentenceParses = map parseSentence $ clusterfuckFreeSentencesTags
    
    return fineSentenceParses

-- Possible future assertions:
-- * only one empty drel per sentence
-- * drels have to be valid
-- * what the hell is troot and mtype?

parseSentence
  :: (String, [Tag String]) -- ^ (id of sentence, tags)
  -> KannadaSentence
parseSentence (i, tags) = id -- trace ("\n\n--< " ++ show i ++ " >--\n\n")
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
              ws -> error {-trace-} ("Failing at a line with " ++ show (length ws) ++ " words: " ++ unwords ws)
              --    $ return (junkChunk : chunks)
              --          where junkChunk = KannadaChunk "JUNK" (ChunkFeatureSet "JUNK" "") []
           chunkReader chunks' ws
        where
          makeChunk :: String -> State Int KannadaChunk
          makeChunk chunkTag
            = fsMemberCheckChunk
            $ return (KannadaChunk chunkTag fs [])
              where
                address = fromJust $ lookup "name" attrs
                [drelname, drelhead] = splitOn ":"
                                     $ fromMaybe "ROOT:" (lookup "drel" attrs)
                fs = ChunkFeatureSet address drelname drelhead
          updateChunk :: KannadaChunk -> String -> String -> State Int KannadaChunk
          updateChunk chunk word fullTag
            = fsMemberCheckWord
            $ do id <- get
                 put (id + 1)
                 return $ chunk{getWords = words' id}
              where
                words' i = (KannadaWord i word fullTag fs) : getWords chunk
                fs = fsFrom
                   $ splitOn ","
                   $ lookup' "af" attrs
                fsFrom (l:c:f) = WordFeatureSet l c f
          
          -- Sanity Checks
          fsMemberCheckChunk = flip (foldr ($))
            [ assert $ attrContains "name" -- CAUTION: this does not check if these links are valid!
            , assert $ attrEqualsIfExists "af" ",,,,,,,"
            , assert $ attrAtMost ["name", "af", "drel"]
            ]
          fsMemberCheckWord = flip (foldr ($))
            [ assert $ attrContains "af" -- There are some who don't contain any. Infer it or what? TODO.
            , assert $ attrAtMost ["name", "af", "troot", "mtype"]
            , assert $ attrOnlyNullMay "troot"
            , assert $ attrOnlyNullMay "mtype"
            ] -- name should be there, too, but we don't need it
          attrContains key = not $ lookup key attrs == Nothing
          attrEqualsIfExists key val = case lookup key attrs of
            Nothing -> True
            Just v -> v == val
          attrAtMost keys = all (\a -> (fst a) `elem` keys) attrs
          attrOnlyNullMay key = case lookup key attrs of
                                  Just v -> id -- trace (key ++ ": " ++ v)
                                          $ (take 1 $ drop 1 $ words text) == ["NULL"]
                                  Nothing -> True
    chunkReader chunks [TagText "\n\t))\n"] -- end of final chunk
      = return chunks
    chunkReader _ ts = error $ "Unexpected XML Tag " ++ show (take 2 ts)
    
    lookup' key assocs
      = case lookup key assocs of
          Just val -> val
          Nothing -> error ("Could not find key " ++ key ++ " in a <fs> tag!")

main = do
    parsedSentences <- getKannadaTB
    let coNLLTB = transformKannadaTBToCoNLL parsedSentences
        splitPoint = (*19) $ length coNLLTB `div` 20
    
    -- writeCoNLLTreebankTo "../data/kannada_train.conll" coNLLTB
    -- writeCoNLLTreebankTo "../data/kannada_test.conll" coNLLTB
    writeCoNLLTreebankTo "../data/kannada_train.conll" $ take splitPoint coNLLTB
    writeCoNLLTreebankTo "../data/kannada_test.conll" $ drop splitPoint coNLLTB
