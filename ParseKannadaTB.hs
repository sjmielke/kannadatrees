module ParseKannadaTB where

import Data.Foldable (foldl')
import Data.List (intercalate, find)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.State.Lazy

import Debug.Trace
import Control.Exception.Base (assert)

import KannadaTB
import CoNLLOutput

trim = unwords . words

getKannadaTB :: IO KannadaTreebank
getKannadaTB = do
    --let treebankPath = "treebank_small.xml"
    let treebankPath = "../data/datasources/TREE 1-4150 (3.8 (1).15 CORRECTED)"
    dirtyTreebankFile <- readFile treebankPath
    
    -- This whole file is a dirty desaster on pretty much every level.
    -- Some cleaning is necessary before it touches any meaningful code.
    let replace old new = intercalate new . splitOn old
    let cleanTreebankFile = replace "<fs\t<fs" "\t<fs"
                          $ replace "' dmrel='" "' drel='"
                          $ replace " JJ<fs\taf='" "\tJJ\t<fs af='"
                          $ replace "Axa'\t\n" "Axa'>\n"
                          $ replace "<fs af=' ಬರು,v,,sg,3" "<fs af='ಬರು,v,,sg,3"
                          $ replace "<fs af=' ಎಷ್ಟು,adj,,,,o" "<fs af='ಎಷ್ಟು,adj,,,,o"
                          $ replace "3,d,0,0\n" "3,d,0,0'>\n"
                          $ replace "\t <fs " "\t<fs "
                          $ replace "'>'>\n" "'>\n"
                          $ replace "'\n" "'>\n"
                          $ replace "> \n" ">\n"
                          $ replace ">\t\n" ">\n"
                          $ dirtyTreebankFile
    
    let allSentences :: [(String, [String])]
        allSentences = map (\(headline : content) -> (getid headline, content))
                     $ init -- drop </document>
                     $ splitOn ["</Sentence>"]
                     $ filter (not . null)
                     $ dropWhile ((/= "<Sentence ") . take 10)
                     $ lines cleanTreebankFile
          where
            getid = fromJust . lookup "id" . getAttrsForTag "Sentence" . trim
    
    let clusterfuckFreeSentences = id --takeWhile (\(i,_) -> i /= "513")
                                 -- Contain null super-chunk
                                 $ filter ((/='0') . head . head . snd)
                                 -- Empty non-root deprel
                                 $ filter (not . (`elem` ["1189"]) . fst)
                                 -- All A-sentences Are Bastards. Apparently.
                                 $ filter ((/='A') . last . fst)
                                 -- Strange big fs's in the original fs's.
                                 $ filter (not . (`elem` ["423", "457"]) . fst)
                                 $ allSentences
    
    let lookatit x ss = case x of Right s -> (s:ss); Left e -> trace e ss
    let fineSentenceParses = foldr lookatit []
                           $ map (parseSentence >=> checkSentence)
                           $ clusterfuckFreeSentences
    
    length fineSentenceParses `seq` return ()
    
    putStrLn $ "Of " ++ show (length allSentences) ++ " input sentences, "
                     ++ show (length fineSentenceParses) ++ " are usable."
    
    return fineSentenceParses

parseSentence
  :: (String, [String]) -- ^ (id of sentence, lines)
  -> Either String (String, KannadaSentence) -- ^ error message or id and sentence
parseSentence (i, alllines)
  = id --trace ("\n--< " ++ i ++ " >--")
  $ fmap ((,) i)
  $ fmap (reverse . map (\c -> c{getWords = reverse $ getWords c}))
  $ evalState (chunkReader [] alllines) 1
  where
    chunkReader :: [KannadaChunk] -> [String] -> State Int (Either String [KannadaChunk])
    chunkReader chunksSoFar [] = return $ Right chunksSoFar
    chunkReader chunksSoFar (line : remlines)
      = case splitOn "\t" line of -- could also use `words`
          _ : "))" : _
            -> chunkReader chunksSoFar remlines
          _ : "((" : chunktag : [chunkfs]
            -> let attrs = if null chunkfs
                           then []
                           else getAttrsForTag "fs" chunkfs
                   maybeaddress = lookup "name" attrs
                   [drelname, drelhead] = splitOn ":"
                                        $ fromMaybe "ROOT:"
                                        $ lookup "drel" attrs
                   -- TODO: clean drelname: consolidate rsym_eos with _/- and case
                   fs = ChunkFeatureSet maybeaddress drelname drelhead
                   newChunk = KannadaChunk (trim chunktag) fs []
               in chunkReader (newChunk : chunksSoFar) remlines
          _ : form : finetag : [wordfs]
            -> do wordid <- get
                  put (wordid + 1)
                  let (workingChunk : finishedChunks) = chunksSoFar
                      attrs = if null wordfs
                              then [("af", ",,,,,,,")]
                              else getAttrsForTag "fs" wordfs
                      af = case lookup "af" attrs of
                             Just v -> v
                             Nothing -> ",,,,,,," -- Imply standard af.
                      updatedChunk = do -- Either monad
                          fs <- case splitOn "," af of
                                  (l:c:f) -> return $ WordFeatureSet l c f
                                  _ -> sentenceError i $ "Malformed af='" ++ af ++ "'"
                          let newWords = KannadaWord wordid (trim form) (trim finetag) fs
                                       : getWords workingChunk
                          return $ workingChunk{getWords = newWords}
                  case updatedChunk of
                    Right c -> chunkReader (c : finishedChunks) remlines
                    Left e -> return $ Left e
          faultyLine
            -> return . sentenceError i
             $ "Don't know how to deal with TSV line " ++ show faultyLine

sentenceError :: String -> String -> Either String a
sentenceError i s = Left $ "Sentence " ++ i ++ ": " ++ s

-- We can't use TagSoup or something proper, because the file doesn't even care
-- about basic SGML conformity.
getAttrsForTag :: String -> String -> [(String, String)]
getAttrsForTag name tag
  = assert (take (1 + length name) tag == "<" ++ name && last tag == '>')
  $ map ((\[k, v] -> (k, unquote v)) . splitOn "=")
  $ words
  $ drop (2 + length name) . init
  $ tag
  where unquote cs
          | head cs == '\'' && last cs == '\'' = tail $ init cs
          | head cs == '"' && last cs == '"' = tail $ init cs
          | otherwise = cs

checkSentence
  :: (String, KannadaSentence) -- ^ (id of sentence, sentence itself)
  -> Either String (String, KannadaSentence)
checkSentence (sid, cs) = fmap ((,) sid)
                        -- two separate runs to make sure addresses work for checking for cycles
                        $ mapM (checkChunkFS >=> checkAddress) >=> mapM (checkCyclity [])
                        $ cs
  where
    checkChunkFS c@(KannadaChunk{getChunkFS = fs})
      | fs == (ChunkFeatureSet Nothing "ROOT" "") = sentenceError sid $ "Chunk without fs"
      | otherwise = Right c
    checkAddress c@(KannadaChunk{getChunkFS = ChunkFeatureSet{getDRelHead = a}})
      | null a || findIdForAddress cs a /= Nothing = Right c
      | otherwise = sentenceError sid $ "Invalid Address: " ++ a
    -- | Right now this is not very efficient. Might be worth implementing more cleverly.
    checkCyclity fringe c@(KannadaChunk{getChunkFS = ChunkFeatureSet{getDRelHead = a}})
      = do let nextAddress = fromJust $ findIdForAddress cs a -- checkAdress allows fromJust
           let nextChunk = find (any ((==nextAddress) . getWordId) . getWords) cs
           -- trace (show nextAddress ++ ": " ++ show (nextChunk /= Nothing)) $ return ()
           if nextAddress == 0
             then Right c
             else if nextAddress `elem` fringe
                    then sentenceError sid $ "Cycle with word no. " ++ show nextAddress
                    else case checkCyclity (nextAddress : fringe) (fromJust nextChunk) of
                           Left e -> Left e
                           Right _ -> Right c

{-
Possible future assertions:
 * only one empty drel per sentence
 * what the hell is troot and mtype?
-}

fromJust' :: Maybe a -> a
fromJust' Nothing = error "AAAAH!"
fromJust' (Just x) = x


main = do
    parsedSentences <- getKannadaTB
    let coNLLTB = transformKannadaTBToCoNLL parsedSentences
        kannadaOpts = stdCoNLLExportOptions{getOutputPrefix = "../data/Kannada/sentences/"}
    generateTrainAndTestFiles kannadaOpts coNLLTB
