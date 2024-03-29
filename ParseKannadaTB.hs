module ParseKannadaTB where

import Data.Char (toLower)
import Data.Foldable (foldl')
import Data.List (intercalate, find)
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.State.Lazy

import Debug.Trace
import Control.Exception.Base (assert)

import KannadaTB
import CoNLLOutput

trim :: String -> String
trim = unwords . words
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

getKannadaTB :: Int -> IO KannadaTreebank
getKannadaTB i = do
    let treebankBasePath = "../data/datasources/kannada_new/"
        files = ["till 7400 sep 10", "TREE 1-3500", "TREE 12451 TO 16655", "TREE  ALL   SEP  10"]
    
    dirtyTreebankFile <- readFile $ treebankBasePath ++ (files !! i)
    
    -- This whole file is a dirty desaster on pretty much every level.
    -- Some cleaning is necessary before it touches any meaningful code.
    let cleanTreebankFile = replace "<fs\t<fs" "\t<fs"
                          $ replace " JJ<fs\taf='" "\tJJ\t<fs af='"
                          $ replace "ಸೋಜಿಗ\t\tN__NN " "ಸೋಜಿಗ\tN__NN\t"
                          $ replace "Axa'\t\n" "Axa'>\n"
                          $ replace "<fs af=' ಬರು,v,,sg,3" "<fs af='ಬರು,v,,sg,3"
                          $ replace "<fs af=' ಎಷ್ಟು,adj,,,,o" "<fs af='ಎಷ್ಟು,adj,,,,o"
                          $ replace "3,d,0,0\n" "3,d,0,0'>\n"
                          $ replace "\t <fs " "\t<fs "
                          $ replace "'>'>\n" "'>\n"
                          $ replace "'\n" "'>\n"
                          $ replace "> \n" ">\n"
                          $ replace ">\t\n" ">\n"
                          $ replace "<document id=\"\">\n<head>\n\n</head>\n<Sentence" "\n<Sentence"
                          $ replace "\n \n" "\n\n"
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
    
    let considerableSentences = id --takeWhile (\(i,_) -> i /= "513")
                              -- Contain null super-chunk
                              $ filter ((/='0') . head . head . snd)
                              -- Things are very wrong here
                              $ filter (not . (`elem` ["14", "423", "457", "1189", "4351", "6709", "14050", "14577"]) . fst)
                              -- All A-sentences Are Bastards. Apparently.
                              $ filter ((/='A') . last . fst)
                              $ allSentences
    
    let lookatit x ss = case x of Right s -> (s:ss); Left e -> ss -- trace e ss
    let fineSentenceParses = foldr lookatit []
                           $ map (parseSentence >=> checkSentence)
                           $ considerableSentences
    
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
                   -- Because many relnames exist in the strangest variations:
                   catchRsymEos s
                     | take 4 s == "rsym" || take 3 (reverse s) == "soe"
                         = "rsym_eos"
                     | otherwise = s
                   canonify = catchRsymEos
                            . replace "__" "_"
                            . replace "-" "_"
                            . map toLower
                   drelspec = splitOn ":"
                            $ fromMaybe "ROOT:"
                            $ lookup "drel" attrs
               in case drelspec of
                    [drelname, drelhead]
                       -> let fs = ChunkFeatureSet maybeaddress (canonify drelname) drelhead
                              newChunk = KannadaChunk (trim chunktag) fs []
                          in chunkReader (newChunk : chunksSoFar) remlines
                    ss -> return $ sentenceError i $ "Malformed drel pieces: " ++ show ss
               
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

ensure2 :: Show a => String -> [a] -> [a]
ensure2 _ [x, y] = [x, y]
ensure2 msg xs = error $ "Not just 2 elements: " ++ msg

-- We can't use TagSoup or something proper, because the file doesn't even care
-- about basic SGML conformity.
getAttrsForTag :: String -> String -> [(String, String)]
getAttrsForTag name "" = error $ "Expected tag " ++ name ++ " was empty"
getAttrsForTag name tag
  = assert (take (1 + length name) tag == "<" ++ name && last tag == '>')
  $ map ((\[k, v] -> (k, unquote v)) . ensure2 tag . splitOn "=")
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
    isNullish = (=="NULL") . take 4
    checkChunkFS c@(KannadaChunk{getChunkFS = fs, getWords = ws})
      | getAddress fs == Nothing
          = sentenceError sid $ "Chunk without named fs"
      | not (isNullish $ fromJust $ getAddress fs) && any isNullish (map getWord ws)
          = sentenceError sid $ "Chunk " ++ fromJust (getAddress fs) ++ " contains NULL word"
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

main = do
    parsedSentences <- fmap concat $ mapM getKannadaTB [0..3]
    let maxlength = 12500
        actuallength = length parsedSentences
    putStrLn $ show actuallength ++ " sentences would be available, using " ++ show (min maxlength actuallength)
    let coNLLTB = transformKannadaTBToCoNLL parsedSentences
        kannadaOpts = stdCoNLLExportOptions
                        { getOutputPrefix = "../data/Kannada/sentences/"
                        , getNoOfSentences = Just (min maxlength actuallength)
                        }
    generateTrainAndTestFiles kannadaOpts coNLLTB
