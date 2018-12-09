{-=Spellit=-}
{-=WebtoHunspell: A .aff File Creator.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Release=-}
{-=Synopsis:  This Haskell Script will create an affix file=-}
{-=and dictionary file from the JSON inputs.=-}

{-Syntax Extensions.-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE QuasiQuotes       #-}

{--------------------}

{-Imports-}

import Control.Monad (mzero)
import Data.Aeson as A
import Data.Aeson.Types as AT
import Data.Aeson.Internal as ATI
import Data.ByteString.Lazy.Char8 as LBSC
import Data.Char as Ch
import Data.Function as Fn
import Data.HashMap.Lazy as HL
import Data.List as L
import Data.List.Split as S
import qualified Data.List.Extra as E
import Data.Maybe as M
import Data.Ord as Or
import Data.Text as T
import System.Directory as D
import System.Environment as En
import System.IO as IO
import System.IO.Temp as Temp
import Text.PrettyPrint.Boxes as PB

{---------}

{-Custom Data Declarations (JSON).-}

--Paradigm
data Paradigm = Paradigm {
    paradigm :: String
  , slots :: [String]
  , words :: A.Value
  } deriving (Read,Show)

instance FromJSON Paradigm
    where
        parseJSON (Object v) = Paradigm <$> (v .: "paradigm_name")
                                        <*> (v .: "slots")
                                        <*> (v .: "words")

        parseJSON _          = mzero
--

--Inputs
data Inputs = Inputs {
    slotandword :: [(String,String)]
    } deriving (Read,Show)
--
--

{----------------------------------}

{-Affix file-}
{-Custom Data Declarations (after JSON).-}

data AffixFileEncoding = 
    AffixFileEncoding String
    deriving (Eq,Ord,Show)

data PreRuleHeader =
    PrePfxHead String String Char
  | PreSfxHead String String Char
    deriving (Eq,Ord,Show) 

data PreRule =
    PrePfx String String String String
  | PreSfx String String String String
    deriving (Eq,Ord,Show)

data RuleHeader =
    PfxHead String String Char Int
  | SfxHead String String Char Int
    deriving (Eq,Ord,Show)

data Rule = 
    Pfx String String String String String 
  | Sfx String String String String String
    deriving (Eq,Ord,Show)

{----------------------------------------}
{------------}

{-Dictionary file-}
{-Custom Data Declarations (after JSON).-}

data DictFileHeader = 
    DictFileHeader String
    deriving (Eq,Ord,Show)

data DictFileBody = 
    DictFileBody String
    deriving (Eq,Ord,Show)

{----------------------------------------}
{-----------------}

{-General Utility Functions.-}

--linefeed -> To feed file into nested list line by line, 
--delimited by newline characters.
linefeed :: String -> [String]
linefeed xs = L.lines xs

--singleunnest
singleunnest :: [a] -> a
singleunnest [xs] = xs

--tripletfst
tripletfst :: (a,b,c) -> a
tripletfst (a,b,c) = a

--tripletsnd
tripletsnd :: (a,b,c) -> b
tripletsnd (a,b,c) = b

--tripletthrd
tripletthrd :: (a,b,c) -> c
tripletthrd (a,b,c) = c

--mtripletfst
mtripletfst :: Maybe (a,b,c) -> a
mtripletfst (Just (a,b,c)) = a

--mtripletsnd
mtripletsnd :: Maybe (a,b,c) -> b
mtripletsnd (Just (a,b,c)) = b

--mtripletthrd 
mtripletthrd :: Maybe (a,b,c) -> c
mtripletthrd (Just (a,b,c)) = c

--tupletotriplet
tupletotriplet :: String -> (String,String) -> (String,String,String)
tupletotriplet x (y,z) = (x,y,z)

--maptupletotriplet
maptupletotriplet :: String -> [(String,String)] -> [(String,String,String)]
maptupletotriplet [] []        = []
maptupletotriplet _  []        = []
maptupletotriplet x ((y,z):xs) = [tupletotriplet x (y,z)] ++ (maptupletotriplet x xs)

{----------------------------}

{-Functions to grab information from parsed JSON.-}

--grabparadigm -> To grab the paradigm of the json.
grabparadigm :: Maybe Paradigm -> String
grabparadigm  Nothing                                  = []
grabparadigm (Just (Paradigm str1 liststr listinputs)) = str1

--grabslots -> To grab the slots of the json.
grabslots :: Maybe Paradigm -> [String]
grabslots  Nothing                                  = []
grabslots (Just (Paradigm str1 liststr listinputs)) = liststr

--grabwords -> To grab the words of the json.
grabwords :: Maybe Paradigm -> Maybe A.Value
grabwords (Just (Paradigm str1 liststr listvalues)) = Just listvalues

--parseinput -> To grab information out of object.
parseinput :: A.Value -> AT.Parser [Inputs]
parseinput xs = (L.map (\word -> Inputs (HL.toList word)) <$> parseJSON xs)

{-------------------------------------------------}

{-Functions dealing with Reading parsed JSON into original datatypes.-}

--readslots -> To read slots back into proper datatype.
readslots :: String -> [String]
readslots xs = read xs

--readwords -> To read words back into proper datatype.
readwords :: String -> Maybe [Inputs]
readwords xs = read xs

--slotandwordgrabber -> To grab all slotandword fields
--from the parsed Maybe [Inputs].
slotandwordgrab :: Maybe [Inputs] -> [[(String,String)]]
slotandwordgrab (Just [])                               = []
slotandwordgrab (Just ((Inputs {slotandword = sw}):xs)) = [sw] ++ (slotandwordgrab (Just xs))

--rootgrabber -> Used in prefinalreader.
rootgrabber :: [[(String,String)]] -> [[(String,String,String)]]
rootgrabber []     = []
rootgrabber (x:xs) = [(maptupletotriplet (L.concat (L.map (snd) (L.filter ((=="root").fst) x))) x)] ++ (rootgrabber xs)

--rootremover -> Used in truefinalreader.
rootremover :: [[(String,String,String)]] -> [[(String,String,String)]]
rootremover []     = []
rootremover (x:xs) = [L.filter (not . ((=="root").tripletsnd)) x] ++ (rootremover xs)

--readchunker -> To chunk the input appropriately.
readchunker :: [String] -> [[String]]
readchunker [] = []
readchunker xs = S.chunksOf 3 xs

--beginreader -> To transform elements of readchunker back into correct types.
beginreader :: [[String]] -> [(String,[String],Maybe [Inputs])]
beginreader []           = [] 
beginreader ([x,y,z]:xs) = [(x,(readslots y),(readwords z))] ++ (beginreader xs)

--midreader -> To transform elements of beginreader
--into intermediate format.
midreader :: [(String,[String],Maybe [Inputs])] -> [(String,[String],[[(String,String)]])]
midreader []                = []
midreader ((x,y,Just z):xs) = [(x,y,slotandwordgrab (Just z))] ++ (midreader xs) 

--midfinalreader -> To tranform elements of midreader
--into a further intermeditate format.
midfinalreader :: [(String,[String],[[(String,String)]])] -> [(String,[[(String,String)]])]
midfinalreader []           = [] 
midfinalreader ((x,y,z):xs) = [(x,z)] ++ (midfinalreader xs)

--prefinalreader ->  To transform the elements of midfinalreader
--into a further intermediate format.
prefinalreader :: [(String,[[(String,String)]])] -> [(String,[[(String,String,String)]])]
prefinalreader []         = []
prefinalreader ((x,y):xs) = [(x,rootgrabber y)] ++ (prefinalreader xs)

--truefinalreader -> To tranform the elements of prefinalreader
--into the necessary format of presufcheck.
truefinalreader :: [(String,[[(String,String,String)]])] -> [(String,[[(String,String,String)]])]
truefinalreader []         = []
truefinalreader ((x,y):xs) = [(x,rootremover y)] ++ (truefinalreader xs)

{---------------------------------------------------------------------}

{-Function operating to take each root and corresponding root paradigm-}
{-and determine differences between them.-}

--presufcheck -> This function will check for prefixes and 
--suffixes for each root form to its corresponding root.
presufcheck :: [(String,[[(String,String,String)]])] -> [(String,[[(String,Maybe (String,String,String))]])]
presufcheck []          = []
presufcheck ((a,xs):ys) = [(a,nesteddiff xs)] ++ (presufcheck ys)
    where
        --Nested Function Definitions.--
        --maybetriplet
        maybetriplet :: (Maybe (String,String),String) -> Maybe (String,String,String)
        maybetriplet (Just (a,b),c) = Just (a,b,c) 
        --nesteddiff
        nesteddiff :: [[(String,String,String)]] -> [[(String,Maybe (String,String,String))]]
        nesteddiff []     = []
        nesteddiff (x:xs) = [diff x] ++ (nesteddiff xs)
        --diff
        diff :: [(String,String,String)] -> [(String,Maybe (String,String,String))]  
        diff []           = [] 
        diff ((x,y,z):xs) = if L.null (x L.\\ z)
                                then if L.isInfixOf x z == True
                                    then [((x,maybetriplet (E.stripInfix x z , "0")))] ++ (diff xs)
                                    else [((x,Just ("",stripper (totuple z x),stripper (totuple x z))))] ++ (diff xs) 
                                else [((x,Just ("",stripper (totuple z x),stripper (totuple x z))))] ++ (diff xs)
            where
                --Nested Function Defintions.--
                --stripper
                stripper :: (String,String) -> String
                stripper ([],[]) = []
                stripper (_,[]) = []
                stripper ([],_) = []
                stripper ((a:as),(b:bs)) = if a /= b
                                               then [a] ++ as
                                               else stripper (as,bs)   
                --totuple
                totuple :: String -> String -> (String,String)
                totuple a b = (a,b)
                -------------------------------

{----------------------------------------------------------}
{----------------------------------------------------------------------}

{-Functions to pull result of presufcheck into lists-}
{-based on datatypes above to generate affix file.-}

--simplifypresufcheck -> This function will change
--the format of the results of presufcheck.
simplifypresufcheck :: [(String,[[(String,Maybe (String,String,String))]])] -> [(String,[(String,[Maybe (String,String,String)])])]
simplifypresufcheck []          = [] 
simplifypresufcheck ((a,xs):ys) = [(a,nestedchanger xs)] ++ (simplifypresufcheck ys)
    where 
        --Nested Function Definitions.--
        --nestedchanger
        nestedchanger :: [[(String,Maybe (String,String,String))]] -> [(String,[Maybe (String,String,String)])]
        nestedchanger []     = [] 
        nestedchanger (x:xs) = [changer x] ++ (nestedchanger xs)
        --changer
        changer :: [(String,Maybe (String,String,String))] -> (String,[Maybe (String,String,String)])
        changer []     = ([],[])
        changer (x:xs) = (fst x,([snd x] ++ (snd (changer xs))))
        --------------------------------

--pfxsfx -> This function will run through the result
--of presufcheck and add PFX or SFX to each tuple.
pfxsfx :: [(String,[(String,[Maybe (String,String,String)])])] -> [(String,[(String,[(Maybe String,Maybe (String,String,String))])])] 
pfxsfx []     = []
pfxsfx ((a,xs):ys) = [(a,nestedadder xs)] ++ (pfxsfx ys)
    where
        --Nested Function Definitions.--
        --nestedadder
        nestedadder :: [(String,[Maybe (String,String,String)])] -> [(String,[(Maybe String,Maybe (String,String,String))])]
        nestedadder []     = []
        nestedadder (x:xs) = [adder x] ++ (nestedadder xs)
        --adder 
        adder :: (String,[Maybe (String,String,String)]) -> (String,[(Maybe String,Maybe (String,String,String))])
        adder ([],[])     = ([],[])
        adder ((_:_), []) = ([],[])
        adder (x,(y:ys))  = 
            if (L.null (M.fromJust (fmap (tripletsnd) y))) && (not (L.null (M.fromJust (fmap (tripletfst) y))))
                then (x,[(Just "PFX",y)] ++ (snd (adder (x,ys)))) 
                else if (L.null (M.fromJust (fmap (tripletfst) y))) && (not (L.null (M.fromJust (fmap (tripletsnd) y))))
                    then (x,[(Just "SFX",y)] ++ (snd (adder (x,ys)))) 
                    else if (not (L.null (M.fromJust (fmap (tripletfst) y)))) && (not (L.null (M.fromJust (fmap (tripletsnd) y))))
                        then (x,[(Just "PFX/SFX",y)] ++ (snd (adder (x,ys))))
                        else (x,[(Nothing,y)] ++ (snd (adder (x,ys))))
        --------------------------------

--toprerule -> This function will pull data from presufcheck
--into a list based on PreRuleHeader and PreRule.
toprerule :: [(String,[(String,[(Maybe String,Maybe (String,String,String))])])] -> [String] -> [(String,[(String,[(PreRuleHeader,PreRule)])])]
toprerule []          []        = []
toprerule []          (_:_)     = []
toprerule ((a,xs):ys) (z:zs)    = [(a,prenestedruler z xs)] ++ (toprerule ys zs)
    where
        --Nested Function Definitions.--
        --prenestedruler
        prenestedruler :: String -> [(String,[(Maybe String,Maybe (String,String,String))])] -> [(String,[(PreRuleHeader,PreRule)])]
        prenestedruler [] []    = []
        prenestedruler (_:_) [] = []
        prenestedruler z (x:xs) = [preruler z x] ++ (prenestedruler z xs) 
        --preruler
        preruler :: String -> (String,[(Maybe String,Maybe (String,String,String))]) -> (String,[(PreRuleHeader,PreRule)])
        preruler [] ([],[])           = ([],[])
        preruler [] ((_:_), [])       = ([],[])
        preruler (_:_) (_, [])        = ([],[])
        preruler []    ([], (_:_))    = ([],[])
        preruler []    ((_:_), (_:_)) = ([],[])
        preruler z (x,(y:ys))    = 
            if fst y == Just "PFX" 
                then (x,[((PrePfxHead "PFX" (z ++ "P") 'Y') , (PrePfx "PFX" (z ++ "P") (mtripletthrd $ (snd y)) (mtripletfst $ (snd y))))] ++ (snd (preruler z (x,ys))))
                else if fst y == Just "SFX"
                    then (x,[((PreSfxHead "SFX" (z ++ "S") 'Y') , (PreSfx "SFX" (z ++ "S") (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y))))] ++ (snd (preruler z (x,ys)))) 
                    else if fst y == Just "PFX/SFX" 
                        then (x,[((PrePfxHead "PFX" (z ++ "P") 'Y') , (PrePfx "PFX" (z ++ "P") (mtripletthrd $ (snd y)) (mtripletfst $ (snd y))))] ++ [((PreSfxHead "SFX" (z ++ "S") 'Y') , (PreSfx "SFX" (z ++ "S") (mtripletthrd $ (snd y)) (mtripletsnd $ (snd y))))] ++ (snd (preruler z (x,ys))))
                            else preruler z (x,ys) 
        --------------------------------

--simplifytoprerule -> This function will simplify 
--the results of toprerule.
simplifytoprerule :: [(String,[(String,[(PreRuleHeader,PreRule)])])] -> [(String,[[(String,PreRuleHeader,PreRule)]])]
simplifytoprerule []          = []
simplifytoprerule ((a,xs):ys) = [(a,aggregaterootprerule xs)] ++ (simplifytoprerule ys)
    where
        --Nested Function Definitions.--
        --aggregaterootprerule
        aggregaterootprerule :: [(String,[(PreRuleHeader,PreRule)])] -> [[(String,PreRuleHeader,PreRule)]]
        aggregaterootprerule []     = []
        aggregaterootprerule (x:xs) = [rootprerule x] ++ (aggregaterootprerule xs)
        --rootprerule
        rootprerule :: (String,[(PreRuleHeader,PreRule)]) -> [(String,PreRuleHeader,PreRule)]
        rootprerule ([],[])        = [] 
        rootprerule ((_:_),[])     = []
        rootprerule ((x,(y,z):ys)) = [(x,y,z)] ++ (rootprerule (x,ys))  
        --------------------------------

--finalsimplifytoprerule -> This function will further
--simplify the results of simplifytoprerule.
finalsimplifytoprerule :: [(String,[[(String,PreRuleHeader,PreRule)]])] -> [(String,[(String,PreRuleHeader,PreRule)])]
finalsimplifytoprerule []     = [] 
finalsimplifytoprerule (x:xs) = [rootprerule x] ++ (finalsimplifytoprerule xs)
    where
        --Nested Function Definitions.--
        --rootprerule
        rootprerule :: (String,[[(String,PreRuleHeader,PreRule)]]) -> (String,[(String,PreRuleHeader,PreRule)])
        rootprerule ([],[])    = ([],[])    
        rootprerule ((_:_),[]) = ([],[])
        rootprerule (x,y) = (x,L.concat y) 
        --------------------------------  

--wordandprerule -> This function will take the results
--of finalsimplifytoprerule and grab just the second part
--of each list.
wordandprerule :: [(String,[(String,PreRuleHeader,PreRule)])] -> [[(String,PreRuleHeader,PreRule)]]
wordandprerule [] = []
wordandprerule (x:xs) = [listgrab x] ++ (wordandprerule xs)
    where
        --Nested Function Definitions.--
        --listgrab
        listgrab :: (String,[(String,PreRuleHeader,PreRule)]) -> [(String,PreRuleHeader,PreRule)]
        listgrab ([],[])    = []
        listgrab ((_:_),[]) = []
        listgrab (x,(y:ys)) = [y] ++ (listgrab (x,ys))
        --------------------------------

--preparetorule -> This function will take the results
--of wordandprerule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
preparetorule :: [[(String,PreRuleHeader,PreRule)]] -> [[[(String,PreRuleHeader,PreRule)]]]
preparetorule []     = []
preparetorule (x:xs) = [initialsortandgroup x] ++ (preparetorule xs) 
    where
        --Nested Function Definitions.--
        --initialsortandgroup
        initialsortandgroup :: [(String,PreRuleHeader,PreRule)] -> [[(String,PreRuleHeader,PreRule)]]
        initialsortandgroup [] = [] 
        initialsortandgroup xs = L.groupBy (\(_,b,_) (_,e,_) -> b == e) 
                                 (L.sortOn (\ys -> case ys of (_,PrePfxHead b _ _,_) -> b 
                                                              (_,PreSfxHead b _ _,_) -> b) xs)
       -------------------------------- 

--sortprerule -> This function will take the results
--of preparetorule and prepare it to be loaded 
--into the RuleHeader and Rule datatypes.
sortprerule :: [[[(String,PreRuleHeader,PreRule)]]] -> [[[(String,PreRuleHeader,PreRule)]]]
sortprerule []     = []
sortprerule (x:xs) = [largeinitialsort x] ++ (sortprerule xs)
    where 
        --Nested Function Definitions.--
        --largeinitialsort
        largeinitialsort :: [[(String,PreRuleHeader,PreRule)]] -> [[(String,PreRuleHeader,PreRule)]]
        largeinitialsort []     = []
        largeinitialsort (x:xs) = [initialsort x] ++ (largeinitialsort xs)
        --initialsort
        initialsort :: [(String,PreRuleHeader,PreRule)] -> [(String,PreRuleHeader,PreRule)]
        initialsort [] = []
        initialsort xs = L.sortOn (\zs -> case zs of (_,_,PrePfx _ _ c _) -> c
                                                     (_,_,PreSfx _ _ c _) -> c) (L.sortOn (\ys -> case ys of (_,_,PrePfx _ _ _ c) -> c 
                                                                                                             (_,_,PreSfx _ _ _ c) -> c) xs)
        --------------------------------  

--finaldifferences -> This function will take the results
--of secondnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes
finaldifferences :: [[[(String,PreRuleHeader,PreRule)]]] -> [[[([(String,PreRuleHeader,PreRule)],[(String,String,String)])]]]
finaldifferences []     = []
finaldifferences (x:xs) = [nestedfinaldifferences x] ++ (finaldifferences xs)    
    where
        --Nested Function Definitions.--
        --nestedfinaldifferences
        nestedfinaldifferences :: [[(String,PreRuleHeader,PreRule)]] -> [[([(String,PreRuleHeader,PreRule)],[(String,String,String)])]]
        nestedfinaldifferences [] = []
        nestedfinaldifferences (x:xs) = [insertfinaldifferences x] ++ (nestedfinaldifferences xs)
        --insertfinaldifferences
        insertfinaldifferences :: [(String,PreRuleHeader,PreRule)] -> [([(String,PreRuleHeader,PreRule)],[(String,String,String)])]
        insertfinaldifferences [] = []
        insertfinaldifferences xs = [(xs,bothfinaldifferences xs)] 
        --bothfinaldifferences
        bothfinaldifferences :: [(String,PreRuleHeader,PreRule)] -> [(String,String,String)]
        bothfinaldifferences [] = []
        bothfinaldifferences xs = if (L.any (isprepfx) xs)
                                      then largealldifferences (pfxdmappedalldifferences (mappedpermutationmapper (allrotations (changerpfx xs))))
                                      else largealldifferences (sfxdmappedalldifferences (mappedpermutationmapper (allrotations (changersfx xs))))
        --largealldifferences
        largealldifferences :: [[(String,String,String)]] -> [(String,String,String)]
        largealldifferences [] = []
        largealldifferences xs = L.concat xs
        --pfxdmappedalldifferences
        pfxdmappedalldifferences :: [[(String,String)]] -> [[(String,String,String)]]
        pfxdmappedalldifferences [] = []
        pfxdmappedalldifferences (x:xs) = [pfxsmappedalldifferences x] ++ (pfxdmappedalldifferences xs)
        --pfxsmappedalldifferences
        pfxsmappedalldifferences :: [(String,String)] -> [(String,String,String)]
        pfxsmappedalldifferences [] = []
        pfxsmappedalldifferences ((x,y):xs) = [(x,y,pfxalldifferences x y)] ++ (pfxsmappedalldifferences xs)
        --sfxdmappedalldifferences
        sfxdmappedalldifferences :: [[(String,String)]] -> [[(String,String,String)]]
        sfxdmappedalldifferences [] = []
        sfxdmappedalldifferences (x:xs) = [sfxsmappedalldifferences x] ++ (sfxdmappedalldifferences xs)
        --sfxsmappedalldifferences
        sfxsmappedalldifferences :: [(String,String)] -> [(String,String,String)]
        sfxsmappedalldifferences [] = []
        sfxsmappedalldifferences ((x,y):xs) = [(L.reverse x,L.reverse y,sfxalldifferences x y)] ++ (sfxsmappedalldifferences xs)
        --pfxalldifferences
        pfxalldifferences :: String -> String -> String
        pfxalldifferences []     []     = []
        pfxalldifferences (x:xs) (y:ys) = if x == y
                                              then [x] ++ (pfxalldifferences xs ys)
                                              else if x /= y
                                                  then [x]
                                                  else pfxalldifferences xs ys
        --sfxalldifferences
        sfxalldifferences :: String -> String -> String
        sfxalldifferences []     []     = []
        sfxalldifferences (x:xs) (y:ys) = if x == y
                                           then (sfxalldifferences xs ys) ++ [x]
                                           else if x /= y
                                               then [x]
                                               else sfxalldifferences xs ys
        --mappedpermutationmapper
        mappedpermutationmapper :: [[String]] -> [[(String,String)]]
        mappedpermutationmapper [] = []
        mappedpermutationmapper xs = L.map (permutationmapper) xs
        --permutationmapper
        permutationmapper :: [String] -> [(String,String)]
        permutationmapper [] = []
        permutationmapper xs = L.filter (\(a,b) -> a == L.head xs && a /= b) (L.concat $ L.zipWith (L.zip . L.repeat) xs $ L.tails xs)
        --allrotations
        allrotations :: [String] -> [[String]]
        allrotations xs = L.take (L.length xs) (L.iterate singlerotate xs)
        --singlerotate
        singlerotate :: [String] -> [String]
        singlerotate [] = []
        singlerotate (x:xs) = xs ++ [x]
        --changerpfx
        changerpfx :: [(String,PreRuleHeader,PreRule)] -> [String]
        changerpfx [] = []
        changerpfx xs = L.nub (L.sort (L.map (tripletfst) xs))
        --changersfx 
        changersfx :: [(String,PreRuleHeader,PreRule)] -> [String] 
        changersfx [] = []
        changersfx xs =  L.map (L.reverse) (L.nub (L.sort (L.map (tripletfst) xs)))
        --isprepfx
        isprepfx :: (String,PreRuleHeader,PreRule) -> Bool
        isprepfx (_,_,PrePfx _ _ _ _) = True
        isprepfx (_,_,PreSfx _ _ _ _) = False   
        --------------------------------  

--allgroup -> This function will take the results
--of firstnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
allgroup :: [[[([(String,PreRuleHeader,PreRule)],[(String,String,String)])]]] -> [[[([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])]]]
allgroup [] = []
allgroup (x:xs) = [largeinitialgroup x] ++ (allgroup xs)
    where
        --Nested Function Definitions.--
        --largeinitialgroup
        largeinitialgroup :: [[([(String,PreRuleHeader,PreRule)],[(String,String,String)])]] -> [[([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])]]
        largeinitialgroup []     = []
        largeinitialgroup (x:xs) = [smallinitialgroup x] ++ (largeinitialgroup xs)
        --smallinitialgroup
        smallinitialgroup :: [([(String,PreRuleHeader,PreRule)],[(String,String,String)])] -> [([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])]
        smallinitialgroup []         = []
        smallinitialgroup ((x,y):xs) = [(initialgroup x,y)] ++ (smallinitialgroup xs) 
        --initialgroup
        initialgroup :: [(String,PreRuleHeader,PreRule)] -> [[(String,PreRuleHeader,PreRule)]]
        initialgroup [] = []
        initialgroup xs = if (L.any (isprepfx) xs)
                              then L.groupBy (\(_,_,PrePfx _ _ c d) (_,_,PrePfx _ _ g h) -> (c == g && d == h)) xs
                              else L.groupBy (\(_,_,PreSfx _ _ c d) (_,_,PreSfx _ _ g h) -> (c == g && d == h)) xs
        --isprepfx
        isprepfx :: (String,PreRuleHeader,PreRule) -> Bool
        isprepfx (_,_,PrePfx _ _ _ _) = True
        isprepfx (_,_,PreSfx _ _ _ _) = False
        --------------------------------

--preresolver -> This function will take the results
--of firstalldifferences and prepare it to be
--loaded into the RuleHeader and Rule datatypes.
preresolver :: [[[([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])]]] -> [[[[([(String,PreRuleHeader,PreRule)],[(String,String)])]]]]
preresolver []     = []
preresolver (x:xs) = [nestednestedalldiffadder x] ++ (preresolver xs)
    where
        --Nested Function Definitions.--
        --nestednestedalldiffadder
        nestednestedalldiffadder :: [[([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])]] -> [[[([(String, PreRuleHeader, PreRule)], [(String,String)])]]]
        nestednestedalldiffadder [] = [] 
        nestednestedalldiffadder (x:xs) = [nestedalldiffadder x] ++ (nestednestedalldiffadder xs)
        --nestedalldiffadder
        nestedalldiffadder :: [([[(String,PreRuleHeader,PreRule)]],[(String,String,String)])] -> [[([(String, PreRuleHeader, PreRule)], [(String,String)])]]
        nestedalldiffadder [] = []
        nestedalldiffadder (x:xs) = [alldiffadder x] ++ (nestedalldiffadder xs)         
        --alldiffadder
        alldiffadder :: ([[(String,PreRuleHeader,PreRule)]],[(String,String,String)]) -> [([(String, PreRuleHeader, PreRule)], [(String,String)])]
        alldiffadder ([], _)    = []
        alldiffadder ((x:xs),y) = [(x,alldifflittlefilter (littlefilter (onlystring x) y))] ++ (alldiffadder (xs,y))
        --alldifflittlefilter
        alldifflittlefilter :: [(String,String,String)] -> [(String,String)]
        alldifflittlefilter []     = []
        alldifflittlefilter (x:xs) = [(tripletfst x,tripletthrd x)] ++ (alldifflittlefilter xs)
        --littlefilter
        littlefilter :: [String] -> [(String,String,String)] -> [(String,String,String)]
        littlefilter [] []     = []
        littlefilter [] (_:_)  = []
        littlefilter (x:xs) ys = (L.filter (\z -> (x == tripletfst z)) ys) ++ (littlefilter xs ys) 
        --onlystring
        onlystring :: [(String,PreRuleHeader,PreRule)] -> [String]
        onlystring [] = []
        onlystring xs = L.map (tripletfst) xs
        --------------------------------

--simplifypreresolver -> This function will take
--the results of preresolver and prepare it to
--be loaded into the RuleHeader and Rule dataypes.
simplifypreresolver :: [[[[([(String,PreRuleHeader,PreRule)],[(String,String)])]]]] -> [[[[[(String,PreRuleHeader,PreRule,[(String,String)])]]]]]
simplifypreresolver [] = []
simplifypreresolver (x:xs) = [nestednestedsimplify x] ++ (simplifypreresolver xs)
    where
        --Nested Function Definitions.--
        --nestednestedsimplify
        nestednestedsimplify :: [[[([(String,PreRuleHeader,PreRule)],[(String,String)])]]] -> [[[[(String,PreRuleHeader,PreRule,[(String,String)])]]]]
        nestednestedsimplify [] = []
        nestednestedsimplify (x:xs) = [nestedsimplify x] ++ (nestednestedsimplify xs)
        --nestedsimplify
        nestedsimplify :: [[([(String,PreRuleHeader,PreRule)],[(String,String)])]] -> [[[(String,PreRuleHeader,PreRule,[(String,String)])]]]
        nestedsimplify [] = []
        nestedsimplify (x:xs) = [simplify x] ++ (nestedsimplify xs)
        --simplify
        simplify :: [([(String,PreRuleHeader,PreRule)],[(String,String)])] -> [[(String,PreRuleHeader,PreRule,[(String,String)])]]
        simplify [] = []
        simplify (x:xs) = [mergetuple x] ++ (simplify xs)
        --mergetuple
        mergetuple :: ([(String,PreRuleHeader,PreRule)],[(String,String)]) -> [(String,PreRuleHeader,PreRule,[(String,String)])]
        mergetuple ([],_)     = []
        mergetuple ((x:xs),y) = [morph x y] ++ (mergetuple (xs,y))
        --morph
        morph :: (String,PreRuleHeader,PreRule) -> [(String,String)] -> (String,PreRuleHeader,PreRule,[(String,String)])
        morph (a,b,c) d = (a,b,c,d)
        --------------------------------

--resolver -> This function will take
--the results of simplifypreresolver
--and prepare it to be loaded into
--the RuleHeader and Rule datatypes.
resolver :: [[[[[(String,PreRuleHeader,PreRule,[(String,String)])]]]]] -> [[[[[(String,PreRuleHeader,PreRule,(String,String))]]]]]
resolver [] = []
resolver (x:xs) = [nestednestednestednestedlengthchecker x] ++ (resolver xs)
    where
        --Nested Function Definitions.--
        --nestednestednestednestedlengthchecker
        nestednestednestednestedlengthchecker :: [[[[(String,PreRuleHeader,PreRule,[(String,String)])]]]] -> [[[[(String,PreRuleHeader,PreRule,(String,String))]]]]
        nestednestednestednestedlengthchecker [] = []
        nestednestednestednestedlengthchecker (x:xs) = [nestednestednestedlengthchecker x] ++ (nestednestednestednestedlengthchecker xs)
        --nestednestednestedlengthchecker
        nestednestednestedlengthchecker :: [[[(String,PreRuleHeader,PreRule,[(String,String)])]]] -> [[[(String,PreRuleHeader,PreRule,(String,String))]]]
        nestednestednestedlengthchecker [] = []
        nestednestednestedlengthchecker (x:xs) = [nestednestedlengthchecker x] ++ (nestednestednestedlengthchecker xs)
        --nestednestedlengthchecker
        nestednestedlengthchecker :: [[(String,PreRuleHeader,PreRule,[(String,String)])]] -> [[(String,PreRuleHeader,PreRule,(String,String))]]
        nestednestedlengthchecker [] = []
        nestednestedlengthchecker (x:xs) = [nestedlengthchecker x] ++ (nestednestedlengthchecker xs)
        --nestedlengthchecker
        nestedlengthchecker :: [(String,PreRuleHeader,PreRule,[(String,String)])] -> [(String,PreRuleHeader,PreRule,(String,String))]
        nestedlengthchecker [] = []
        nestedlengthchecker (x:xs) = [lengthchecker x] ++ (nestedlengthchecker xs)
        --lengthchecker
        lengthchecker :: (String,PreRuleHeader,PreRule,[(String,String)]) -> (String,PreRuleHeader,PreRule,(String,String))
        lengthchecker (a,b,c,d) = if (not (L.null d))
                                      then (a,b,c,maxlengthmatcher a d)
                                      else if (isprepfx (a,b,c,d))
                                          then (a,b,c,(a,[L.head a]))
                                          else (a,b,c,(a,[L.last a]))
        --maxlengthmatcher
        maxlengthmatcher :: String -> [(String,String)] -> (String,String)
        maxlengthmatcher x ys = L.maximumBy (tuplelengthcomparator) (L.filter (\z -> (x == fst z)) ys) 
        --tuplelengthcomparator
        tuplelengthcomparator :: (String,String) -> (String,String) -> Ordering
        tuplelengthcomparator (_,b) (_,d) = if (on Or.compare L.length b d) == EQ
                                                then on Or.compare (Ch.toLower <$>) b d
                                                else on Or.compare L.length b d
        --isprepfx
        isprepfx :: (String,PreRuleHeader,PreRule,[(String,String)]) -> Bool
        isprepfx (_,_,PrePfx _ _ _ _,_) = True
        isprepfx (_,_,PreSfx _ _ _ _,_) = False
        --------------------------------

--subruleadder -> This function will take the results
--of resolver and prepare it to be loaded into
--the RuleHeader and Rule datatypes.
subruleadder :: [[[[[(String,PreRuleHeader,PreRule,(String,String))]]]]] -> [[[[[(String,PreRuleHeader,PreRule,[[String]])]]]]]
subruleadder [] = []
subruleadder (x:xs) = [nestednestednestedinsertsubrule x] ++ (subruleadder xs)
    where
        --Nested Function Definitions.--
        --nestednestednestedinsertsubrule
        nestednestednestedinsertsubrule :: [[[[(String,PreRuleHeader,PreRule,(String,String))]]]] -> [[[[(String,PreRuleHeader,PreRule,[[String]])]]]]
        nestednestednestedinsertsubrule []     = []
        nestednestednestedinsertsubrule (x:xs) = [nestednestedinsertsubrule x] ++ (nestednestednestedinsertsubrule xs)
        --nestednestedinsertsubrule
        nestednestedinsertsubrule :: [[[(String,PreRuleHeader,PreRule,(String,String))]]] -> [[[(String,PreRuleHeader,PreRule,[[String]])]]]
        nestednestedinsertsubrule []     = [] 
        nestednestedinsertsubrule (x:xs) = [nestedinsertsubrule x] ++ (nestednestedinsertsubrule xs)
        --nestedinsertsubrule
        nestedinsertsubrule :: [[(String,PreRuleHeader,PreRule,(String,String))]] -> [[(String,PreRuleHeader,PreRule,[[String]])]] 
        nestedinsertsubrule []     = [] 
        nestedinsertsubrule (x:xs) = [insertsubrule x] ++ (nestedinsertsubrule xs)
        --insertsubrule
        insertsubrule :: [(String,PreRuleHeader,PreRule,(String,String))] -> [(String,PreRuleHeader,PreRule,[[String]])]
        insertsubrule [] = [] 
        insertsubrule xs = subrulemapper (subrulegrouper xs) xs
        --subrulemapper
        subrulemapper :: [[String]] -> [(String,PreRuleHeader,PreRule,(String,String))] -> [(String,PreRuleHeader,PreRule,[[String]])]
        subrulemapper []    [] = []
        subrulemapper (_:_) [] = []
        subrulemapper x ((a,b,c,d):ys) = [(a,b,c,x)] ++ (subrulemapper x ys)
        --subrulegrouper
        subrulegrouper :: [(String,PreRuleHeader,PreRule,(String,String))] -> [[String]]
        subrulegrouper [] = []
        subrulegrouper xs = L.groupBy (\a b -> L.length a == L.length b) (L.map (snd) (L.map (tuplegrabber) xs))
        --tuplegrabber
        tuplegrabber :: (a,b,c,d) -> d
        tuplegrabber (a,b,c,d) = d
        --------------------------------

--subruleregex -> This function will take the results
--of subruleadder and prepare it to be loaded into
--the RuleHeader and Rule datatypes.
subruleregex :: [[[[[(String,PreRuleHeader,PreRule,[[String]])]]]]] -> [[[[[(String,PreRuleHeader,PreRule,[String])]]]]]
subruleregex []     = []
subruleregex (x:xs) = [nestednestednestednestedregexadder x] ++ (subruleregex xs)
    where
        --Nested Function Definition.--
        --nestednestednestednestedregexadder
        nestednestednestednestedregexadder :: [[[[(String,PreRuleHeader,PreRule,[[String]])]]]] -> [[[[(String,PreRuleHeader,PreRule,[String])]]]]
        nestednestednestednestedregexadder []     = []
        nestednestednestednestedregexadder (x:xs) = [nestednestednestedregexadder x] ++ (nestednestednestednestedregexadder xs)
        --nestednestednestedregexadder
        nestednestednestedregexadder :: [[[(String,PreRuleHeader,PreRule,[[String]])]]] -> [[[(String,PreRuleHeader,PreRule,[String])]]]
        nestednestednestedregexadder []     = [] 
        nestednestednestedregexadder (x:xs) = [nestednestedregexadder x] ++ (nestednestednestedregexadder xs)
        --nestednestedregexadder
        nestednestedregexadder :: [[(String,PreRuleHeader,PreRule,[[String]])]] -> [[(String,PreRuleHeader,PreRule,[String])]]
        nestednestedregexadder []     = []
        nestednestedregexadder (x:xs) = [nestedregexadder x] ++ (nestednestedregexadder xs)
        --nestedregexadder
        nestedregexadder :: [(String,PreRuleHeader,PreRule,[[String]])] -> [(String,PreRuleHeader,PreRule,[String])]
        nestedregexadder []     = []
        nestedregexadder (x:xs) = [regexadder x] ++ (nestedregexadder xs)
        --regexadder
        regexadder :: (String,PreRuleHeader,PreRule,[[String]]) -> (String,PreRuleHeader,PreRule,[String])
        regexadder (a,b,c,d) = (a,b,c,regexmapper d)
        --regexmapper
        regexmapper :: [[String]] -> [String]
        regexmapper []     = []
        regexmapper (x:xs) = [regexcreator x] ++ (regexmapper xs)  
        --regexcreator
        regexcreator :: [String] -> String
        regexcreator ([]:_) = []
        regexcreator xs     = ("[" ++ (L.nub (L.sort (L.map (L.head) xs))) ++ "]") ++ (regexcreator (L.map (L.drop 1) xs)) 
        -------------------------------

--subrulecoupler -> This function will take the result
--of subruleregex and prepare it to be loaded into
--the RuleHeader and Rule datatypes.
subrulecoupler :: [[[[[(String,PreRuleHeader,PreRule,[String])]]]]] -> [[[[[(String,PreRuleHeader,PreRule,String)]]]]]
subrulecoupler [] = []
subrulecoupler (x:xs) = [nestednestedsubrulesingleadder x] ++ (subrulecoupler xs)
    where
        --Nested Function Definitions.--
        --nestednestedsubrulesingleadder
        nestednestedsubrulesingleadder :: [[[[(String,PreRuleHeader,PreRule,[String])]]]] -> [[[[(String,PreRuleHeader,PreRule,String)]]]]
        nestednestedsubrulesingleadder []     = []
        nestednestedsubrulesingleadder (x:xs) = [nestedsubrulesingleadder x] ++ (nestednestedsubrulesingleadder xs)
        --nestedsubrulesingleadder
        nestedsubrulesingleadder :: [[[(String,PreRuleHeader,PreRule,[String])]]] -> [[[(String,PreRuleHeader,PreRule,String)]]]
        nestedsubrulesingleadder []     = []
        nestedsubrulesingleadder (x:xs) = [subrulesingleadder x] ++ (nestedsubrulesingleadder xs)
        --subrulesingleadder
        subrulesingleadder :: [[(String,PreRuleHeader,PreRule,[String])]] -> [[(String,PreRuleHeader,PreRule,String)]]
        subrulesingleadder []     = []
        subrulesingleadder (x:xs) = [insertsubruleonetoone x] ++ (subrulesingleadder xs)
        --insertsubruleonetoone
        insertsubruleonetoone :: [(String,PreRuleHeader,PreRule,[String])] -> [(String,PreRuleHeader,PreRule,String)]
        insertsubruleonetoone [] = []
        insertsubruleonetoone xs = subruleonetoone (stringlistgrabber xs) xs
        --subruleonetoone
        subruleonetoone :: [String] -> [(String,PreRuleHeader,PreRule,[String])] -> [(String,PreRuleHeader,PreRule,String)]
        subruleonetoone []    []              = []
        subruleonetoone (_:_) []              = []
        subruleonetoone [] (_:_)              = []
        subruleonetoone (x:xs) ((a,b,c,d):ys) = [(a,b,c,x)] ++ (subruleonetoone xs ys)  
        --stringlistgrabber
        stringlistgrabber :: [(String,PreRuleHeader,PreRule,[String])] -> [String]
        stringlistgrabber xs = (\(a,b,c,d) -> d) (L.head xs)
        --------------------------------

--ruleloader -> This function will take the result
--of subrulecoupler and load the last element of 
--the quadruplet into the Rule datatype.
ruleloader :: [[[[[(String,PreRuleHeader,PreRule,String)]]]]] -> [[[[[(String,PreRuleHeader,PreRule,Rule)]]]]]
ruleloader [] = []
ruleloader (x:xs) = [nestednestednestedinsertrule x] ++ (ruleloader xs)
    where
        --Nested Function Definition.--
        --nestednestednestedinsertrule
        nestednestednestedinsertrule :: [[[[(String,PreRuleHeader,PreRule,String)]]]] -> [[[[(String,PreRuleHeader,PreRule,Rule)]]]]
        nestednestednestedinsertrule []     = []
        nestednestednestedinsertrule (x:xs) = [nestednestedinsertrule x] ++ (nestednestednestedinsertrule xs)
        --nestednestedinsertrule
        nestednestedinsertrule :: [[[(String,PreRuleHeader,PreRule,String)]]] -> [[[(String,PreRuleHeader,PreRule,Rule)]]]
        nestednestedinsertrule []     = []
        nestednestedinsertrule (x:xs) = [nestedinsertrule x] ++ (nestednestedinsertrule xs)
        --nestedinsertrule
        nestedinsertrule :: [[(String,PreRuleHeader,PreRule,String)]] -> [[(String,PreRuleHeader,PreRule,Rule)]]
        nestedinsertrule []     = []
        nestedinsertrule (x:xs) = [insertrule x] ++ (nestedinsertrule xs)
        --insertrule
        insertrule :: [(String,PreRuleHeader,PreRule,String)] -> [(String,PreRuleHeader,PreRule,Rule)]
        insertrule []                          = []
        insertrule ((a,b,PrePfx c d e f,g):xs) = [(a,b,PrePfx c d e f,Pfx c d e f g)] ++ (insertrule xs)
        insertrule ((a,b,PreSfx c d e f,g):xs) = [(a,b,PreSfx c d e f,Sfx c d e f g)] ++ (insertrule xs)
        -------------------------------

--simplifyruleloader -> This function will take the result
--of ruleloader and simplify it to
--add the RuleHeader datatype.
simplifyruleloader :: [[[[[(String,PreRuleHeader,PreRule,Rule)]]]]] -> [[[[(String,PreRuleHeader,PreRule,Rule)]]]]
simplifyruleloader [] = []
simplifyruleloader xs = L.concat xs
      
--simpruleloaderlengthmapper -> Used in ruleheadloader.
simpruleloaderlengthmapper :: [[[[(String,PreRuleHeader,PreRule,Rule)]]]] -> [[Int]]
simpruleloaderlengthmapper [] = []
simpruleloaderlengthmapper xs = L.map (L.map (L.length)) (L.concat xs)

--ruleheadloader -> This function will take the results
--of thirdnexttorule and prepare it to be loaded
--into the RuleHeader and Rule datatypes.
ruleheadloader :: [[[(String,PreRuleHeader,PreRule,Rule)]]] -> [[Int]] -> [[[(String,PreRuleHeader,PreRule,RuleHeader,Rule)]]]
ruleheadloader []     []     = []
ruleheadloader []  (_:_)     = []  
ruleheadloader (x:xs) (y:ys) = [L.map (L.map (\zs -> case zs of (a,PrePfxHead b c d,e,f) -> (a,PrePfxHead b c d,e,PfxHead b c d (L.sum y),f)
                                                                (a,PreSfxHead b c d,e,f) -> (a,PreSfxHead b c d,e,SfxHead b c d (L.sum y),f))) x] ++ (ruleheadloader xs ys)

--torule -> To use the result of fourthnexttorule
--to grab only one RuleHeader and Rule per nested list.
torule :: [[[(String,PreRuleHeader,PreRule,RuleHeader,Rule)]]] -> [(RuleHeader,Rule)]
torule [] = []
torule xs = L.map (\(_,_,_,d,e) -> (d,e)) (L.concat (L.concat xs))

--sorttorule -> To sort the results 
--of torule by Rule.
sorttorule :: [(RuleHeader,Rule)] -> [(RuleHeader,Rule)]
sorttorule [] = []
sorttorule xs = L.sortOn (\ys -> case ys of (PfxHead _ a _ _,_) -> a
                                            (SfxHead _ a _ _,_) -> a) xs

--grouptorule -> To group the results of sorttorule.
grouptorule :: [(RuleHeader,Rule)] -> [[(RuleHeader,Rule)]]
grouptorule [] = []
grouptorule xs =  L.groupBy (ruleheaderruleeq) xs
    where
        --Nested Function Definitions.--
        --ruleheaderruleeq
        ruleheaderruleeq :: (RuleHeader,Rule) -> (RuleHeader,Rule) -> Bool
        ruleheaderruleeq (PfxHead _ a _ _,Pfx _ _ _ _ _) (PfxHead _ b _ _,Pfx _ _ _ _ _) = if (a == b)
                                                                                               then True
                                                                                               else False
        ruleheaderruleeq (SfxHead _ a _ _,Sfx _ _ _ _ _) (SfxHead _ b _ _,Sfx _ _ _ _ _) = if (a == b)
                                                                                               then True
                                                                                               else False
        ruleheaderruleeq (_,_) (_,_) = False
        --------------------------------

--prefinalrule -> To prepare to print 
--the affixfile from the results of
--grouptorule.
prefinalrule :: [[(RuleHeader,Rule)]] -> [([RuleHeader],[Rule])]
prefinalrule [] = []
prefinalrule xs = combineruleheaderrule (grabruleheader xs) (grabrules xs) 
    where
        --Nested Function Definitions.--
        --combineruleheaderrule
        combineruleheaderrule :: [[RuleHeader]] -> [[Rule]] -> [([RuleHeader],[Rule])]
        combineruleheaderrule [] []         = []
        combineruleheaderrule [] (_:_)      = []
        combineruleheaderrule (_:_) []      = [] 
        combineruleheaderrule (x:xs) (y:ys) = [(x,y)] ++ (combineruleheaderrule xs ys)
        --grabruleheader 
        grabruleheader :: [[(RuleHeader,Rule)]] -> [[RuleHeader]]
        grabruleheader [] = []
        grabruleheader xs = L.map (L.nub) (L.map (L.map (fst)) xs)
        --grabrules
        grabrules :: [[(RuleHeader,Rule)]] -> [[Rule]]
        grabrules [] = []
        grabrules xs = L.map (L.map (snd)) xs
        --------------------------------

--finalrule -> To prepare for the final affix file
--from the results of prefinalrule.
finalrule :: [([RuleHeader],[Rule])] -> [([String],[String])]
finalrule [] = []
finalrule ((x,y):xs) = [(nestedruleheadertostring x,nestedruletostring y)] ++ (finalrule xs)
    where
        --Nested Function Definitions.--
        --nestedruleheadertostring
        nestedruleheadertostring :: [RuleHeader] -> [String]
        nestedruleheadertostring [] = []
        nestedruleheadertostring (x:xs) = [ruleheadertostring x] ++ (nestedruleheadertostring xs)
        --nestedruletostring
        nestedruletostring :: [Rule] -> [String]
        nestedruletostring [] = []
        nestedruletostring (x:xs) = [ruletostring x] ++ (nestedruletostring xs)
        --ruleheadertostring
        ruleheadertostring :: RuleHeader -> String
        ruleheadertostring (PfxHead a b c d) = a ++ "  " ++ b ++ "  " ++ [c] ++ "  " ++ (show d)
        ruleheadertostring (SfxHead a b c d) = a ++ "  " ++ b ++ "  " ++ [c] ++ "  " ++ (show d)
        --ruletostring
        ruletostring :: Rule -> String
        ruletostring (Pfx a b c d e) = a ++ "  " ++ b ++ "  " ++ c ++ "  " ++ d ++ "  " ++ e
        ruletostring (Sfx a b c d e) = a ++ "  " ++ b ++ "  " ++ c ++ "  " ++ d ++ "  " ++ e
        --------------------------------

--ruleaggregator
ruleaggregator :: [([String],[String])] -> [[[String]]]
ruleaggregator [] = []
ruleaggregator (x:xs) = [finaltupletolist x] ++ (ruleaggregator xs)
    where
        --Nested Function Definitions.--
        --finaltupletolist
        finaltupletolist :: ([String],[String]) -> [[String]]
        finaltupletolist (x,y) = [x] ++ [y]
        -------------------------------- 

--rulechunker 
rulechunker :: [[[String]]] -> [[String]]
rulechunker [] = []
rulechunker xs = singlechunker (concatenate xs) 
    where
        --Nested Function Definitions.--
        --singlechunker
        singlechunker :: [[String]] -> [[String]]
        singlechunker [] = []
        singlechunker (x:xs) = if (L.length x > 1)
                                   then (S.chunksOf 1 x) ++ (singlechunker xs)
                                   else [x] ++ (singlechunker xs)
        --concatenate
        concatenate :: [[[String]]] -> [[String]]
        concatenate [] = []
        concatenate xs = L.concat xs
        --------------------------------

--encoder
encoder :: [[String]] -> (AffixFileEncoding,[[String]])
encoder xs = ((AffixFileEncoding "SET UTF-8"),xs)

--encodingadder
encodingadder :: (AffixFileEncoding,[[String]]) -> (String,[[String]])
encodingadder (x,y) = (affixfileencodingtostring x,y)
    where
        --Nested Function Definitions.--
        --affixfileencodingtostring
        affixfileencodingtostring :: AffixFileEncoding -> String
        affixfileencodingtostring (AffixFileEncoding a) = a
        --------------------------------

--finalprintaffixlist
finalprintaffixlist :: (String,[[String]]) -> [[String]]
finalprintaffixlist ([],[]) = []
finalprintaffixlist (x,y) = [x] : y

{--------------------------------------------------}
{----------------------------------------------------}

{-Functions to take data from json input files and-}
{-generate dictionary file.-}

--rulerootconcatenator -> This function removes two levels of nesting
--from output of subruleregex function.
rulerootconcatenator :: [[[[[(String,PreRuleHeader,PreRule,[String])]]]]] -> [[[(String,PreRuleHeader,PreRule,[String])]]]
rulerootconcatenator [] = []
rulerootconcatenator xs =  L.concat (L.concat xs)

--rulerootaccumulator -> This function will use the output from 
--midfinalreader and prepare them to be loaded into
--DictFileHeader and DictFileBody datatypes.
rulerootaccumulator :: [[[(String,PreRuleHeader,PreRule,[String])]]] -> [(String,[String])] 
rulerootaccumulator []     = []
rulerootaccumulator (x:xs) = [singleaccumulator x] ++ (rulerootaccumulator xs)
    where
         --Nested Function Definitions.--
         singleaccumulator :: [[(String,PreRuleHeader,PreRule,[String])]] -> (String,[String])
         singleaccumulator [] = ([],[])
         singleaccumulator xs = (L.concat (L.nub (L.concat (L.map (L.map (\zs -> case zs of (_,PrePfxHead _ b _,_,_) -> b
                                                                                            (_,PreSfxHead _ b _,_,_) -> b)) xs))),L.nub (L.concat (L.map (L.map (\(a,_,_,_) -> a)) xs)))
         --------------------------------
         
--rulerootzipper -> This function will take the result of rulerootaccumulator
--and pair each rule to each root word.
rulerootzipper :: [(String,[String])] -> [[(String,String)]]
rulerootzipper []     = []
rulerootzipper (x:xs) = [zipper x] ++ (rulerootzipper xs)
    where
        --Nested Function Definitions.--
        --zipper
        zipper :: (String,[String]) -> [(String,String)] 
        zipper xs = L.zip (L.replicate (L.length (snd xs)) (fst xs)) (snd xs)
        --------------------------------

--rulerootsimplifier -> This function will remove one level of nesting
--from the output of rulerootzipper.
rulerootsimplifier :: [[(String,String)]] -> [(String,String)]
rulerootsimplifier [] = []
rulerootsimplifier xs = L.concat xs

--rulerootswapper -> This function will swap the first and 
--second element of the result of rulerootsimplifier.
rulerootswapper :: [(String,String)] -> [(String,String)]
rulerootswapper [] = []
rulerootswapper xs =  L.map (\(a,b) -> (b,a)) xs

--rulerootfinal -> This function will add the foward slash
--that is necessary to separate root words and rules 
--within the dictionary file from the result of 
--rulerootswapper.
rulerootfinal :: [(String,String)] -> [String]
rulerootfinal [] = []
rulerootfinal xs = L.map (\(a,b) -> a ++ "/" ++ b) xs

--ruleroottodictfilebody -> This function will push the result of
--rulerootfinal into the dictionary file datatypes.
ruleroottodictfilebody :: [String] -> [DictFileBody]
ruleroottodictfilebody []     = []
ruleroottodictfilebody (x:xs) = [DictFileBody x] ++ (ruleroottodictfilebody xs)

--dictfileheaderadder -> This function will add a custom dictionary file header
--datatype to the result of ruleroottodictfilebody.
dictfileheaderadder :: [DictFileBody] -> (DictFileHeader,[DictFileBody])
dictfileheaderadder xs = (DictFileHeader (show (L.length xs)),xs)

--dictfiletostring -> This function will turn the result of dictfileheaderadder
--and turn them into strings.
dictfiletostring :: (DictFileHeader,[DictFileBody]) -> (String,[String])
dictfiletostring (x,y) = (dictfileheadertostring x,nesteddictfilebodytostring y)
    where
        --Nested Function Definitions.--
        --dictfileheadertostring
        dictfileheadertostring :: DictFileHeader -> String
        dictfileheadertostring (DictFileHeader a) = a
        --nesteddictfilebodytostring
        nesteddictfilebodytostring :: [DictFileBody] -> [String]
        nesteddictfilebodytostring []     = []
        nesteddictfilebodytostring (x:xs) = [dictfilebodytostring x] ++ (nesteddictfilebodytostring xs)
        --dictfilebodytostring
        dictfilebodytostring :: DictFileBody -> String
        dictfilebodytostring (DictFileBody a) = a
        --------------------------------

--prefinalprintdictionarylist -> This will compress the result of 
--dictfiletostring into a single list of strings.
prefinalprintdictionarylist :: (String,[String]) -> [String]
prefinalprintdictionarylist ([],[]) = []
prefinalprintdictionarylist (x,y) = x : y

--finalprintdictionarylist -> This function will add single
--level of nesting to each element of the result of 
--prefinalprintdictionarylist.
finalprintdictionarylist :: [String] -> [[String]]
finalprintdictionarylist [] = []
finalprintdictionarylist xs =  L.map (\x -> [x]) xs 

{---------------------------}
{--------------------------------------------------}

{-IO Function.-}

--jsonparse -> This function will parse the JSON input files.
jsonparse :: [IO.FilePath] -> String -> (IO.FilePath,Handle) -> IO [String]
jsonparse []     []    (tempnamed,temphd) = return []
jsonparse []     (_:_) _                  = return []   
jsonparse (x:xs) ys    (tempnamed,temphd) = do 
    --Decode the current json file.
    jsondecoded <- (A.decode <$> (LBSC.readFile (ys ++ x))) :: IO (Maybe Paradigm)
    --Grab each part of decoded JSON.
    let paradigm = grabparadigm jsondecoded
    let slots    = grabslots jsondecoded
    let words    = parseMaybe parseinput =<< (grabwords jsondecoded)
    --Add these to a temp file. 
    IO.hPutStrLn temphd paradigm   
    IO.hPutStrLn temphd (show slots)
    IO.hPutStrLn temphd (show words)
    --Recurse through rest of list.
    jsonparse xs ys (tempnamed,temphd)

{--------------}

{-Main Function.-}

main :: IO ()
main = do
    --Get Command line arguments.
    cmdargs <- En.getArgs
    case cmdargs of
        [] -> error "No directory provided."
        [allargs] ->  do --Load in all files in allargs.
                         alljson <- D.listDirectory allargs
                         
                         --Create a temporary file.
                         (tempfile,temph) <- Temp.openTempFile "." "parsed.txt" 
                       
                         --Run jsonparse on all inputs.
                         _ <- jsonparse alljson allargs (tempfile,temph)
                      
                         --Seek to the beginning of the temporary file.
                         hSeek temph AbsoluteSeek 0

                         --Read temporary file into [[String]] 
                         fulltempread <- IO.hGetContents temph

                         --Prepare to print out affix file with finalaffixlist. 
                         let finalaffixlist = finalprintaffixlist
                                              (encodingadder
                                              (encoder
                                              (rulechunker
                                              (ruleaggregator 
                                              (finalrule 
                                              (prefinalrule 
                                              (grouptorule 
                                              (sorttorule 
                                              (torule 
                                              (ruleheadloader concatenator lengthmapper))))))))))
                                                  where
                                                      --Nested Definitions.-- 
                                                      concatenator = L.concat
                                                                     (simplifyruleloader
                                                                     (ruleloader
                                                                     (subrulecoupler
                                                                     (subruleregex
                                                                     (subruleadder
                                                                     (resolver
                                                                     (simplifypreresolver
                                                                     (preresolver
                                                                     (allgroup
                                                                     (finaldifferences
                                                                     (sortprerule
                                                                     (preparetorule
                                                                     (wordandprerule
                                                                     (finalsimplifytoprerule
                                                                     (simplifytoprerule
                                                                     (toprerule pfxsfxgenerator rulenamegenerator))))))))))))))))
                                                                         where
                                                                             --Nested Definitions.--
                                                                             pfxsfxgenerator = pfxsfx
                                                                                               (simplifypresufcheck
                                                                                               (presufcheck
                                                                                               (truefinalreader
                                                                                               (prefinalreader
                                                                                               (midfinalreader
                                                                                               (midreader
                                                                                               (beginreader
                                                                                               (readchunker
                                                                                               (linefeed fulltempread)))))))))
                                                                             rulenamegenerator = (sequence $ L.replicate 1 ['A'..'Z']) 
                                                                                              ++ (sequence $ L.replicate 2 ['A'..'Z']) 
                                                                                              ++ (sequence $ L.replicate 3 ['A'..'Z'])
                                                                             ----------------------- 
                                                      lengthmapper = simpruleloaderlengthmapper
                                                                     (simplifyruleloader
                                                                     (ruleloader
                                                                     (subrulecoupler
                                                                     (subruleregex
                                                                     (subruleadder
                                                                     (resolver
                                                                     (simplifypreresolver
                                                                     (preresolver
                                                                     (allgroup
                                                                     (finaldifferences
                                                                     (sortprerule
                                                                     (preparetorule
                                                                     (wordandprerule
                                                                     (finalsimplifytoprerule
                                                                     (simplifytoprerule
                                                                     (toprerule pfxsfxgenerator rulenamegenerator))))))))))))))))
                                                                         where
                                                                             --Nested Definitions.--
                                                                             pfxsfxgenerator = pfxsfx
                                                                                               (simplifypresufcheck
                                                                                               (presufcheck
                                                                                               (truefinalreader
                                                                                               (prefinalreader
                                                                                               (midfinalreader
                                                                                               (midreader
                                                                                               (beginreader
                                                                                               (readchunker
                                                                                               (linefeed fulltempread)))))))))
                                                                             rulenamegenerator = (sequence $ L.replicate 1 ['A'..'Z']) 
                                                                                              ++ (sequence $ L.replicate 2 ['A'..'Z']) 
                                                                                              ++ (sequence $ L.replicate 3 ['A'..'Z'])
                                                                             -----------------------
                                                      -----------------------
                                                      
                         --Print the result of finalaffixlist.
                         IO.writeFile "out.aff" $
                             (PB.render $
                             (PB.hsep 2 PB.left . L.map (PB.vcat PB.left) . L.map (L.map (PB.text)))
                             (L.transpose finalaffixlist))
                         
                         --Prepare to print out dictionary file with finaldictionarylist.
                         let finaldictionarylist = finalprintdictionarylist
                                                   (prefinalprintdictionarylist
                                                   (dictfiletostring
                                                   (dictfileheaderadder
                                                   (ruleroottodictfilebody
                                                   (rulerootfinal
                                                   (rulerootswapper
                                                   (rulerootsimplifier
                                                   (rulerootzipper
                                                   (rulerootaccumulator
                                                   (rulerootconcatenator
                                                   (subruleregex
                                                   (subruleadder
                                                   (resolver
                                                   (simplifypreresolver
                                                   (preresolver
                                                   (allgroup
                                                   (finaldifferences
                                                   (sortprerule
                                                   (preparetorule
                                                   (wordandprerule
                                                   (finalsimplifytoprerule
                                                   (simplifytoprerule
                                                   (toprerule pfxsfxgenerator rulenamegenerator)))))))))))))))))))))))
                                                       where
                                                           --Nested Definitions.--
                                                           pfxsfxgenerator = pfxsfx
                                                                             (simplifypresufcheck
                                                                             (presufcheck
                                                                             (truefinalreader
                                                                             (prefinalreader
                                                                             (midfinalreader
                                                                             (midreader
                                                                             (beginreader
                                                                             (readchunker
                                                                             (linefeed fulltempread)))))))))
                                                           rulenamegenerator = (sequence $ L.replicate 1 ['A'..'Z']) 
                                                                            ++ (sequence $ L.replicate 2 ['A'..'Z']) 
                                                                            ++ (sequence $ L.replicate 3 ['A'..'Z'])
                                                           ----------------------
                         
                         --Print the result of finaldictionarylist.
                         IO.writeFile "out.dic" $
                             (PB.render $
                             (PB.hsep 2 PB.left . L.map (PB.vcat PB.left) . L.map (L.map (PB.text)))
                             (L.transpose finaldictionarylist))

                         --Close temporary file.
                         hClose temph

                         --Delete temporary file.
                         removeFile tempfile      

        _ -> error "More than one directory provided."     

{----------------}
