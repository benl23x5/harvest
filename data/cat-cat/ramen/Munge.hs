
-- | Munge Ramen rating data to produce bag-of-words features for the name.
module Main where
import Data.List
import Data.Maybe
import qualified Data.Char              as Char
import Data.List.Extra
import Data.Text                        (Text)
import qualified System.Environment     as System
import qualified Text.Comma             as Comma
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Map               as Map
import qualified Data.Set               as Set

main :: IO ()
main
 = do   args <- System.getArgs
        case args of
         [fileName] -> runMain fileName
         _          -> error "usage: munge <FILE.csv>"

runMain fileName
 = do   file <- T.readFile fileName
        case Comma.comma file of
         Left err -> error err
         Right ls -> runMunge ls


runMunge :: [[Text]] -> IO ()
runMunge (lHeader : ls)
 = do
        let descWords (_sIx : _sManuf : sName : _)
                = T.words sName

        let keepChar c
                = c >= 'a' && c <= 'z'

        let ws  = filter (\w -> T.length w >= 2)
                $ filter (\w -> all keepChar $ T.unpack w)
                $ map (T.filter Char.isAlpha)
                $ map (T.pack . map Char.toLower . T.unpack)
                $ concat $ map descWords ls

        let mpWordFreq
                = Map.unionsWith (+)
                $ [Map.singleton w 1 | w <- ws ]

        let mpWordKeep
                = Map.filter (>= 10)
                $ mpWordFreq

        let ssWords = Map.keys mpWordKeep

        T.putStrLn
         $ T.intercalate ","
         $ ("quality"
                : "manufacturer" : "country" : "pack"
                : ["w_" <> w | w <- ssWords])

        mapM_ T.putStrLn $ mapMaybe (mungeLine ssWords) ls

        return ()


mungeLine :: [Text] -> [Text] -> Maybe Text
mungeLine ssWords (sIx : sManuf : sName : sPack : sCountry : sRating : _)
 | sRating /= "Unrated"
 = let
        fRating :: Float = read $ T.unpack sRating

        sQuality
                | fRating >= 3.5  = T.pack "good"
                | otherwise       = T.pack "bad"

        ssName  = Set.fromList
                $ map (T.filter Char.isAlpha)
                $ map (T.pack . map Char.toLower . T.unpack)
                $ T.words sName

   in   Just
         $ T.intercalate ","
         $ (sQuality
                : T.filter (/= ' ') sManuf
                : T.filter (/= ' ') sCountry
                : case sPack of
                   "" -> "unknown"
                   _  -> sPack
                : [ if Set.member w ssName
                        then "O"
                        else "-"
                  | w <- ssWords])

 | otherwise = Nothing