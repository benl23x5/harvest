
module Config where
import Text.Read
import Data.Text                (Text)
import qualified Data.Text      as T


data Config
        = Config
        { configFile            :: Maybe FilePath
        , configLabelTrue       :: Text
        , configTestRatio       :: Float
        , configLearnRate       :: Float
        , configInitWeight      :: Float }
        deriving Show


configDefault :: Config
configDefault
        = Config
        { configFile            = Nothing
        , configLabelTrue       = "true"
        , configTestRatio       = 0.2
        , configLearnRate       = 0.00001
        , configInitWeight      = 0.001  }


parseArgs :: [String] -> Config -> IO Config

parseArgs [] config
 = return config

parseArgs ("-label-true" : sLabel : rest) config
 = parseArgs rest
 $ config { configLabelTrue = T.pack sLabel }

parseArgs ("-test-ratio" : sRatio : rest) config
 | Just fRatio <- readMaybe sRatio
 = parseArgs rest
 $ config { configTestRatio = fRatio }

parseArgs ("-init-weight" : sWeight : rest) config
 | Just fWeight <- readMaybe sWeight
 = parseArgs rest
 $ config { configInitWeight = fWeight }

parseArgs ("-learn-rate" : sRate : rest) config
 | Just fRate <- readMaybe sRate
 = parseArgs rest
 $ config { configLearnRate = fRate }

parseArgs (fileName@(c : _) : rest) config
 | c /= '-'
 = parseArgs rest
 $ config { configFile = Just fileName }

 | otherwise
 = error "usage"
