
-- | The Find-S algorithm learns a maximally specific hypothesis from
--   the given set of training data.
--
--   We start with the most specific hypothesis, which is that no attribute
--   value is acceptable, then progressively weaken the hypothesis so that
--   it covers all positive training examples.
--
--   Limitations of the basic algorithm:
--    * Assumes the training data is noiseless.
--    * Does not use negative examples, which only works if data is noiseless.
--    * Returns the most specific hypothesis, we might not want that.
--
--   Limitations of the implementation:
--   * The arity of each instance must be the same.
--
--   Input format is a CSV file where the first column in each row gives
--   a boolean result and the rest give the other attributes for each
--   instance.
--
--   Example:
--
--   Enjoy,Sky,Air,Humidity,Wind,Water,Forecast
--   true,sunny,warm,normal,strong,warm,same
--   true,sunny,warm,high,strong,warm,same
--   false,rainy,cold,high,strong,warm,change
--   true,sunny,warm,high,strong,cool,change
--
import qualified Harvest.Concept.FindS  as H

import Data.Text                        (Text)
import qualified System.Environment     as System
import qualified Text.Comma             as Comma
import qualified Data.Text.IO           as T


main
 = do   args    <- System.getArgs
        case args of
         [fileName]     -> runMain fileName
         _              -> error "usage: harvest-finds FILE.csv"


runMain fileName
 = do   file    <- T.readFile fileName
        case Comma.comma file of
         Left err       -> error err
         Right (lHeader:lsRow) -> runFindS lsRow


runFindS :: [[Text]] -> IO ()
runFindS lsRow
 = print $ H.findS $ map slurpRow lsRow


slurpRow :: [Text] -> (H.Instance Text, Bool)
slurpRow txs
 = ( H.Instance $ tail txs
   , case head txs of
        "true"  -> True
        "false" -> False
        _       -> error "bad input data format")

