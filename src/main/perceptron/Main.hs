
-- | Perceptron classifier.
import qualified Harvest.Neural.Perceptron as H

import Data.Text                        (Text)
import Data.Set                         (Set)
import Data.Map                         (Map)
import Data.List
import Data.List.Extra
import Text.Printf
import qualified System.Environment     as System
import qualified Text.Comma             as Comma
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Set               as Set
import qualified Data.Map.Strict        as Map
import qualified System.Random          as Random


---------------------------------------------------------------------------------------------------
main
 = do   args <- System.getArgs
        case args of
         [fileName]     -> runMain fileName
         _              -> error "usage: harvest-perceptron FILE.csv"

runMain fileName
 = do   file    <- T.readFile fileName
        case Comma.comma file of
         Left err  -> error err
         Right ls  -> runCategorize ls


---------------------------------------------------------------------------------------------------
runCategorize :: [[Text]] -> IO ()
runCategorize lsRows
 = do
        -- Split off header row which gives attribute names.
        let lAttrs : lsInstances = lsRows

        -- Build list of all instances,
        --  where the category and features are named like
        let insts = [ H.loadInstance lAttrs ssFeatures (sClass == "e")
                    | lsValues <- lsInstances
                    , let sClass : ssFeatures = lsValues ]

        -- Collect the set of all features and write it out.
        let ssFeatures = Set.unions $ map H.instanceFeatures insts
        T.writeFile "output/features.csv"
         $ T.unlines
         $ ("feature" : Set.toList ssFeatures)

        -- Initialize the model and write it out.
        let modelInit = H.initModel 42 ssFeatures
        T.writeFile "output/model-0-init.txt"
         $ H.showModel modelInit

        -- Enter the training loop.
        T.putStrLn H.scoreMetricsHeader
        loopTrain 0 20 insts modelInit


---------------------------------------------------------------------------------------------------
loopTrain iIter iIterMax insts model
 | iIter > iIterMax = return ()
 | otherwise
 = do
        -- Write out the current model.
        T.writeFile (printf "output/model-%04d.txt" (iIter :: Int))
         $ H.showModel model

        -- Score all the instances using the current model and write them out.
        let exScores = map (H.makeExampleScore model) insts

        -- Score the examples and write it out.
        T.writeFile (printf "output/score-%04d.txt" (iIter :: Int))
         $ T.unlines $ map H.showExampleScore $ exScores

        -- Print score metrics to console.
        let metrics  = H.takeScoreMetrics exScores
        T.putStrLn $ H.showScoreMetrics metrics

        -- Update the model.
        let model' = foldl' H.updateModel model insts
        loopTrain (iIter + 1) iIterMax insts model'

