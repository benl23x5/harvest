
-- | Use of a Perceptron for binary classification of categorical data.
--     Files of current model and scores are written to "./output"
import qualified Harvest.Neural.Perceptron as H
import Config

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
 = do   args   <- System.getArgs
        config <- parseArgs args configDefault

        case configFile config of
         Just fileName -> runMain config fileName
         _ -> error "usage: harvest-perceptron [FLAGS] FILE.csv"

runMain config fileName
 = do   file    <- T.readFile fileName
        case Comma.comma file of
         Left err  -> error err
         Right ls  -> runClassify config ls


---------------------------------------------------------------------------------------------------
-- Run the classifier.
runClassify
        :: Config
        -> [[Text]]     -- ^ Rows from the CSV file, first row is header.
        -> IO ()

runClassify config lsRows
 = do
        let sLabelTrue  = configLabelTrue config
        let fTestRatio  = configTestRatio config

        -- Split off header row which gives attribute names.
        let lAttrs : lsInstances = lsRows

        -- Build list of all instances,
        --  where the category and features are named like
        let insts = [ H.loadInstance
                        ssFeatureNames ssFeatureValues
                        (sClass == sLabelTrue)
                    | lsValues <- lsInstances
                    , let sClassName : ssFeatureNames  = lAttrs
                    , let sClass     : ssFeatureValues = lsValues ]

        -- Collect the set of all features and write it out.
        let ssFeatures = Set.unions $ map H.instanceFeatures insts
        T.writeFile "output/features.csv"
         $ T.unlines
         $ ("feature" : Set.toList ssFeatures)

        -- Split the example instances into the training and holdout sets
        let (instsTest, instsTrain)
                = H.splitRatio 23 fTestRatio insts

        putStrLn $ printf "train instances = %d" (length instsTrain)
        putStrLn $ printf "test  instances = %d" (length instsTest)


        -- Initialize the model and write it out.
        let modelInit = H.initModel 42 (configInitWeight config) ssFeatures
        T.writeFile "output/model-0-init.txt"
         $ H.showModel modelInit

        -- Enter the training loop.
        T.putStrLn H.scoreMetricsHeader
        loopTrain
                (configLearnRate config)
                20
                instsTrain instsTest modelInit


---------------------------------------------------------------------------------------------------
-- | Train an initial model.
--     At each step we update the model using all the training instances,
--     and print out the current performance on the test instances.
loopTrain
        :: Float        -- ^ Learning rate
        -> Int          -- ^ Number of training iterations.
        -> [H.Instance] -- ^ Instances to use for training.
        -> [H.Instance] -- ^ Instances to use for testing.
        -> H.Model      -- ^ Current model.
        -> IO ()

loopTrain fLearnRate iIterMax instsTrain instsTest model_
 = loop 0 model_
 where
  loop !i !model
   | i > iIterMax = return ()
   | otherwise
   = do
        -- Write out the current model.
        T.writeFile (printf "output/model-%04d.txt" i)
         $ H.showModel model

        -- Score the holdout instances using the current model and write them out.
        let exScores = map (H.makeExampleScore model) instsTest
        T.writeFile (printf "output/score-%04d.txt" i)
         $ T.unlines $ map H.showExampleScore $ exScores

        -- Print score metrics to console.
        let metrics  = H.takeScoreMetrics exScores
        T.putStrLn $ H.showScoreMetrics metrics

        -- Update the model.
        let model' = foldl' (H.updateModel fLearnRate) model instsTrain
        loop (i + 1) model'
