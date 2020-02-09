
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
 = do   file_    <- T.readFile fileName
        let file =  T.filter (/= '\r') file_
        case Comma.comma file of
         Left err  -> error err
         Right ls  -> runClassify config ls


---------------------------------------------------------------------------------------------------
-- Run the classifier.
runClassify
        :: Config       -- ^ Configuration.
        -> [[Text]]     -- ^ Rows from the CSV file, first row is header.
        -> IO ()

runClassify config lsRows
 = do
        let sLabelTrue  = configLabelTrue config
        let fTestRatio  = configTestRatio config

        -- Split off header row which gives attribute names.
        let lAttrs : lsInstances = lsRows

        -- Guess the sorts of each feature based on the values.
        let lsCols = transpose lsInstances
        let _stClass : stFeatureSorts = map H.guessSortOfFeature lsCols

        -- Build list of all example instances.
        let instsAll
                = [ H.loadInstance
                        stFeatureSorts ssFeatureNames ssFeatureValues
                        (sClassValue == sLabelTrue)
                   | lsValues <- lsInstances
                   , let sClassName  : ssFeatureNames  = lAttrs
                   , let sClassValue : ssFeatureValues = lsValues ]

        -- Write out feature names if we were asked.
        let ssFeaturesCat = Set.unions $ map H.instanceCat instsAll
        let ssFeaturesCon = Set.unions $ map (Set.fromList . Map.keys . H.instanceCon) instsAll
        let ssFeatures    = Set.union ssFeaturesCat ssFeaturesCon
        (case configOutFeatures config of
          Nothing       -> return ()
          Just filePath -> T.writeFile filePath $ H.showFeaturesOfInstances instsAll)

        -- Split the example instances into the training and testing sets.
        let (instsTest, instsTrain)
                = H.splitRatio 23 fTestRatio instsAll

        putStrLn $ printf "* total examples = %d" (length instsAll)
        putStrLn $ printf "* train examples = %d" (length instsTrain)
        putStrLn $ printf "* test  examples = %d" (length instsTest)

        -- Initialize the model to have small random weights.
        let modelInit
                = H.initModel 42
                        (configInitWeight config)
                        ssFeaturesCat ssFeaturesCon

        -- Enter the training loop.
        T.putStrLn ""
        T.putStrLn $ " iter" <> H.scoreMetricsHeader

        (modelFinal, scoresFinal)
         <- loopTrain config
                instsTrain instsTest modelInit

        (case configOutModel config of
          Nothing   -> return ()
          Just path -> T.writeFile path $ H.showModel modelFinal)

        (case configOutScores config of
          Nothing   -> return ()
          Just path -> T.writeFile path
                     $ T.unlines $ map H.showExampleScore $ scoresFinal)

        return ()

---------------------------------------------------------------------------------------------------
-- | Train an initial model.
--     At each step we update the model using all the training instances,
--     and print out the current performance on the test instances.
loopTrain
        :: Config
        -> [H.Instance] -- ^ Instances to use for training.
        -> [H.Instance] -- ^ Instances to use for testing.
        -> H.Model      -- ^ Current model.
        -> IO (H.Model, [H.ExampleScore])

loopTrain config instsTrain instsTest model_
 = loop 0 model_
 where
  loop !i !model
   = do -- Write out the intermediate model, if we were asked for it.
        (case configOutModelPath config of
          Nothing   -> return ()
          Just path -> T.writeFile (printf (path ++ "/model-%06d.txt") i)
                     $ H.showModel model)

        -- Score the test instances using the current model and write them out.
        let exScores = map (H.makeExampleScore model) instsTest
        (case configOutScoresPath config of
          Nothing   -> return ()
          Just path -> T.writeFile (printf (path ++ "/scores-%06d.txt") i)
                     $ T.unlines $ map H.showExampleScore $ exScores)

        -- Print score metrics to console.
        let metrics    = H.takeScoreMetrics exScores
        let iItersLeft = configIterations config - i
        T.putStrLn   $  (T.pack $ printf "% 5d" iItersLeft)
                     <> H.showScoreMetrics metrics

        -- Update the model.
        let model'   = foldl'   (H.updateModel (configLearnRate config))
                                model instsTrain

        if iItersLeft == 0
         then return (model', exScores)
         else loop (i + 1) model'
