
module Harvest.Neural.Perceptron where
import Data.Text                        (Text)
import Data.Set                         (Set)
import Data.Map                         (Map)
import Data.Either
import Data.List
import Data.List.Extra
import Text.Printf
import qualified System.Environment     as System
import qualified System.Random          as Random
import qualified Text.Comma             as Comma
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.IO           as T


---------------------------------------------------------------------------------------------------
-- | Data instance where boolean features in the set are true,
--   and all others are assumed to be false.
data Instance
        = Instance
        { instanceClass         :: !Bool
        , instanceFeatures      :: !(Set Text) }
        deriving Show


-- | Load instance data.
loadInstance
        :: [Text]       -- ^ Names of all attributes.
        -> [Text]       -- ^ Values for each attribute.
        -> Bool         -- ^ Label classification.
        -> Instance

loadInstance ssNames ssValue bClass
 = Instance bClass
        $ Set.fromList
        [ sName <> "=" <> sValue
        | sName  <- ssNames
        | sValue <- ssValue ]


-- | Show an `Instance`.
showInstance :: Instance -> Text
showInstance (Instance bClass stFeatures)
 =  (T.pack $ show bClass)
 <> " | "
 <> T.intercalate " " (Set.toList stFeatures)


-- | Split a list of things based on the given ratio.
splitRatio :: Int -> Float -> [a] -> ([a], [a])
splitRatio iSeed fRatio xs
 = let  rgen    = Random.mkStdGen iSeed

        rs :: [Float]
                = take (length xs)
                $ Random.randomRs (0, 1) rgen

        es      = zipWith (\r x -> if r <= fRatio then Left x else Right x) rs xs
   in   partitionEithers es


---------------------------------------------------------------------------------------------------
-- | Model has a bias and map of feature name to weight.
--   To evaluate the model we sum the bias with the
--   dot product of features and weights.
data Model
        = Model
        { modelBias             :: !Float
        , modelWeights          :: !(Map Text Float) }
        deriving Show


-- | Initialize the model with small random bias and weights.
initModel
        :: Int          -- ^ Random seed.
        -> Set Text     -- ^ Set of all feature values.
        -> Model        -- ^ Initial model with small values for weights.

initModel iSeed ssFeatures
 = let  rgen      = Random.mkStdGen iSeed
        nFeatures = Set.size ssFeatures

        fBias : fsWeights
         = take (1 + nFeatures)
         $ Random.randomRs (-0.01, 0.01) rgen

        mpWeights = Map.fromList $ zip (Set.toList ssFeatures) fsWeights

  in    Model   { modelBias     = fBias
                , modelWeights  = mpWeights }


-- | Score a data instance using the given model.
scoreInstance :: Model -> Instance -> Float
scoreInstance (Model fBias mpWeights)
              (Instance sClass stFeatures)
 = fBias
 + sum [ fWeight * indicate (Set.member sFeature stFeatures)
       | (sFeature, fWeight) <- Map.toList mpWeights ]


-- | Classify an instance using the given model.
classifyInstance :: Model -> Instance -> Bool
classifyInstance model inst
 = scoreInstance model inst > 0


-- | Update the model based on the given instance.
updateModel :: Model -> Instance -> Model
updateModel
        model@(Model fBias mpWeights)
         inst@(Instance bClass stFeatures)
 = let
        -- The target score for this instance.
        fTarget = indicate bClass

        -- The actual score for this instance.
        fActual = indicate $ classifyInstance model inst

        -- Difference between target and actual score.
        fDiff   = fTarget - fActual

        -- Learning rate.
        fRate   = 0.000001

        -- Update the bias.
        fBias'  = fBias + fRate * fDiff * 1

        -- Update the feature weights.
        mpWeights'
         = flip Map.mapWithKey mpWeights
         $ \k w -> let xi = if Set.member k stFeatures then 1 else -1
                       wd = fRate * fDiff * xi
                   in  w  + wd

   in   Model fBias' mpWeights'


-- | Show a `Model`.
showModel :: Model -> Text
showModel (Model fBias mpWeights)
 = T.pack $ unlines
 $ (  printf "%-32s" ("bias" :: Text)
   <> printf "% 14.12f" fBias )
 : ""
 : [  printf "%-32s" sFeature
   <> printf "% 14.12f" fWeight
   | (sFeature, fWeight) <- Map.toList mpWeights ]


---------------------------------------------------------------------------------------------------
-- | Packages a known example, with its score result.
data ExampleScore
         = ExampleScore
         { exampleLabel         :: !Bool
         , exampleScore         :: !Float
         , exampleCorrect       :: !Bool }
         deriving Show


-- | Score an example and check if the prediction was correct.
makeExampleScore :: Model -> Instance -> ExampleScore
makeExampleScore model inst@(Instance bClass _fs)
 = let  fScore  = scoreInstance model inst

        bCorrect
         | bClass,       fScore > 0 = True
         | not (bClass), fScore < 0 = True
         | otherwise = False

   in   ExampleScore
        { exampleLabel          = bClass
        , exampleScore          = fScore
        , exampleCorrect        = bCorrect }


-- | Show an `ExampleScore`.
showExampleScore :: ExampleScore -> Text
showExampleScore (ExampleScore bClass fScore bCorrect)
 =  T.pack
 $  printf "%-8s  "    (show bClass)
 <> printf "% 14.12f  " fScore
 <> printf "%-8s  "    (show bCorrect)


---------------------------------------------------------------------------------------------------
-- | Holds quality metrics we get from scoring our examples.
data ScoreMetrics
        = ScoreMetrics
        { scoreTotal            :: Int
        , scoreCorrect          :: Int
        , scoreTrueTotal        :: Int
        , scoreTrueCorrect      :: Int
        , scoreFalseTotal       :: Int
        , scoreFalseCorrect     :: Int }
        deriving Show


-- | Take metrics from some scored examples.
takeScoreMetrics :: [ExampleScore] -> ScoreMetrics
takeScoreMetrics ems
 = let  emsTrue         = filter exampleLabel ems
        emsFalse        = filter (not . exampleLabel) ems
   in   ScoreMetrics
        { scoreTotal            = length ems
        , scoreCorrect          = length $ filter exampleCorrect ems
        , scoreTrueTotal        = length $ emsTrue
        , scoreTrueCorrect      = length $ filter exampleCorrect emsTrue
        , scoreFalseTotal       = length $ emsFalse
        , scoreFalseCorrect     = length $ filter exampleCorrect emsFalse }


-- | Header to use for rows produced by showScoreMetrics
scoreMetricsHeader :: Text
scoreMetricsHeader
 = T.pack
 $ "    total correct     ratio  | (T) total correct     ratio  | (F) total correct     ratio"


-- | Show a `ScoreMetrics`.
showScoreMetrics :: ScoreMetrics -> Text
showScoreMetrics
        (ScoreMetrics
                iTotal iCorrect
                iTrueTotal iTrueCorrect
                iFalseTotal iFalseCorrect)
 = T.pack
 $ printf "   %6d  %6d  %7.6f       %6d  %6d  %7.6f       %6d  %6d  %7.6f"
        iTotal iCorrect fRatio
        iTrueTotal  iTrueCorrect  fRatioTrue
        iFalseTotal iFalseCorrect fRatioFalse
 where
        fRatio      :: Float = fromIntegral iCorrect      / fromIntegral iTotal
        fRatioTrue  :: Float = fromIntegral iTrueCorrect  / fromIntegral iTrueTotal
        fRatioFalse :: Float = fromIntegral iFalseCorrect / fromIntegral iFalseTotal


---------------------------------------------------------------------------------------------------
-- | Indicate whether a boolean value is true with +1 / -1.
indicate :: Bool -> Float
indicate True  = 1
indicate False = -1


