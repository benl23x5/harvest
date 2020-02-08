
-- | FindS Concept Learning Algorithm.
--
--   From Machine Learning, Mitchell, 1997.
--
module Harvest.Concept.FindS
        ( Constraint    (..)
        , Hypothesis    (..)
        , Instance      (..)
        , findS)
where
import qualified Data.List      as L


-- | A constraint on a particular value.
data Constraint a
        = None          -- ^ No value is acceptable.
        | Exact a       -- ^ Need this exact value.
        | Any           -- ^ Any value is acceptable.
        deriving (Eq, Show)


-- | A hypothesis is a list of constraints.
data Hypothesis a
        = Hypothesis [Constraint a]
        deriving (Eq, Show)


-- | An instance.
data Instance a
        = Instance   [a]
        deriving (Eq, Show)


-- | Weaken a single constraint, given an example attribute.
weakenFromAttribute :: Eq a => Constraint a -> a -> Constraint a
weakenFromAttribute cc x
 = case cc of
        None            -> Exact x
        Exact x'
         | x == x'      -> Exact x
         | otherwise    -> Any
        Any             -> Any


-- | Weaken a hypothesis, given an example instance.
weakenFromInstance  :: Eq a => Hypothesis a -> Instance a  -> Hypothesis a
weakenFromInstance (Hypothesis cs) (Instance xs)
        = Hypothesis (zipWith weakenFromAttribute cs xs)


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
findS :: Eq a => [(Instance a, Bool)] -> Hypothesis a
findS train@(i0 : _)
 = let
        -- Get the arity of the first instance,
        -- and assume all the others have the same arity.
        (Instance as0, _) = i0
        arity           = length as0

        -- Start with the most specific hypothesis.
        hyp0            = Hypothesis (replicate arity None)

        -- We only use the positive training examples.
        instPos         = [ i | (i, b@True)     <- train]

        -- Successively refine the hypothesis using the training examples.
   in   L.foldl' weakenFromInstance hyp0 instPos

