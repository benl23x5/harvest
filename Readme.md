# Harvest

Simple implementations of a variety of Machine Learning algorithms,
along with command-line tools to exercise them on human readable input files.

These were implemented as vehicles for understanding the algorithms,
so no promises about performance on production data volumes.

## FindS

The Find-S algorithm learns a maximally specific hypothesis from the given
set of training data. We start with the most specific hypothesis, which is
that no attribute value is acceptable, then progressively weaken the
hypothesis so that it covers all positive training examples.

```
 $ harvest-finds data/cat-cat/watersport.csv
 Hypothesis [Exact "sunny",Exact "warm",Any,Exact "strong",Any,Any]
```

From Machine Learning, Tom M. Mitchell, 1997


## Perceptron Binary Classification

The Perceptron algorithm is the simplest linear model, where instances
are scored by taking the dot product of features and weights and adding
a bias value. A simple training approach will converge for linearly seperable data.

Classifiction of poisonous vs edible mushrooms.
```
$ harvest-perceptron p 0.8 data/cat-cat/mushrooms.csv
train instances = 1637
test  instances = 6487
(T) total correct     ratio  | (F) total correct     ratio
     3141    1452  0.462273         3346     941  0.281231
     3141    3079  0.980261         3346    3094  0.924686
     3141    3135  0.998090         3346    2588  0.773461
...
     3141    3127  0.995543         3346    3330  0.995218
     3141    3127  0.995543         3346    3330  0.995218
     3141    3127  0.995543         3346    3330  0.995218
```

From Machine Learning, Tom M. Mitchell, 1997
