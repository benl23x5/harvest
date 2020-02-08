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
 $ harvest-finds data/cat-cat/watersport/watersport.csv
 Hypothesis [Exact "sunny",Exact "warm",Any,Exact "strong",Any,Any]
```

From Machine Learning, Tom M. Mitchell, 1997


## Perceptron Binary Classification

The Perceptron algorithm is the simplest linear model, where instances
are scored by taking the dot product of features and weights and adding
a bias value. A simple training approach where the model weights are updated
for every training example individually will converge if the examples
are linearly seperable

Classifiction of poisonous vs edible mushrooms.
```
$ harvest-perceptron -label-true p -iterations 20 data/cat-cat/mushrooms/mushrooms.csv
train instances = 6512
test  instances = 1612
 iter    total correct     ratio  | (T) total correct     ratio  | (F) total correct     ratio
   20     1612    1048  0.650124          790     679  0.859494          822     369  0.448905
   19     1612    1371  0.850496          790     782  0.989873          822     589  0.716545
   18     1612    1350  0.837469          790     790  1.000000          822     560  0.681265
...
    2     1612    1610  0.998759          790     789  0.998734          822     821  0.998783
    1     1612    1611  0.999380          790     789  0.998734          822     822  1.000000
    0     1612    1611  0.999380          790     789  0.998734          822     822  1.000000
```

From Machine Learning, Tom M. Mitchell, 1997
