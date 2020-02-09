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
$ harvest-perceptron -label-true p \
        -iterations 20 -learn-rate 0.00001 -test-ratio 0.2 \
        -out-features output/_features.ssv \
        -out-model    output/_model.ssv \
        -out-scores   output/_scores.ssv \
        data/cat-cat/mushrooms/mushrooms.csv

* total examples = 8124
* train examples = 6512
* test  examples = 1612
 iter    total correct     ratio  | (T) total correct     ratio  | (F) total correct     ratio
   20     1612     941  0.583747          790     753  0.953165          822     188  0.228710
   19     1612    1471  0.912531          790     754  0.954430          822     717  0.872263
   18     1612    1455  0.902606          790     778  0.984810          822     677  0.823601
...
    2     1612    1602  0.993796          790     790  1.000000          822     812  0.987835
    1     1612    1607  0.996898          790     790  1.000000          822     817  0.993917
    0     1612    1612  1.000000          790     790  1.000000          822     822  1.000000
```

From Machine Learning, Tom M. Mitchell, 1997
