# Perceptron Binary Classification

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

Some fish are either to identify than others.
```
$ harvest-perceptron -label-true Bream \
        -iterations 10000 -learn-rate 0.000001 -test-ratio 0.5 \
        -out-features output/_features.ssv \
        -out-model    output/_model.ssv \
        -out-scores   output/_scores.ssv \
        data/cat-con/fishmarket/fishmarket.csv
...
    2       81      78  0.962963           13      13  1.000000           68      65  0.955882
    1       81      78  0.962963           13      13  1.000000           68      65  0.955882
    0       81      78  0.962963           13      13  1.000000           68      65  0.955882
```

```
$ harvest-perceptron -label-true Perch \
        -iterations 10000 -learn-rate 0.000001 -test-ratio 0.5 \
        -out-features output/_features.ssv \
        -out-model    output/_model.ssv \
        -out-scores   output/_scores.ssv \
        data/cat-con/fishmarket/fishmarket.csv
...
    4       81      56  0.691358           31       7  0.225806           50      49  0.980000
    3       81      60  0.740741           31      23  0.741936           50      37  0.740000
    2       81      50  0.617284           31       1  0.032258           50      49  0.980000
    1       81      59  0.728395           31      21  0.677419           50      38  0.760000
    0       81      55  0.679012           31      30  0.967742           50      25  0.500000
```
From Machine Learning, Tom M. Mitchell, 1997
