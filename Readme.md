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
