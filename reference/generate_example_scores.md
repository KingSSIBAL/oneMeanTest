# Generate example test score data

Convenience function to generate a numeric vector of test scores for
illustrating the one-sample t-test.

## Usage

``` r
generate_example_scores(n = 30, mean = 80, sd = 10, seed = NULL)
```

## Arguments

- n:

  Sample size.

- mean:

  True mean used for simulation.

- sd:

  True standard deviation used for simulation.

- seed:

  Optional integer seed for reproducibility.

## Value

A numeric vector of length `n`.
