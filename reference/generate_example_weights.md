# Generate example product weight data

Convenience function to generate product weights (e.g., grams) for
illustrating the one-sample t-test.

## Usage

``` r
generate_example_weights(n = 30, mean = 500, sd = 5, seed = NULL)
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
