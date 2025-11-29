# Example analysis pipeline

This helper shows a simple workflow: generate data, run the one-sample
t-test, and return the test object. This can be used in demonstrations
and vignettes.

## Usage

``` r
example_one_mean_pipeline(
  n = 30,
  true_mean = 80,
  sd = 10,
  mu0 = 75,
  seed = NULL
)
```

## Arguments

- n:

  Sample size.

- true_mean:

  True mean for data generation.

- sd:

  True standard deviation for data generation.

- mu0:

  Hypothesized mean.

- seed:

  Optional integer seed.

## Value

An object of class `"oneMeanTest"`.
