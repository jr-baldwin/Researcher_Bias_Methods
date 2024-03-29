Script to split dataset into training and holdout samples
================

**Randomly splitting a dataset into training and holdout samples is very simple to do in R, using the ["sample" function](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sample). Below we provide script to do so.**

1: Open toy data<a name="link1"></a>
====================================

We first open toy data. If you wish to subset your own dataset without using toy data, please skip to [Select subset of data at random](#link1) for the code.

``` r
library(psych)
```

``` r
data(iris)
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

``` r
str(iris)
```

    ## 'data.frame':    150 obs. of  5 variables:
    ##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
describe(iris)
```

    ##              vars   n mean   sd median trimmed  mad min max range  skew
    ## Sepal.Length    1 150 5.84 0.83   5.80    5.81 1.04 4.3 7.9   3.6  0.31
    ## Sepal.Width     2 150 3.06 0.44   3.00    3.04 0.44 2.0 4.4   2.4  0.31
    ## Petal.Length    3 150 3.76 1.77   4.35    3.76 1.85 1.0 6.9   5.9 -0.27
    ## Petal.Width     4 150 1.20 0.76   1.30    1.18 1.04 0.1 2.5   2.4 -0.10
    ## Species*        5 150 2.00 0.82   2.00    2.00 1.48 1.0 3.0   2.0  0.00
    ##              kurtosis   se
    ## Sepal.Length    -0.61 0.07
    ## Sepal.Width      0.14 0.04
    ## Petal.Length    -1.42 0.14
    ## Petal.Width     -1.36 0.06
    ## Species*        -1.52 0.07

2: Select subset of data at random<a name="link1"></a>
======================================================

``` r
subsample <- iris[sample(nrow(iris), 50), ] # select subset of 50 observations at random
describe(subsample)
```

    ##              vars  n mean   sd median trimmed  mad min max range  skew kurtosis
    ## Sepal.Length    1 50 5.86 0.86   5.80    5.84 1.04 4.3 7.7   3.4  0.10    -1.07
    ## Sepal.Width     2 50 3.07 0.43   3.00    3.05 0.44 2.3 4.2   1.9  0.31    -0.29
    ## Petal.Length    3 50 3.78 1.84   4.45    3.79 2.00 1.1 6.7   5.6 -0.26    -1.54
    ## Petal.Width     4 50 1.20 0.81   1.35    1.18 1.04 0.1 2.5   2.4 -0.06    -1.41
    ## Species*        5 50 2.02 0.84   2.00    2.02 1.48 1.0 3.0   2.0 -0.04    -1.63
    ##                se
    ## Sepal.Length 0.12
    ## Sepal.Width  0.06
    ## Petal.Length 0.26
    ## Petal.Width  0.11
    ## Species*     0.12
