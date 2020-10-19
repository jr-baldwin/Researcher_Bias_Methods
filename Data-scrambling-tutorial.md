Data Scrambling Tutorial
================
Jessie Baldwin

<br>

**This tutorial demonstrates how to use 'data scrambling' to blind your data, to allow proposed analyses to be trialled without insight into the findings. This involves randomly shuffling the data points so that any associations between variables are obscured, whilst the variable distributions (and amounts of missing data) remain the same. All analyses will be conducted within R.**

<br>

### Overview of tutorial

#### [1: Simulate data](#link1)

#### [2: Scramble data](#link2)

#### [3: Compare scrambled and non-scrambled data](#link3)

<br>

1: Simulate data<a name="link1"></a>
====================================

For the sake of this tutorial, we will first simulate data to be scrambled. If you wish to scramble your own dataset without following the simulation, please skip to [Scramble data](#link2) for the code.

The simulated dataset will comprise 1000 observations across 5 variables, 2 of which are the predictor and outcome variable for the regression of interest. This simulation was adapted from Prof. Dorothy Bishop's scripts which can be found at: <https://osf.io/gupxv/>

<br>

#### Install and load packages

Please first install and load the following packages.

``` r
install.packages("MASS")
install.packages("tidyr")
install.packages("dplyr")
install.packages("psych")
```

If you have already installed them, you can skip to loading them using the code below.

``` r
library(MASS)
library(tidyr)
library(dplyr)
library(psych)
```

<br>

#### Specify parameters for the simulated variables<a name="link4"></a>

Below we specify parameters for the simulation, including: - number of variables - sample size - mean value for the variables - variance of the variables - correlations between predictor variables - correlations between each predictor with the outcome

``` r
nVar <- 4 # Number of simulated variables (we will treat the 4th variable as the outcome)
N <- 1000 # Sample size for simulated dataset 
mean <- 0 # Mean score for all variables in the sample - we're using z scores for simplicity
var <- 1 # Variance of all variables. For z-scores, SD is 1, so variance is also 1
cor <- 0.3 # Correlation between predictor variables (this is arbitrary)
corDV <- c(0, 0, 0.2, 1) # Correlation between each predictor and the outcome. The final '1' is the variance of the outcome, in effect, the correlation of each variable with itself.
```

<br>

#### Generate a correlation matrix for the simulated variables

We will next generate a correlation matrix for the correlations between all the variables to be simulated.

``` r
covMatrix <- matrix(rep(cor, nVar*nVar), nrow=nVar) # create a matrix with correlation value 
# Now we change the final row and column to show the correlation with the outcome. 
covMatrix[nVar, ] <- corDV # note that we specify nVar to indicate last row, but leave blank after the comma: this means we want all columns
covMatrix[, nVar] <- corDV #note that we leave blank before comma to indicate we want all rows; nVar indicates we want last column
diag(covMatrix) <- var # Change diagonal corresponding to the variance of all variables
covMatrix
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]  1.0  0.3  0.3  0.0
    ## [2,]  0.3  1.0  0.3  0.0
    ## [3,]  0.3  0.3  1.0  0.2
    ## [4,]  0.0  0.0  0.2  1.0

<br>

#### Generate simulated data from specified means, variances, and covariance matrix

We will now simulate the dataset from the specified parameters.

``` r
set.seed(109) # Set seed for reproducibility
sim_data <- data.frame(id=1:N, mvrnorm(n = N, rep(mean, nVar), covMatrix)) # Include ID variable
head(sim_data)
```

    ##   id         X1         X2         X3         X4
    ## 1  1 -0.6557270 -1.6339240 -1.9669352 -0.3188175
    ## 2  2  0.0942917  1.7150251  0.6706405  0.9638397
    ## 3  3  1.5248561 -0.8092098 -0.3095560  0.5415261
    ## 4  4  1.5527085  1.1954027  1.7609831  0.8254483
    ## 5  5 -0.6993125 -1.9994158  0.2052681  0.8088263
    ## 6  6 -0.2275366  0.4815002 -0.3813776  0.2502864

``` r
dim(sim_data)
```

    ## [1] 1000    5

We can see that the simulated data includes an ID variable and 4 other variables, with 1000 rows.

<br>

#### Generate some missing data in the simulated dataset

We will now convert some of the simulated data to missing. This in order to test whether data scrambling affects the amount of missing data in a variable.

``` r
# Specify variables X3 and X4 to have missing data 
c_names <- c("X3","X4")

# Specify 20% missing data 
prc_missing <- 0.20
n_remove <- prc_missing * nrow(sim_data)

# Input missing data into simulated dataset
sim_data <- sim_data %>%
  gather(var, value, -id) %>%   # reshape data
  sample_frac(1) %>%            # shuffle rows
  group_by(var) %>%             # for each variables
  mutate(value = ifelse(var %in% c_names & row_number() <= n_remove, NA, value)) %>%  # update to NA top x number of rows if it's one of the variables you specified
  spread(var, value)   %>%         # reshape to original format
  as.data.frame()
```

<br>

#### Describe simulated dataset

Let's have a look at the simulated dataset and run some descriptives.

``` r
head(sim_data)
```

    ##   id         X1         X2         X3         X4
    ## 1  1 -0.6557270 -1.6339240 -1.9669352 -0.3188175
    ## 2  2  0.0942917  1.7150251  0.6706405         NA
    ## 3  3  1.5248561 -0.8092098 -0.3095560  0.5415261
    ## 4  4  1.5527085  1.1954027  1.7609831  0.8254483
    ## 5  5 -0.6993125 -1.9994158  0.2052681         NA
    ## 6  6 -0.2275366  0.4815002         NA  0.2502864

``` r
tail(sim_data)
```

    ##        id         X1          X2          X3          X4
    ## 995   995 -1.0688127 -1.28160505 -0.17294415  0.16080481
    ## 996   996 -0.1228097 -0.06786033          NA  0.02149524
    ## 997   997 -0.1310938  1.59700823  0.01018438 -1.35181326
    ## 998   998  0.6414681 -0.01558887 -0.08993011  0.42148876
    ## 999   999  1.9566076  0.26041581  0.29666757  0.26124357
    ## 1000 1000 -1.0790501 -0.98511085 -0.81549538  0.65391436

``` r
dim(sim_data)
```

    ## [1] 1000    5

``` r
describe(sim_data)
```

    ##    vars    n   mean     sd median trimmed    mad   min     max  range  skew
    ## id    1 1000 500.50 288.82 500.50  500.50 370.65  1.00 1000.00 999.00  0.00
    ## X1    2 1000  -0.02   0.99  -0.05   -0.03   0.99 -3.00    3.08   6.07  0.04
    ## X2    3 1000   0.01   1.02   0.00    0.01   1.04 -3.09    3.62   6.71  0.04
    ## X3    4  800   0.01   0.98  -0.01    0.01   0.95 -3.10    2.77   5.87 -0.01
    ## X4    5  800  -0.02   1.00  -0.05   -0.03   1.01 -3.36    2.93   6.29  0.02
    ##    kurtosis   se
    ## id    -1.20 9.13
    ## X1    -0.02 0.03
    ## X2    -0.04 0.03
    ## X3    -0.03 0.03
    ## X4     0.07 0.04

This shows that:

-   There are 1000 rows and 5 variables in the dataset
-   Variables X3 and X4 have 20% missing data (N=800 instead of N=1000 in the other variables).
-   The means of X1-X4 are close to 0 and the standard deviations are close to 1 as we [specified above](#link4).

<br>

#### Run regression testing the association between two variables of interest.

We will now test the association between X3 and X4, the variables we simulated to have a correlation of r=0.2.

``` r
reg_unscrambled <- lm(X4 ~ X3, data=sim_data)
summary(reg_unscrambled)
```

    ## 
    ## Call:
    ## lm(formula = X4 ~ X3, data = sim_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.93497 -0.66987 -0.06113  0.67788  2.72969 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.03434    0.03867  -0.888    0.375    
    ## X3           0.25893    0.03956   6.546 1.21e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9841 on 646 degrees of freedom
    ##   (352 observations deleted due to missingness)
    ## Multiple R-squared:  0.06221,    Adjusted R-squared:  0.06075 
    ## F-statistic: 42.85 on 1 and 646 DF,  p-value: 1.205e-10

We can see a statistically significant association between X3 and X4 (beta=0.26, SE=0.04, p=0.00).

<br>

2: Scramble data<a name="link2"></a>
====================================

Scrambling data (also known as data shuffling) is surprisingly easy. It can be achieved using the 'sample' function.

Below we scramble data for each row individually (meaning that all relationships between variables will be obscured). We do so following the advice in [this Stack Overflow post](https://stackoverflow.com/questions/32620534/reshuffle-the-sequence-of-rows-in-data-frame).

``` r
scrambled_data <- as.data.frame(lapply(sim_data, function(x) { sample(x)}))
```

<br>

3: Compare scrambled and non-scrambled data<a name="link3"></a>
===============================================================

Let's have a look at the data before and after scrambling, ordering by ID. In practice, the person who scrambled the data (not the lead researcher) should do this as a check that the scrambling has worked.

<br>

#### Check data structure of the original dataset

``` r
head(sim_data[sim_data$id<10,], 10) 
```

    ##   id          X1         X2         X3          X4
    ## 1  1 -0.65572703 -1.6339240 -1.9669352 -0.31881750
    ## 2  2  0.09429170  1.7150251  0.6706405          NA
    ## 3  3  1.52485610 -0.8092098 -0.3095560  0.54152612
    ## 4  4  1.55270852  1.1954027  1.7609831  0.82544832
    ## 5  5 -0.69931253 -1.9994158  0.2052681          NA
    ## 6  6 -0.22753658  0.4815002         NA  0.25028644
    ## 7  7  1.48572883 -0.6010970  0.1955811  0.15540458
    ## 8  8  0.41043406 -1.0869932  1.0840443 -0.03882667
    ## 9  9 -0.01786195  0.3777352         NA          NA

#### Check data structure of the scrambled dataset

``` r
head(scrambled_data[order(scrambled_data$id),], 10)
```

    ##     id         X1          X2         X3         X4
    ## 956  1 -0.8347911 -0.35820323  1.5484421 -0.0840755
    ## 640  2  0.5165052  0.67014903         NA -0.7600910
    ## 8    3 -1.7192140 -0.06166686 -0.9994966  0.9239226
    ## 507  4 -0.3859897 -1.19137645         NA -0.8759332
    ## 610  5 -0.2218472  1.32979537 -1.0971181  0.7083013
    ## 808  6  0.3758261  0.38747458  1.0000217 -0.5067124
    ## 57   7 -2.7092425  2.13927050  0.1471600         NA
    ## 100  8  0.3043861 -0.22153880 -0.5034120         NA
    ## 428  9 -0.8717108 -0.80869105         NA  0.3829177
    ## 252 10  0.2963366 -0.03873572 -0.4066575 -0.3164592

We can see that the values differ in the scrambled dataset, including the cells with missing data.

<br>

#### Check descriptives in the original dataset

``` r
describe(sim_data) 
```

    ##    vars    n   mean     sd median trimmed    mad   min     max  range  skew
    ## id    1 1000 500.50 288.82 500.50  500.50 370.65  1.00 1000.00 999.00  0.00
    ## X1    2 1000  -0.02   0.99  -0.05   -0.03   0.99 -3.00    3.08   6.07  0.04
    ## X2    3 1000   0.01   1.02   0.00    0.01   1.04 -3.09    3.62   6.71  0.04
    ## X3    4  800   0.01   0.98  -0.01    0.01   0.95 -3.10    2.77   5.87 -0.01
    ## X4    5  800  -0.02   1.00  -0.05   -0.03   1.01 -3.36    2.93   6.29  0.02
    ##    kurtosis   se
    ## id    -1.20 9.13
    ## X1    -0.02 0.03
    ## X2    -0.04 0.03
    ## X3    -0.03 0.03
    ## X4     0.07 0.04

#### Check descriptives in the scrambled dataset

``` r
describe(scrambled_data) # Scrambled dataset
```

    ##    vars    n   mean     sd median trimmed    mad   min     max  range  skew
    ## id    1 1000 500.50 288.82 500.50  500.50 370.65  1.00 1000.00 999.00  0.00
    ## X1    2 1000  -0.02   0.99  -0.05   -0.03   0.99 -3.00    3.08   6.07  0.04
    ## X2    3 1000   0.01   1.02   0.00    0.01   1.04 -3.09    3.62   6.71  0.04
    ## X3    4  800   0.01   0.98  -0.01    0.01   0.95 -3.10    2.77   5.87 -0.01
    ## X4    5  800  -0.02   1.00  -0.05   -0.03   1.01 -3.36    2.93   6.29  0.02
    ##    kurtosis   se
    ## id    -1.20 9.13
    ## X1    -0.02 0.03
    ## X2    -0.04 0.03
    ## X3    -0.03 0.03
    ## X4     0.07 0.04

We can see that the descriptives for all variables are exactly the same before and after scrambling. This means that the scrambled dataset is informative about the distributions and amounts of missingness in the data.

<br>

#### Run regression of interest in the scrambled dataset

We will now test the association between X4 and X3 in the scrambled dataset

``` r
reg_scrambled <- lm(X4 ~ X3, data=scrambled_data)
summary(reg_scrambled)
```

    ## 
    ## Call:
    ## lm(formula = X4 ~ X3, data = scrambled_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.05240 -0.69170 -0.02893  0.68441  2.97544 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -0.04012    0.03935  -1.019    0.308
    ## X3          -0.02944    0.03898  -0.755    0.450
    ## 
    ## Residual standard error: 0.9994 on 643 degrees of freedom
    ##   (355 observations deleted due to missingness)
    ## Multiple R-squared:  0.0008865,  Adjusted R-squared:  -0.0006674 
    ## F-statistic: 0.5705 on 1 and 643 DF,  p-value: 0.4503

In the scrambled dataset, there is no association between X4 and X3 (beta= -0.029, SE=0.039, p=0.45). Let's compare this to the true association in the unscrambled dataset.

#### Run regression of interest in the original dataset

``` r
summary(reg_unscrambled)
```

    ## 
    ## Call:
    ## lm(formula = X4 ~ X3, data = sim_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.93497 -0.66987 -0.06113  0.67788  2.72969 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.03434    0.03867  -0.888    0.375    
    ## X3           0.25893    0.03956   6.546 1.21e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9841 on 646 degrees of freedom
    ##   (352 observations deleted due to missingness)
    ## Multiple R-squared:  0.06221,    Adjusted R-squared:  0.06075 
    ## F-statistic: 42.85 on 1 and 646 DF,  p-value: 1.205e-10

The true statistically significant association (beta=0.26, SE=0.04) is present in the original dataset, but not in the scrambled dataset.

This means that the scrambling procedure has obscured the significant association between variables, whilst retaining the distribution of the variables and amounts of missingness.
