
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/softloud/metasim.svg?branch=master)](https://travis-ci.org/softloud/metasim)

# metasim

The goal of metasim is to simulate meta-analysis data.

I found I was rewriting the same types of analyses. I got to thinking
how to make a modular set of tools for simulating meta-anlaysis data.

In particular, I’m interested in simulating for different values of

  - \(k\), number of studies
  - \(\tau^2\), variation between studies
  - \(\varepsilon^2\), variation within a study
  - numbers of trials, say 10, 100, 1000
  - distributions, *and* parameters; e.g., \(\exp(\lambda = 1)\) and
    \(\exp(\lambda = 2)\).

## work in progress

This package is a work in progress, can’t guarantee anything works as
intended.

## installation

You can install metasim from github with:

``` r
# install.packages("devtools")
devtools::install_github("softloud/metasim")
```

## examples

### simulate paired sample sizes

``` r
# packages
library(metasim)
#> Warning: replacing previous import 'assertthat::has_name' by
#> 'tibble::has_name' when loading 'metasim'
library(tidyverse)

# so these results are reproducible
set.seed(38) 

# I like to set.seed with my age. It makes me feel smug that I'm a middle-aged woman who codes. 
```

This is a function I have often wished I’ve had on hand when simulating
meta-analysis data. Thing is, running, say, 1000 simulations, I want to
do this for the *same* sample sizes. So, I need to generate the sample
sizes for each study and for each group (control or intervention).

Given a specific \(k\), generate a set of sample sizes.

``` r

# defaults to k = 3
sim_n() %>% knitr::kable()
```

| study    | group        |   n |
| :------- | :----------- | --: |
| study\_1 | control      |  83 |
| study\_2 | control      |  28 |
| study\_3 | control      |  44 |
| study\_1 | intervention | 123 |
| study\_2 | intervention |  23 |
| study\_3 | intervention |  42 |

``` r

sim_n(k = 3) %>% knitr::kable()
```

| study    | group        |   n |
| :------- | :----------- | --: |
| study\_1 | control      |  67 |
| study\_2 | control      | 169 |
| study\_3 | control      |  22 |
| study\_1 | intervention |  76 |
| study\_2 | intervention | 226 |
| study\_3 | intervention |  34 |

``` r

# set k to a different value

sim_n(k = 6) %>% knitr::kable()
```

| study    | group        |   n |
| :------- | :----------- | --: |
| study\_1 | control      | 146 |
| study\_2 | control      |  42 |
| study\_3 | control      | 154 |
| study\_4 | control      | 116 |
| study\_5 | control      | 105 |
| study\_6 | control      |  78 |
| study\_1 | intervention | 143 |
| study\_2 | intervention |  34 |
| study\_3 | intervention | 156 |
| study\_4 | intervention | 151 |
| study\_5 | intervention | 141 |
| study\_6 | intervention | 130 |

Suppose we require data that mimics small cohorts, say as small as 3,
and as large as 50.

``` r
# control upper and lower bounds
sim_n(min_n = 3, max_n = 50) %>% knitr::kable()
```

| study    | group        |  n |
| :------- | :----------- | -: |
| study\_1 | control      | 51 |
| study\_2 | control      | 41 |
| study\_3 | control      | 39 |
| study\_1 | intervention | 49 |
| study\_2 | intervention | 42 |
| study\_3 | intervention | 17 |

We expect cohorts from the same study to have roughly the same size,
proportional to that size. We can control this proportion with the
`prop` argument.

Suppose we wish to mimic data for which the cohorts are almost exactly
the same (say becaues of classes of undergrads being split in half and
accounting for dropouts).

``` r
# small variation between sample sizes of studies
sim_n(k = 2, prop = 0.05, max_n = 50) %>% knitr::kable()
```

| study    | group        |  n |
| :------- | :----------- | -: |
| study\_1 | control      | 47 |
| study\_2 | control      | 45 |
| study\_1 | intervention | 49 |
| study\_2 | intervention | 45 |

It can be useful, for more human-interpretable purposes, to display the
sample sizes in wide format.

This is also useful for calculations that convert two measures to one,
say, the standardised mean difference of the control and intervention
groups.

Consider four classrooms of children, who may have one or two away for
illness.

``` r
sim_n(k = 4, prop = 0.05, max_n = 30, wide = TRUE) %>%
  # from here I'm just relabelling the class variable for prettiness
  separate(study, into = c("remove", "class"), sep = "_") %>% 
  select(-remove) %>% 
  mutate(class = letters[as.numeric(class)]) %>% knitr::kable()
```

| class | control | intervention |
| :---- | ------: | -----------: |
| a     |      26 |           23 |
| b     |      29 |           29 |
| c     |      31 |           31 |
| d     |      27 |           28 |

### simulation parameters

Adding a few values of \(\tau\), different numbers of studies \(k\), and
so forth can ramp up the number of combinations of simulation parameters
very quickly.

I haven’t settled on a *way* of simulating data, and haven’t found heaps
in the way of guidance yet. So, this is all a bit experimental. My
guiding star is packaging what I’d use right now.

What I do always end up with is generating a dataset that summarises
what I would like to iterate over in simulation.

The `sim_df` takes user inputs for distributions, numbers of studies,
between-study error \(\tau\), within-study error \(\varepsilon\), and
the proportion \(\rho\) of sample size we expect the sample sizes to
different within study cohorts.

``` r
# defaults
sim_df() 
#> # A tibble: 90 x 9
#>        k between_study_v… median_ratio  prop rdist parameters n     id   
#>    <dbl>            <dbl>        <dbl> <dbl> <chr> <list>     <lis> <chr>
#>  1     3                0            1   0.3 norm  <list [2]> <tib… sim_1
#>  2     3                0            1   0.3 exp   <list [1]> <tib… sim_2
#>  3     3                0            1   0.3 pare… <list [2]> <tib… sim_3
#>  4     3                0            1   0.3 pare… <list [2]> <tib… sim_4
#>  5     3                0            1   0.3 lnorm <list [2]> <tib… sim_5
#>  6     7                0            1   0.3 norm  <list [2]> <tib… sim_6
#>  7     7                0            1   0.3 exp   <list [1]> <tib… sim_7
#>  8     7                0            1   0.3 pare… <list [2]> <tib… sim_8
#>  9     7                0            1   0.3 pare… <list [2]> <tib… sim_9
#> 10     7                0            1   0.3 lnorm <list [2]> <tib… sim_…
#> # … with 80 more rows, and 1 more variable: true_median <dbl>

sim_df() %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    90 obs. of  9 variables:
#>  $ k                      : num  3 3 3 3 3 7 7 7 7 7 ...
#>  $ between_study_variation: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ prop                   : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist                  : chr  "norm" "exp" "pareto" "pareto" ...
#>  $ parameters             :List of 90
#>  $ n                      :List of 90
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_median            : num  6.70e+01 2.31e-01 7.80e-01 4.14e-01 1.29e+19 ...

# only consider small values of k
sim_df(k = c(2, 5, 7)) %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    90 obs. of  9 variables:
#>  $ k                      : num  2 2 2 2 2 5 5 5 5 5 ...
#>  $ between_study_variation: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ prop                   : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist                  : chr  "norm" "exp" "pareto" "pareto" ...
#>  $ parameters             :List of 90
#>  $ n                      :List of 90
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_median            : num  6.70e+01 2.31e-01 7.80e-01 4.14e-01 1.29e+19 ...
```

For the list-column of tibbles `n`, the `sim_df` function calls `sim_n`
and generates a set of sample sizes based on the value in the column
`k`.

``` r
demo_k <- sim_df() 

# the variable n is a list-column of tibbles
demo_k %>% pluck("n") %>% head(3)
#> [[1]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control        122
#> 2 study_2 control         94
#> 3 study_3 control         44
#> 4 study_1 intervention   128
#> 5 study_2 intervention    67
#> 6 study_3 intervention    39
#> 
#> [[2]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         84
#> 2 study_2 control         72
#> 3 study_3 control        171
#> 4 study_1 intervention   112
#> 5 study_2 intervention    58
#> 6 study_3 intervention   105
#> 
#> [[3]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control        105
#> 2 study_2 control         23
#> 3 study_3 control         50
#> 4 study_1 intervention   105
#> 5 study_2 intervention    38
#> 6 study_3 intervention    76


# compare the number of rows in the dataframe in the n column with the k value
# divide by two because there are two rows for each study,
# one for each group, control and intervention
demo_k %>% pluck("n") %>% map_int(nrow) %>% head(3) / 2
#> [1] 3 3 3
demo_k %>% pluck("k") %>% head(3)
#> [1] 3 3 3
```

## simulating data

Once we have established a set of sample sizes for a given distribution,
with parameters, and so forth, I usually want to generate a sample for
each of those `n`. We need to adjust the value of the sampled data based
on the median ratio, and whether the `n` is from a control or
intervention group.

A random effect is added to account for the between study error \(\tau\)
and within study error \(\varepsilon\).

For meta-analysis data, we work with summmary statistics, so we drop the
sample and return tabulated summary
stats.

``` r
sim_stats()  %>% knitr::kable()
```

| this\_study\_error | study    | group        |   n |      min |      max |     mean |        sd | first\_q |   median | third\_q |       iqr |
| -----------------: | :------- | :----------- | --: | -------: | -------: | -------: | --------: | -------: | -------: | -------: | --------: |
|        \-0.0007688 | study\_1 | control      |  42 | 49.47683 | 50.33770 | 49.90456 | 0.2090809 | 49.77838 | 49.90421 | 50.02258 | 0.2442020 |
|        \-0.0007688 | study\_1 | intervention |  85 | 59.40434 | 60.37750 | 59.92519 | 0.2064809 | 59.74825 | 59.95014 | 60.07171 | 0.3234604 |
|          0.0131850 | study\_2 | control      | 114 | 50.17195 | 51.20726 | 50.70880 | 0.2027588 | 50.56701 | 50.72414 | 50.84995 | 0.2829396 |
|          0.0131850 | study\_2 | intervention | 113 | 60.41371 | 61.34459 | 60.80126 | 0.2233165 | 60.62855 | 60.77614 | 60.94840 | 0.3198540 |
|          0.0472667 | study\_3 | control      |  73 | 51.88031 | 52.81565 | 52.38961 | 0.1752744 | 52.28427 | 52.37943 | 52.51571 | 0.2314398 |
|          0.0472667 | study\_3 | intervention | 136 | 62.39408 | 63.50001 | 62.90528 | 0.2047222 | 62.75745 | 62.90479 | 63.05386 | 0.2964118 |

## trial

In a trial, we’d first want to simulate some data, for a given
distribution, for this we use the `sim_stats` function discussed in the
above section.

With the summary statistics, we then calculate an estimate of the effect
or the variance of the effect.

1.  simulate data
2.  calculate summary statistics
3.  **calculate estimates using summary statistics**
4.  calculate effects using estimates (difference, standardised,
    log-ratio)\[1\]
5.  meta-analyse
6.  return simulation results of interest

The first two steps are taken care of by the `sim_stats` function. The
third step will by necessity be bespoke.

But the rest could be automated, assuming there are the same kinds of
results.

| step                | input                                     | output                         |
| ------------------- | ----------------------------------------- | ------------------------------ |
| calculate estimates | summary statistics as produced by `sim_n` | summary stats                  |
| calculate effects   | summary stats                             | `effect` and `effect_se`       |
| meta-analyse        | `effect` and `effect_se`                  | `rma` object                   |
| summary stats       | `rma` object                              | some kind of `broom`ing script |

``` r
metatrial()
#> $results
#> # A tibble: 3 x 11
#>   effect effect_se p_value  ci_lb  ci_ub   tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>         <dbl>
#> 1 47.5    7.71     2.53e-2 14.4   80.7   179.   REML   m            50    
#> 2  9.49   1.56     2.62e-2  2.75  16.2     7.34 REML   md           10    
#> 3  0.182  0.000419 5.32e-6  0.180  0.184   0    REML   lr            0.182
#> # … with 2 more variables: coverage <lgl>, bias <dbl>
#> 
#> $errors
#> $errors$model
#> # A tibble: 0 x 3
#> # … with 3 variables: measure <chr>, rma <list>, fe <chr>
#> 
#> $errors$safely
#> NULL
```

## summarising simulation results

So, now we can put together some generic summarisations. Things I always
want to do. Like calculate the coverage probability, confidence interval
width, and bias. Most results here are mean values across all trials,
the exceptions being `cp_` variables.

`metasim` calls `metatrial` many times and summarises the results.

``` r
metasim()
#> $results
#> $results[[1]]
#> $results[[1]]$results
#> # A tibble: 3 x 11
#>   effect effect_se p_value  ci_lb   ci_ub    tau2 method measure
#>    <dbl>     <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <chr>  <chr>  
#> 1 57.7   13.1      4.78e-2 1.36   114.    5.14e+2 REML   m      
#> 2 11.5    2.64     4.93e-2 0.0903  22.8   2.09e+1 REML   md     
#> 3  0.181  0.000809 2.00e-5 0.178    0.185 1.48e-6 REML   lr     
#> # … with 3 more variables: true_effect <dbl>, coverage <lgl>, bias <dbl>
#> 
#> $results[[1]]$errors
#> $results[[1]]$errors$model
#> # A tibble: 0 x 3
#> # … with 3 variables: measure <chr>, rma <list>, fe <chr>
#> 
#> $results[[1]]$errors$safely
#> NULL
#> 
#> 
#> 
#> $results[[2]]
#> $results[[2]]$results
#> # A tibble: 3 x 11
#>   effect effect_se p_value  ci_lb  ci_ub  tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>  <dbl>  <dbl> <dbl> <chr>  <chr>         <dbl>
#> 1 44.7     1.16e+1 6.10e-2 -5.13  94.6   403.  REML   m            50    
#> 2  8.94    2.32e+0 6.10e-2 -1.02  18.9    16.1 REML   md           10    
#> 3  0.182   4.30e-5 5.58e-8  0.182  0.182   0   REML   lr            0.182
#> # … with 2 more variables: coverage <lgl>, bias <dbl>
#> 
#> $results[[2]]$errors
#> $results[[2]]$errors$model
#> # A tibble: 0 x 3
#> # … with 3 variables: measure <chr>, rma <list>, fe <chr>
#> 
#> $results[[2]]$errors$safely
#> NULL
#> 
#> 
#> 
#> $results[[3]]
#> $results[[3]]$results
#> # A tibble: 3 x 11
#>   effect effect_se p_value  ci_lb  ci_ub    tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>  <dbl>  <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 45.3     7.56    2.68e-2 12.7   77.8   1.72e+2 REML   m            50    
#> 2  9.07    1.60    2.97e-2  2.20  16.0   7.66e+0 REML   md           10    
#> 3  0.182   0.00183 1.01e-4  0.174  0.190 8.37e-6 REML   lr            0.182
#> # … with 2 more variables: coverage <lgl>, bias <dbl>
#> 
#> $results[[3]]$errors
#> $results[[3]]$errors$model
#> # A tibble: 0 x 3
#> # … with 3 variables: measure <chr>, rma <list>, fe <chr>
#> 
#> $results[[3]]$errors$safely
#> NULL
#> 
#> 
#> 
#> $results[[4]]
#> $results[[4]]$results
#> # A tibble: 3 x 11
#>   effect effect_se p_value  ci_lb   ci_ub    tau2 method measure
#>    <dbl>     <dbl>   <dbl>  <dbl>   <dbl>   <dbl> <chr>  <chr>  
#> 1 53.0   12.6      5.25e-2 -1.41  107.    4.80e+2 REML   m      
#> 2 10.6    2.58     5.41e-2 -0.469  21.7   2.00e+1 REML   md     
#> 3  0.183  0.000907 2.47e-5  0.179   0.186 2.01e-6 REML   lr     
#> # … with 3 more variables: true_effect <dbl>, coverage <lgl>, bias <dbl>
#> 
#> $results[[4]]$errors
#> $results[[4]]$errors$model
#> # A tibble: 0 x 3
#> # … with 3 variables: measure <chr>, rma <list>, fe <chr>
#> 
#> $results[[4]]$errors$safely
#> NULL
#> 
#> 
#> 
#> 
#> $errors
#> [1] "errors"
#> 
#> $results_summary
#> # A tibble: 3 x 8
#>   measure    tau2 ci_width     bias coverage_count successful_tria…
#>   <chr>     <dbl>    <dbl>    <dbl>          <int>            <int>
#> 1 lr      2.96e-6  0.00772  2.73e-4              4                4
#> 2 m       3.92e+2 96.6     -1.64e-1              4                4
#> 3 md      1.62e+1 19.7     -2.44e-2              4                4
#> # … with 2 more variables: coverage <dbl>, id <chr>
```

## simulate over parameters

``` r
# metasims is not working yet.
```

1.  Ideally this would be configurable but let’s hardcode it for now.
