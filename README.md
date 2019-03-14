
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
#> # A tibble: 18 x 9
#>        k between_study_v… median_ratio  prop rdist parameters n     id   
#>    <dbl>            <dbl>        <dbl> <dbl> <chr> <list>     <lis> <chr>
#>  1     3              0            1     0.3 norm  <list [2]> <tib… sim_1
#>  2     7              0            1     0.3 norm  <list [2]> <tib… sim_2
#>  3    20              0            1     0.3 norm  <list [2]> <tib… sim_3
#>  4     3              0.2          1     0.3 norm  <list [2]> <tib… sim_4
#>  5     7              0.2          1     0.3 norm  <list [2]> <tib… sim_5
#>  6    20              0.2          1     0.3 norm  <list [2]> <tib… sim_6
#>  7     3              0.4          1     0.3 norm  <list [2]> <tib… sim_7
#>  8     7              0.4          1     0.3 norm  <list [2]> <tib… sim_8
#>  9    20              0.4          1     0.3 norm  <list [2]> <tib… sim_9
#> 10     3              0            1.2   0.3 norm  <list [2]> <tib… sim_…
#> 11     7              0            1.2   0.3 norm  <list [2]> <tib… sim_…
#> 12    20              0            1.2   0.3 norm  <list [2]> <tib… sim_…
#> 13     3              0.2          1.2   0.3 norm  <list [2]> <tib… sim_…
#> 14     7              0.2          1.2   0.3 norm  <list [2]> <tib… sim_…
#> 15    20              0.2          1.2   0.3 norm  <list [2]> <tib… sim_…
#> 16     3              0.4          1.2   0.3 norm  <list [2]> <tib… sim_…
#> 17     7              0.4          1.2   0.3 norm  <list [2]> <tib… sim_…
#> 18    20              0.4          1.2   0.3 norm  <list [2]> <tib… sim_…
#> # … with 1 more variable: true_median <dbl>

sim_df() %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    18 obs. of  9 variables:
#>  $ k                      : num  3 7 20 3 7 20 3 7 20 3 ...
#>  $ between_study_variation: num  0 0 0 0.2 0.2 0.2 0.4 0.4 0.4 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1.2 ...
#>  $ prop                   : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist                  : chr  "norm" "norm" "norm" "norm" ...
#>  $ parameters             :List of 18
#>  $ n                      :List of 18
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_median            : num  67 67 67 67 67 67 67 67 67 67 ...

# only consider small values of k
sim_df(k = c(2, 5, 7)) %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    18 obs. of  9 variables:
#>  $ k                      : num  2 5 7 2 5 7 2 5 7 2 ...
#>  $ between_study_variation: num  0 0 0 0.2 0.2 0.2 0.4 0.4 0.4 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1.2 ...
#>  $ prop                   : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist                  : chr  "norm" "norm" "norm" "norm" ...
#>  $ parameters             :List of 18
#>  $ n                      :List of 18
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_median            : num  67 67 67 67 67 67 67 67 67 67 ...
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
#> 1 study_1 control         46
#> 2 study_2 control        178
#> 3 study_3 control        110
#> 4 study_1 intervention    51
#> 5 study_2 intervention   124
#> 6 study_3 intervention    61
#> 
#> [[2]]
#> # A tibble: 14 x 3
#>    study   group            n
#>    <chr>   <chr>        <int>
#>  1 study_1 control          7
#>  2 study_2 control         24
#>  3 study_3 control        162
#>  4 study_4 control        239
#>  5 study_5 control         19
#>  6 study_6 control        184
#>  7 study_7 control        125
#>  8 study_1 intervention     4
#>  9 study_2 intervention    40
#> 10 study_3 intervention   117
#> 11 study_4 intervention   232
#> 12 study_5 intervention    31
#> 13 study_6 intervention    45
#> 14 study_7 intervention   172
#> 
#> [[3]]
#> # A tibble: 40 x 3
#>    study    group       n
#>    <chr>    <chr>   <int>
#>  1 study_1  control    45
#>  2 study_2  control   191
#>  3 study_3  control   102
#>  4 study_4  control   199
#>  5 study_5  control   193
#>  6 study_6  control    21
#>  7 study_7  control    39
#>  8 study_8  control    45
#>  9 study_9  control    99
#> 10 study_10 control    39
#> # … with 30 more rows


# compare the number of rows in the dataframe in the n column with the k value
# divide by two because there are two rows for each study,
# one for each group, control and intervention
demo_k %>% pluck("n") %>% map_int(nrow) %>% head(3) / 2
#> [1]  3  7 20
demo_k %>% pluck("k") %>% head(3)
#> [1]  3  7 20
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
|          0.4012801 | study\_1 | control      |  32 | 32.94933 | 33.75332 | 33.43390 | 0.1931430 | 33.30435 | 33.40224 | 33.59519 | 0.2908327 |
|          0.4012801 | study\_1 | intervention |  25 | 89.23389 | 90.18812 | 89.66403 | 0.2674621 | 89.46491 | 89.65326 | 89.87055 | 0.4056447 |
|        \-0.1717911 | study\_2 | control      | 162 | 58.89839 | 59.78051 | 59.38849 | 0.1730677 | 59.29269 | 59.40011 | 59.50348 | 0.2107853 |
|        \-0.1717911 | study\_2 | intervention | 190 | 50.05789 | 51.07778 | 50.52452 | 0.1805773 | 50.39629 | 50.51469 | 50.63299 | 0.2367086 |
|          0.0544281 | study\_3 | control      |  80 | 46.83805 | 47.85991 | 47.37630 | 0.2045542 | 47.27983 | 47.37593 | 47.50218 | 0.2223505 |
|          0.0544281 | study\_3 | intervention |  90 | 62.89808 | 63.81617 | 63.39175 | 0.1914712 | 63.28966 | 63.37348 | 63.52904 | 0.2393868 |

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
#>   effect effect_se p_value  ci_lb ci_ub    tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>  <dbl> <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 37.7       2.75  0.00529 25.8   49.5  2.27e+1 REML   m            50    
#> 2 42.9       8.32  0.0357   7.06  78.7  2.08e+2 REML   md           10    
#> 3  0.760     0.143 0.0334   0.147  1.37 6.10e-2 REML   lr            0.182
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
#>   effect effect_se p_value  ci_lb ci_ub    tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>  <dbl> <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 53.0       8.67   0.0258  15.7  90.3  225.    REML   m            50    
#> 2 22.9      10.6    0.164  -22.8  68.7  339.    REML   md           10    
#> 3  0.123     0.337  0.751   -1.33  1.57   0.340 REML   lr            0.182
#> # … with 2 more variables: coverage <lgl>, bias <dbl>
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
#>   effect effect_se p_value   ci_lb  ci_ub    tau2 method measure
#>    <dbl>     <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <chr>  <chr>  
#> 1 47.0       3.17  0.00452  33.4   60.6   3.01e+1 REML   m      
#> 2 17.4       7.54  0.147   -15.0   49.9   1.71e+2 REML   md     
#> 3  0.315     0.136 0.146    -0.268  0.899 5.52e-2 REML   lr     
#> # … with 3 more variables: true_effect <dbl>, coverage <lgl>, bias <dbl>
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
#>   effect effect_se p_value   ci_lb ci_ub    tau2 method measure true_effect
#>    <dbl>     <dbl>   <dbl>   <dbl> <dbl>   <dbl> <chr>  <chr>         <dbl>
#> 1 42.2       4.40   0.0107  23.3   61.1   58.1   REML   m            50    
#> 2 30.4      11.5    0.118  -19.0   79.8  396.    REML   md           10    
#> 3  0.543     0.203  0.116   -0.330  1.42   0.124 REML   lr            0.182
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
#>   effect effect_se p_value   ci_lb  ci_ub    tau2 method measure
#>    <dbl>     <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <chr>  <chr>  
#> 1 40.3       7.19   0.0303   9.39   71.3  1.55e+2 REML   m      
#> 2 38.4      19.2    0.184  -44.3   121.   1.11e+3 REML   md     
#> 3  0.672     0.336  0.183   -0.773   2.12 3.38e-1 REML   lr     
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
#> NULL
#> 
#> $results_summary
#> # A tibble: 3 x 8
#>   measure    tau2 ci_width    bias coverage_count successful_tria… coverage
#>   <chr>     <dbl>    <dbl>   <dbl>          <int>            <int>    <dbl>
#> 1 lr        0.214     2.18  -0.231              4                4        1
#> 2 m       117.       50.4    4.38               4                4        1
#> 3 md      504.      105.   -17.3                4                4        1
#> # … with 1 more variable: id <chr>
```

## simulate over parameters

``` r
# metasims is not working yet.
```

1.  Ideally this would be configurable but let’s hardcode it for now.
