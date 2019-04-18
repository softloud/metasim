
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/softloud/metasim.svg?branch=master)](https://travis-ci.org/softloud/metasim)

[![Coverage
status](https://codecov.io/gh/softloud/metasim/branch/master/graph/badge.svg)](https://codecov.io/github/softloud/metasim?branch=master)

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
#> # A tibble: 108 x 9
#>        k tau2_true median_ratio  prop rdist parameters n     id   
#>    <dbl>     <dbl>        <dbl> <dbl> <chr> <list>     <lis> <chr>
#>  1     3         0            1   0.3 norm  <list [2]> <tib… sim_1
#>  2     3         0            1   0.3 exp   <list [1]> <tib… sim_2
#>  3     3         0            1   0.3 pare… <list [2]> <tib… sim_3
#>  4     3         0            1   0.3 pare… <list [2]> <tib… sim_4
#>  5     3         0            1   0.3 pare… <list [2]> <tib… sim_5
#>  6     3         0            1   0.3 lnorm <list [2]> <tib… sim_6
#>  7     7         0            1   0.3 norm  <list [2]> <tib… sim_7
#>  8     7         0            1   0.3 exp   <list [1]> <tib… sim_8
#>  9     7         0            1   0.3 pare… <list [2]> <tib… sim_9
#> 10     7         0            1   0.3 pare… <list [2]> <tib… sim_…
#> # … with 98 more rows, and 1 more variable: true_effect <dbl>

sim_df() %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    108 obs. of  9 variables:
#>  $ k           : num  3 3 3 3 3 3 7 7 7 7 ...
#>  $ tau2_true   : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ prop        : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist       : chr  "norm" "exp" "pareto" "pareto" ...
#>  $ parameters  :List of 108
#>  $ n           :List of 108
#>  $ id          : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_effect : num  67 0.347 0.78 0.414 3 ...

# only consider small values of k
sim_df(k = c(2, 5, 7)) %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    108 obs. of  9 variables:
#>  $ k           : num  2 2 2 2 2 2 5 5 5 5 ...
#>  $ tau2_true   : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ prop        : num  0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 ...
#>  $ rdist       : chr  "norm" "exp" "pareto" "pareto" ...
#>  $ parameters  :List of 108
#>  $ n           :List of 108
#>  $ id          : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
#>  $ true_effect : num  67 0.347 0.78 0.414 3 ...
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
#>   <chr>   <chr>        <dbl>
#> 1 study_1 control         79
#> 2 study_2 control        164
#> 3 study_3 control        155
#> 4 study_1 intervention    45
#> 5 study_2 intervention   127
#> 6 study_3 intervention   177
#> 
#> [[2]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <dbl>
#> 1 study_1 control         70
#> 2 study_2 control        184
#> 3 study_3 control         98
#> 4 study_1 intervention    79
#> 5 study_2 intervention   138
#> 6 study_3 intervention   170
#> 
#> [[3]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <dbl>
#> 1 study_1 control        195
#> 2 study_2 control         91
#> 3 study_3 control         78
#> 4 study_1 intervention    78
#> 5 study_2 intervention    31
#> 6 study_3 intervention    52


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
|        \-0.1340313 | study\_1 | control      | 197 | 56.65835 | 57.66129 | 57.19131 | 0.1895185 | 57.06155 | 57.20920 | 57.32117 | 0.2596229 |
|        \-0.1340313 | study\_1 | intervention | 104 | 51.95953 | 53.02328 | 52.50215 | 0.1910127 | 52.38960 | 52.50954 | 52.63940 | 0.2498005 |
|        \-0.2843719 | study\_2 | control      | 188 | 65.86379 | 66.94693 | 66.44225 | 0.2020039 | 66.32263 | 66.45260 | 66.56719 | 0.2445628 |
|        \-0.2843719 | study\_2 | intervention | 127 | 44.60071 | 45.67585 | 45.16485 | 0.2075106 | 45.02813 | 45.19041 | 45.29744 | 0.2693061 |
|        \-0.3709537 | study\_3 | control      | 196 | 72.04091 | 73.16230 | 72.47827 | 0.1935494 | 72.36590 | 72.48110 | 72.60956 | 0.2436504 |
|        \-0.3709537 | study\_3 | intervention | 158 | 40.70510 | 41.87238 | 41.38953 | 0.2036729 | 41.25773 | 41.39994 | 41.53640 | 0.2786698 |

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
#> # A tibble: 2 x 9
#>   conf_low conf_high  tau_sq     k  effect measure true_effect coverage
#>      <dbl>     <dbl>   <dbl> <int>   <dbl> <chr>         <dbl> <lgl>   
#> 1    25.6      84.3  140.        3 54.9    m            50     TRUE    
#> 2    -1.12      1.18   0.214     3  0.0284 lr            0.182 TRUE    
#> # … with 1 more variable: bias <dbl>
```

## summarising simulation results

So, now we can put together some generic summarisations. Things I always
want to do. Like calculate the coverage probability, confidence interval
width, and bias. Most results here are mean values across all trials,
the exceptions being `cp_` variables.

`metasim` calls `metatrial` many times and summarises the results.

``` r
metasim()
#> $errors
#> NULL
#> 
#> $results
#> # A tibble: 2 x 8
#>   measure  tau_sq ci_width   bias coverage_count successful_tria… coverage
#>   <chr>     <dbl>    <dbl>  <dbl>          <int>            <int>    <dbl>
#> 1 lr        0.623     2.90  0.113              3                4     0.75
#> 2 m       345.       70.0  -0.245              3                4     0.75
#> # … with 1 more variable: id <chr>
```

## simulate over parameters

``` r
# metasims is not working yet.
```

1.  Ideally this would be configurable but let’s hardcode it for now.
