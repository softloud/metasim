
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
#>        k between_study_v… within_study_va… median_ratio rdist parameters
#>    <dbl>            <dbl>            <dbl>        <dbl> <chr> <list>    
#>  1     3              0                  0            1 norm  <list [2]>
#>  2     3              0                  0            1 exp   <list [1]>
#>  3     7              0                  0            1 norm  <list [2]>
#>  4     7              0                  0            1 exp   <list [1]>
#>  5    50              0                  0            1 norm  <list [2]>
#>  6    50              0                  0            1 exp   <list [1]>
#>  7     3              0.2                0            1 norm  <list [2]>
#>  8     3              0.2                0            1 exp   <list [1]>
#>  9     7              0.2                0            1 norm  <list [2]>
#> 10     7              0.2                0            1 exp   <list [1]>
#> # ... with 98 more rows, and 3 more variables: n <list>,
#> #   true_median <dbl>, id <chr>

sim_df() %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    108 obs. of  9 variables:
#>  $ k                      : num  3 3 7 7 50 50 3 3 7 7 ...
#>  $ between_study_variation: num  0 0 0 0 0 0 0.2 0.2 0.2 0.2 ...
#>  $ within_study_variation : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ rdist                  : chr  "norm" "exp" "norm" "exp" ...
#>  $ parameters             :List of 108
#>  $ n                      :List of 108
#>  $ true_median            : num  50 0.347 50 0.347 50 ...
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...

# only consider small values of k
sim_df(k = c(2, 5, 7)) %>% str(1)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    108 obs. of  9 variables:
#>  $ k                      : num  2 2 5 5 7 7 2 2 5 5 ...
#>  $ between_study_variation: num  0 0 0 0 0 0 0.2 0.2 0.2 0.2 ...
#>  $ within_study_variation : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ median_ratio           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ rdist                  : chr  "norm" "exp" "norm" "exp" ...
#>  $ parameters             :List of 108
#>  $ n                      :List of 108
#>  $ true_median            : num  50 0.347 50 0.347 50 ...
#>  $ id                     : chr  "sim_1" "sim_2" "sim_3" "sim_4" ...
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
#> 1 study_1 control        157
#> 2 study_2 control        144
#> 3 study_3 control         96
#> 4 study_1 intervention   150
#> 5 study_2 intervention   139
#> 6 study_3 intervention    75
#> 
#> [[2]]
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control        196
#> 2 study_2 control        165
#> 3 study_3 control         47
#> 4 study_1 intervention   180
#> 5 study_2 intervention   119
#> 6 study_3 intervention    22
#> 
#> [[3]]
#> # A tibble: 14 x 3
#>    study   group            n
#>    <chr>   <chr>        <int>
#>  1 study_1 control          3
#>  2 study_2 control        127
#>  3 study_3 control         11
#>  4 study_4 control         44
#>  5 study_5 control        117
#>  6 study_6 control        124
#>  7 study_7 control          2
#>  8 study_1 intervention     2
#>  9 study_2 intervention   211
#> 10 study_3 intervention    11
#> 11 study_4 intervention    35
#> 12 study_5 intervention   140
#> 13 study_6 intervention   251
#> 14 study_7 intervention     2


# compare the number of rows in the dataframe in the n column with the k value
# divide by two because there are two rows for each study,
# one for each group, control and intervention
demo_k %>% pluck("n") %>% map_int(nrow) %>% head(3) / 2
#> [1] 3 3 7
demo_k %>% pluck("k") %>% head(3)
#> [1] 3 3 7
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

| bn\_study\_error | wn\_study\_error | study    | group        |   n | control\_indicator |       min |       max |      mean |        sd |  first\_q |    median |  third\_q |       iqr |
| ---------------: | ---------------: | :------- | :----------- | --: | :----------------- | --------: | --------: | --------: | --------: | --------: | --------: | --------: | --------: |
|      \-0.0564630 |      \-0.2089087 | study\_1 | control      |  83 | TRUE               |  90.63629 |  91.51104 |  91.06723 | 0.2058649 |  90.92014 |  91.04510 |  91.27448 | 0.3543377 |
|      \-0.0564630 |      \-0.2089087 | study\_1 | intervention |  61 | FALSE              | 108.92362 | 109.85997 | 109.31564 | 0.1935069 | 109.18835 | 109.30264 | 109.44115 | 0.2527978 |
|      \-0.3261461 |      \-0.0005479 | study\_2 | control      |  16 | TRUE               |  90.82948 |  91.59164 |  91.12424 | 0.1770324 |  91.05349 |  91.09893 |  91.17851 | 0.1250236 |
|      \-0.3261461 |      \-0.0005479 | study\_2 | intervention |  57 | FALSE              | 108.93336 | 109.88618 | 109.32419 | 0.2039480 | 109.13624 | 109.33847 | 109.45589 | 0.3196531 |
|        0.0878746 |      \-0.1275746 | study\_3 | control      | 111 | TRUE               |  90.57394 |  91.57614 |  91.08237 | 0.2241267 |  90.92625 |  91.08064 |  91.23108 | 0.3048278 |
|        0.0878746 |      \-0.1275746 | study\_3 | intervention | 112 | FALSE              | 108.84338 | 109.90397 | 109.35992 | 0.2230317 | 109.22032 | 109.35954 | 109.50735 | 0.2870282 |

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
#> # A tibble: 3 x 8
#>     ci_lb   ci_ub    i2  tau2       b effect_type true_effect in_ci
#>     <dbl>   <dbl> <dbl> <dbl>   <dbl> <chr>             <dbl> <lgl>
#> 1 123.    123.        0     0 123.    m                50     FALSE
#> 2  24.6    24.7       0     0  24.6   md               10     FALSE
#> 3   0.182   0.183     0     0   0.183 lr                0.182 TRUE
```

## summarising simulation results

So, now we can put together some generic summarisations. Things I always
want to do. Like calculate the coverage probability, confidence interval
width, and bias. Most results here are mean values across all trials,
the exceptions being `cp_` variables.

`metasim` calls `metatrial` many times and summarises the results.

``` r
metasim()
#> # A tibble: 3 x 10
#>   effect_type ci_width   ci_lb   ci_ub    tau2     i2 cp_sum cp_length
#>   <chr>          <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <int>     <int>
#> 1 lr          0.000939   0.182   0.183 1.17e-9 1.47        4         4
#> 2 m           0.0949   123.    123.    1.72e-5 3.26        0         4
#> 3 md          0.122     24.5    24.6   1.67e-7 0.0121      0         4
#> # ... with 2 more variables: cp <dbl>, id <chr>
```

## simulate over parameters

``` r
# metasims is not working yet.
```

1.  Ideally this would be configurable but let’s hardcode it for now.
