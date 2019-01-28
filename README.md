
<!-- README.md is generated from README.Rmd. Please edit that file -->
metasim
=======

The goal of metasim is to simulate meta-analysis data.

I found I was rewriting the same types of analyses. I got to thinking how to make a modular set of tools for simulating meta-anlaysis data.

In particular, I'm interested in simulating for different values of

-   *k*, number of studies
-   *τ*<sup>2</sup>, variation between studies
-   *ε*<sup>2</sup>, variation within a study
-   numbers of trials, say 10, 100, 1000
-   distributions, *and* parameters; e.g., exp(*λ* = 1) and exp(*λ* = 2).

work in progress
----------------

This package is a work in progress, can't guarantee anything works as intended.

installation
------------

You can install metasim from github with:

``` r
# install.packages("devtools")
devtools::install_github("softloud/metasim")
```

examples
--------

### simulate paired sample sizes

``` r
# packages
library(metasim)
library(tidyverse)

# so these results are reproducible
set.seed(38) 

# I like to set.seed with my age. It makes me feel smug that I'm a middle-aged woman who codes. 
```

This is a function I have often wished I've had on hand when simulating meta-analysis data. Thing is, running, say, 1000 simulations, I want to do this for the *same* sample sizes. So, I need to generate the sample sizes for each study and for each group (control or intervention).

Given a specific *k*, generate a set of sample sizes.

``` r

# defaults to k = 3
sim_n()
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         83
#> 2 study_2 control         28
#> 3 study_3 control         44
#> 4 study_1 intervention   123
#> 5 study_2 intervention    23
#> 6 study_3 intervention    42

sim_n(k = 3)
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         67
#> 2 study_2 control        169
#> 3 study_3 control         22
#> 4 study_1 intervention    76
#> 5 study_2 intervention   226
#> 6 study_3 intervention    34

# set k to a different value

sim_n(k = 6) 
#> # A tibble: 12 x 3
#>    study   group            n
#>    <chr>   <chr>        <int>
#>  1 study_1 control        146
#>  2 study_2 control         42
#>  3 study_3 control        154
#>  4 study_4 control        116
#>  5 study_5 control        105
#>  6 study_6 control         78
#>  7 study_1 intervention   143
#>  8 study_2 intervention    34
#>  9 study_3 intervention   156
#> 10 study_4 intervention   151
#> 11 study_5 intervention   141
#> 12 study_6 intervention   130
```

Suppose we require data that mimics small cohorts, say as small as 3, and as large as 50.

``` r
# control upper and lower bounds
sim_n(min_n = 3, max_n = 50)
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         51
#> 2 study_2 control         41
#> 3 study_3 control         39
#> 4 study_1 intervention    49
#> 5 study_2 intervention    42
#> 6 study_3 intervention    17
```

We expect cohorts from the same study to have roughly the same size, proportional to that size. We can control this proportion with the `prop` argument.

Suppose we wish to mimic data for which the cohorts are almost exactly the same (say becaues of classes of undergrads being split in half and accounting for dropouts).

``` r
# small variation between sample sizes of studies
sim_n(k = 2, prop = 0.05, max_n = 50)
#> # A tibble: 4 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         47
#> 2 study_2 control         45
#> 3 study_1 intervention    49
#> 4 study_2 intervention    45
```

It can be useful, for more human-interpretable purposes, to display the sample sizes in wide format.

This is also useful for calculations that convert two measures to one, say, the standardised mean difference of the control and intervention groups.

Consider four classrooms of children, who may have one or two away for illness.

``` r
sim_n(k = 4, prop = 0.05, max_n = 30, wide = TRUE) %>%
  # from here I'm just relabelling the class variable for prettiness
  separate(study, into = c("remove", "class"), sep = "_") %>% 
  select(-remove) %>% 
  mutate(class = letters[as.numeric(class)])
#> # A tibble: 4 x 3
#>   class control intervention
#>   <chr>   <int>        <int>
#> 1 a          26           23
#> 2 b          29           29
#> 3 c          31           31
#> 4 d          27           28
```

### simulation parameters

Adding a few values of *τ*, different numbers of studies *k*, and so forth can ramp up the number of combinations of simulation parameters very quickly.

I haven't settled on a *way* of simulating data, and haven't found heaps in the way of guidance yet. So, this is all a bit experimental. My guiding star is packaging what I'd use right now.

What I do always end up with is generating a dataset that summarises what I would like to iterate over in simulation.

The `sim_df` takes user inputs for distributions, numbers of studies, between-study error *τ*, within-study error *ε*, and the proportion *ρ* of sample size we expect the sample sizes to different within study cohorts.

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

sim_df() %>%   str(1)
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

For the list-column of tibbles `n`, the `sim_df` function calls `sim_n` and generates a set of sample sizes based on the value in the column `k`.

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
