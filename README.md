
<!-- README.md is generated from README.Rmd. Please edit that file -->
metasim
=======

The goal of metasim is to simulate meta-analysis data.

I found I was rewriting the same types of analyses. I got to thinking how to make a modular set of tools for simulating meta-anlaysis data.

In particular, I'm interested in simulating for different values of

-   *k*, number of studies
-   *τ* variation between studies
-   *ε* variation within a study
-   numbers of trials
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

Examples
--------

``` r
# packages
library(metasim)

# so these results are reproducible
set.seed(38) 

# I like to set.seed with my age. It makes me feel smug that I'm a middle-aged woman who codes. 
```

This is a function I have often wished I've had on hand when simulating meta-analysis data. Thing is, running, say, 1000 simulations, I want to do this for the *same* sample sizes. So, I need to generate the sample sizes for each study and for each group (control or intervention).

Given a specific *k*, generate a set of sample sizes.

``` r

# defaults to k = 3
meta_n()
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         83
#> 2 study_2 control         28
#> 3 study_3 control         44
#> 4 study_1 intervention   123
#> 5 study_2 intervention    23
#> 6 study_3 intervention    42

meta_n(k = 3)
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

meta_n(k = 6) 
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
meta_n(min_n = 3, max_n = 50)
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
meta_n(k = 2, prop = 0.025, max_n = 50)
#> # A tibble: 4 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         48
#> 2 study_2 control         45
#> 3 study_1 intervention    49
#> 4 study_2 intervention    45
```

It can be useful, for more human-interpretable purposes, to display the sample sizes in wide format.

This is also useful for calculations that convert two measures to one, say, the standardised mean difference of the control and intervention groups.

Consider four classrooms of children, who may have one or two away for illness.

``` r
meta_n(k = 4, prop = 0.05, max_n = 30, wide = TRUE)
#> # A tibble: 4 x 3
#>   study   control intervention
#>   <chr>     <int>        <int>
#> 1 study_1      26           23
#> 2 study_2      29           29
#> 3 study_3      31           31
#> 4 study_4      27           28
```
