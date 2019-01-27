
<!-- README.md is generated from README.Rmd. Please edit that file -->
metasim
=======

The goal of metasim is to simulate meta-analysis data.

I found I was rewriting the same types of analyses. I got to thinking how to make a modular set of tools for simulating meta-anlaysis data.

In particular, I'm interested in simulating for different - *k*, number of studies - *τ* variation between studies - *ϵ* variation within a study - numbers of trials - distributions, and multiple sets of parameters

work in progress
----------------

This package is a work in progress, not intended for public consumption.

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
library(metasim)
```

This is a function I have often wished I've had on hand when simulating meta-analysis data. Thing is, running, say, 1000 simulations, I want to do this for the *same* sample sizes. So, I need to generate the sample sizes for each study and for each group (control or intervention).

Given a specific *k*, generate a set of sample sizes.

``` r

# defaults to k = 3
meta_n()
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         59
#> 2 study_2 control         50
#> 3 study_3 control        106
#> 4 study_1 intervention    88
#> 5 study_2 intervention    48
#> 6 study_3 intervention   128

meta_n(k = 3)
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control        149
#> 2 study_2 control        130
#> 3 study_3 control        105
#> 4 study_1 intervention   220
#> 5 study_2 intervention   118
#> 6 study_3 intervention    72

# set k to a different value

meta_n(k = 6) 
#> # A tibble: 12 x 3
#>    study   group            n
#>    <chr>   <chr>        <int>
#>  1 study_1 control         27
#>  2 study_2 control         65
#>  3 study_3 control         46
#>  4 study_4 control         63
#>  5 study_5 control         73
#>  6 study_6 control         62
#>  7 study_1 intervention    16
#>  8 study_2 intervention   109
#>  9 study_3 intervention    54
#> 10 study_4 intervention    89
#> 11 study_5 intervention    55
#> 12 study_6 intervention    49
```

Suppose we require data that mimics small cohorts, say as small as 3, and as large as 50.

``` r
# control upper and lower bounds
meta_n(min_n = 3, max_n = 50)
#> # A tibble: 6 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         15
#> 2 study_2 control         51
#> 3 study_3 control         32
#> 4 study_1 intervention    12
#> 5 study_2 intervention    31
#> 6 study_3 intervention    30
```

We expect cohorts from the same study to have roughly the same size, proportional to that size. We can control this proportion with the `prop` argument.

Suppose we wish to mimic data for which the cohorts are almost exactly the same (say becaues of classes of undergrads being split in half and accounting for dropouts).

``` r
# small variation between sample sizes of studies
meta_n(k = 2, prop = 0.1)
#> # A tibble: 4 x 3
#>   study   group            n
#>   <chr>   <chr>        <int>
#> 1 study_1 control         31
#> 2 study_2 control        138
#> 3 study_1 intervention    25
#> 4 study_2 intervention   119
```
