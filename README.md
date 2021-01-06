
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalpath

<!-- badges: start -->

<!-- badges: end -->

The goal of criticalpath is to calculate the project duration and to
find its critical activities through application of Critical Path Method
to activities and precedence relation between then.

## Installation

You can install the released version of criticalpath from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("criticalpath")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rubens2005/criticalpath")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(criticalpath)


  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
  )

  relations <- data.frame(
    from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,  7,  8,  9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11, 12, 13, 14, 15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
  )

  schedule <- schedule_from_data_frame_R6(
    activities,
    relations,
    "Project 1: Cost Information System",
    "VANHOUCKE, Mario. Integrated project management and control: first comes the theory, then the practice. Gent: Springer, 2014, p. 6"
  )

  schedule$duration
#> [1] 11
  
  schedule$activities_as_data_frame()
#>    id name duration milestone critical ES EF LS LF total_float free_float
#> 1   1   a1        1     FALSE     TRUE  0  1  0  1           0          0
#> 2   2   a2        2     FALSE     TRUE  1  3  1  3           0          0
#> 3   3   a3        2     FALSE    FALSE  1  3  3  5           2          0
#> 4   4   a4        4     FALSE     TRUE  3  7  3  7           0          0
#> 5   5   a5        3     FALSE    FALSE  3  6  4  7           1          1
#> 6   6   a6        3     FALSE    FALSE  3  6  4  7           1          1
#> 7   7   a7        3     FALSE    FALSE  3  6  5  8           2          0
#> 8   8   a8        2     FALSE    FALSE  3  5  6  8           3          0
#> 9   9   a9        1     FALSE    FALSE  3  4  7  8           4          0
#> 10 10  a10        1     FALSE    FALSE  3  4  7  8           4          0
#> 11 11  a11        2     FALSE     TRUE  7  9  7  9           0          0
#> 12 12  a12        1     FALSE    FALSE  6  7  8  9           2          2
#> 13 13  a13        1     FALSE    FALSE  5  6  8  9           3          3
#> 14 14  a14        1     FALSE    FALSE  4  5  8  9           4          4
#> 15 15  a15        1     FALSE    FALSE  4  5  8  9           4          4
#> 16 16  a16        2     FALSE     TRUE  9 11  9 11           0          0
#> 17 17  a17        1     FALSE    FALSE  9 10 10 11           1          0
#>    progr_level regr_level topo_float
#> 1            1          1          0
#> 2            2          2          0
#> 3            2          2          0
#> 4            3          3          0
#> 5            3          3          0
#> 6            3          3          0
#> 7            3          3          0
#> 8            3          3          0
#> 9            3          3          0
#> 10           3          3          0
#> 11           4          4          0
#> 12           4          4          0
#> 13           4          4          0
#> 14           4          4          0
#> 15           4          4          0
#> 16           5          5          0
#> 17           5          5          0
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
