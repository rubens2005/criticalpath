
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalpath Package

<!-- badges: start -->
<!-- badges: end -->

criticalpath package is an R implementation of the Critical Path Method
(CPM) in R with R6 library. CPM is a method used to estimate the minimum
project duration and determine the amount of scheduling flexibility on
the logical network paths within the schedule model. The flexibility is
in terms of early start, early finish, late start, late finish, total
float and free float. Beside, it permits to quantify the complexity of
network diagram through the analysis of topological indicators. Finally,
it permits to change the activities duration to perform what-if scenario
analysis.

With this package, you can calculate the following CPM parameters:

-   Schedule duration
-   Early start and finish date of each activity
-   Late start and finish date of each activity
-   Critical activities
-   Critical path
-   Total float and free float
-   Gantt Matrix
-   What-if scenario analysis
-   Topological indicators

The aim of this package is to apply critical path method, for Project
Manager and Researches make “What if?” scenario analysis or experiments
with CPM parameters.

## Installation

You can install the released version of criticalpath from
[CRAN](https://CRAN.R-project.org) with:

``` r
#install.packages("criticalpath")
```

## Example

To create a schedule:

``` r
library(criticalpath)

sch <- sch_new() %>% 
  sch_title("Project 1: Cost Information System") %>% 
  sch_reference("VANHOUCKE, Mario.
Integrated project management and control:
  first comes the theory, then the practice.
  Gent: Springer, 2014, p. 6") %>% 
  sch_add_activities(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
  ) %>% 
  sch_add_relations(
    from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
           7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
    to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
           12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
  ) %>% 
  sch_plan()
```

What is the schedule duration?

``` r
sch_duration(sch)
#> [1] 11
```

Which activities are critical?

``` r
sch_critical_activities(sch)
#> # A tibble: 5 x 14
#>      id name  duration milestone critical early_start early_finish late_start
#>   <int> <chr>    <int> <lgl>     <lgl>          <int>        <int>      <int>
#> 1     1 a1           1 FALSE     TRUE               0            1          0
#> 2     2 a2           2 FALSE     TRUE               1            3          1
#> 3     4 a4           4 FALSE     TRUE               3            7          3
#> 4    11 a11          2 FALSE     TRUE               7            9          7
#> 5    16 a16          2 FALSE     TRUE               9           11          9
#> # ... with 6 more variables: late_finish <int>, total_float <int>,
#> #   free_float <int>, progr_level <int>, regr_level <int>, topo_float <int>
```

What is the critical relations?

``` r
sch_critical_relations(sch)
#> # A tibble: 4 x 8
#>    from    to type    lag critical   ord i_from  i_to
#>   <int> <int> <chr> <int> <lgl>    <int>  <int> <int>
#> 1     1     2 FS        0 TRUE         1      1     2
#> 2     2     4 FS        0 TRUE         3      2     4
#> 3     4    11 FS        0 TRUE        10      4    11
#> 4    11    16 FS        0 TRUE        17     11    16
```

# More information

[CRAN - Package
criticalpath](https://CRAN.R-project.org/package=criticalpath)

[criticalpath Page](https://rubensjoserosa.com/criticalpath)
