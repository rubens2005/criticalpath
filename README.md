
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criticalpath Package

<!-- badges: start -->

<!-- badges: end -->

criticalpath package is an object oriented implementation of the
Critical Path Method (CPM) in R with R6 library. CPM is a method used to
estimate the minimum project duration and determine the amount of
scheduling flexibility on the logical network paths within the schedule
model. The flexibility is in terms of early start, early finish, late
start, late finish, total float and free float. Beside, it permits to
quantify the complexity of network diagram through the analysis of
topological indicators. Finally, it permits to change the activities
duration to perform what-if scenario analysis.

With this package, you can calculate the following CPM parameters:

  - Schedule duration
  - Early start and finish date of each activity
  - Late start and finish date of each activity
  - Critical activities
  - Critical path
  - Total float and free float
  - Gantt Matrix
  - What-if scenario analysis
  - Topological indicators

The aim of this package is to apply critical path method, for
researchers to make experiments with CPM parameters.

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
# An empty schedule.
schedule <- Schedule$new()

# A schedule with activities and relations.
activities <- data.frame(
  id        = 1:17,
  name      = paste("a", as.character(1:17), sep=""),
  duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
)

relations <- data.frame(
  from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,
           7,  8,  9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
  to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11,
           12, 13, 14, 15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
)
schedule <- Schedule$new(activities, relations)
schedule$title <- "Project 1: Cost Information System"
schedule$reference <- "VANHOUCKE, Mario.
Integrated project management and control:
  first comes the theory, then the practice.
  Gent: Springer, 2014, p. 6"
```

What is the schedule duration?

``` r
schedule$duration
#> [1] 11
```

Which activities are critical?

``` r
schedule$activities$name[schedule$activities$critical]
#> [1] "a1"  "a2"  "a4"  "a11" "a16"
```

What is the critical relations?

``` r
schedule$relations[schedule$relations$critical, c(1,2)]
#>    from to
#> 1     1  2
#> 3     2  4
#> 10    4 11
#> 17   11 16
```

# More information

[CRAN - Package
criticalpath](https://CRAN.R-project.org/package=criticalpath)

[criticalpath Page](https://rubensjoserosa.com/criticalpath)
