# criticalpath 0.2.0


I have transformed the package 'criticalpath' in functional.
This permits chained call to functions and made the schedule manipulation faster with pipe "%>%".

It was created all function to work with schedule.

The Schedule class was deprecated.
All the function in Schedule class was deprecated too.

For this reason, there are several message about 'deprecated'.

In next release, I will remove all class Schedule and her functions.


Thanks.



# criticalpath 0.1.0




## Cran comments Package Submission (22 de Janeiro de 2021 13:13)


-> validar 
  - check()
  - check_rhub()
    - https://builder.r-hub.io/
  - check_win_release()
  - ### este não --->>> check_win_devel()


--->>> build()




## Test environments
* local OS windows 7,  R version 4.0.3 (2020-10-10)
* https://builder.r-hub.io/
* 

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.

## Downstream dependencies

