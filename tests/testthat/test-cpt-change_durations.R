vanhoucke2014_project_2 <- function() {
  return(
    sch_new() %>%
      sch_title("Project 2: Patient Transport System") %>%
      sch_reference(
        "VANHOUCKE, Mario. Integrated project management and control:
  first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
      ) %>%
      sch_add_activities(
        id        = 1:17,
        name      = paste("a", as.character(1:17), sep=""),
        duration  = as.integer(c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2))
      ) %>%
      sch_add_relations(
        from = as.integer(c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9, 10, 11, 12, 13, 13, 14, 14, 15, 15)),
        to   = as.integer(c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14, 14, 14, 14, 14, 15, 16, 17, 16, 17))
      ) %>%
      sch_plan()
  )
}

test_that("Project 2: sch duration is 25", {
  sch <- vanhoucke2014_project_2()
  expect_equal(sch_duration(sch), 25)
})

test_that("Project 2: sch critical activities are identified", {
  atb <- vanhoucke2014_project_2() %>%
    sch_critical_activities()
  critical_activities <- paste0(atb$id, collapse=",")
  expected <- paste0(c(1,2,3,4,5,6,7,8,10,14,16), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Project 2: sch NON critical activities are identified", {
  atb <- vanhoucke2014_project_2() %>%
    sch_non_critical_activities()
  non_critical_activities <- paste0(atb$id, collapse=",")
  expected <- paste0(c(9,11,12,13,15,17), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Project 2: Early Start and Early Finish are correct!", {
  atb <- vanhoucke2014_project_2() %>%
    sch_activities()

  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 1)

  expect_equal(atb$early_start[2], 1)
  expect_equal(atb$early_finish[2], 2)

  expect_equal(atb$early_start[3], 2)
  expect_equal(atb$early_finish[3], 5)

  expect_equal(atb$early_start[4], 5)
  expect_equal(atb$early_finish[4], 7)

  expect_equal(atb$early_start[5], 7)
  expect_equal(atb$early_finish[5], 9)

  expect_equal(atb$early_start[6], 5)
  expect_equal(atb$early_finish[6], 7)

  expect_equal(atb$early_start[7], 7)
  expect_equal(atb$early_finish[7], 9)

  expect_equal(atb$early_start[8], 9)
  expect_equal(atb$early_finish[8], 10)

  expect_equal(atb$early_start[9], 10)
  expect_equal(atb$early_finish[9], 14)

  expect_equal(atb$early_start[10], 10)
  expect_equal(atb$early_finish[10], 15)

  expect_equal(atb$early_start[11], 10)
  expect_equal(atb$early_finish[11], 13)

  expect_equal(atb$early_start[12], 10)
  expect_equal(atb$early_finish[12], 13)

  expect_equal(atb$early_start[13], 10)
  expect_equal(atb$early_finish[13], 14)

  expect_equal(atb$early_start[14], 15)
  expect_equal(atb$early_finish[14], 20)

  expect_equal(atb$early_start[15], 14)
  expect_equal(atb$early_finish[15], 15)

  expect_equal(atb$early_start[16], 20)
  expect_equal(atb$early_finish[16], 25)

  expect_equal(atb$early_start[17], 20)
  expect_equal(atb$early_finish[17], 22)

})

test_that("Project 2: Late Start and Late Finish are correct!", {
  atb <- vanhoucke2014_project_2() %>%
    sch_activities()

  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 1)

  expect_equal(atb$late_start[2], 1)
  expect_equal(atb$late_finish[2], 2)

  expect_equal(atb$late_start[3], 2)
  expect_equal(atb$late_finish[3], 5)

  expect_equal(atb$late_start[4], 5)
  expect_equal(atb$late_finish[4], 7)

  expect_equal(atb$late_start[5], 7)
  expect_equal(atb$late_finish[5], 9)

  expect_equal(atb$late_start[6], 5)
  expect_equal(atb$late_finish[6], 7)

  expect_equal(atb$late_start[7], 7)
  expect_equal(atb$late_finish[7], 9)

  expect_equal(atb$late_start[8], 9)
  expect_equal(atb$late_finish[8], 10)

  expect_equal(atb$late_start[9], 11)
  expect_equal(atb$late_finish[9], 15)

  expect_equal(atb$late_start[10], 10)
  expect_equal(atb$late_finish[10], 15)

  expect_equal(atb$late_start[11], 12)
  expect_equal(atb$late_finish[11], 15)

  expect_equal(atb$late_start[12], 12)
  expect_equal(atb$late_finish[12], 15)

  expect_equal(atb$late_start[13], 11)
  expect_equal(atb$late_finish[13], 15)

  expect_equal(atb$late_start[14], 15)
  expect_equal(atb$late_finish[14], 20)

  expect_equal(atb$late_start[15], 19)
  expect_equal(atb$late_finish[15], 20)

  expect_equal(atb$late_start[16], 20)
  expect_equal(atb$late_finish[16], 25)

  expect_equal(atb$late_start[17], 23)
  expect_equal(atb$late_finish[17], 25)

})

############ Durations changed!

old_durations <- as.integer(c(1,1,3, 2,2, 2,2, 1, 4,5,3,3,4, 5,1, 5,2))
new_durations <- as.integer(c(1,2,5, 4,3, 2,1, 5, 3,5,5,3,4, 2,1, 2,4))

test_that("Project 2: Durations changed! sch duration is 28", {
  sch <- vanhoucke2014_project_2() %>%
    sch_change_activities_duration(new_durations)

  expect_equal(sch_duration(sch), 31)
})

test_that("Project 2: Durations changed! sch critical activities are identified", {
  atb <- vanhoucke2014_project_2() %>%
    sch_change_activities_duration(new_durations) %>%
    sch_critical_activities()
  critical_activities <- base::paste0(atb$id, collapse=",")
  expected <- base::paste0(c(1,2,3, 4,5, 8, 10,11, 14, 17), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Project 2: Durations changed! sch NON critical activities are identified", {
  atb <- vanhoucke2014_project_2() %>%
    sch_change_activities_duration(new_durations) %>%
    sch_non_critical_activities()

  non_critical_activities <- paste0(atb$id, collapse=",")
  expected <- paste0(c(6,7, 9,12,13, 15,16), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Project 2: Durations changed! Early Start and Early Finish are correct!", {
  atb <- vanhoucke2014_project_2() %>%
    sch_change_activities_duration(new_durations) %>%
    sch_activities()

  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 1)

  expect_equal(atb$early_start[2], 1)
  expect_equal(atb$early_finish[2], 3)

  expect_equal(atb$early_start[3], 3)
  expect_equal(atb$early_finish[3], 8)

  expect_equal(atb$early_start[4], 8)
  expect_equal(atb$early_finish[4], 12)

  expect_equal(atb$early_start[5], 12)
  expect_equal(atb$early_finish[5], 15)

  expect_equal(atb$early_start[6], 8)
  expect_equal(atb$early_finish[6], 10)

  expect_equal(atb$early_start[7], 10)
  expect_equal(atb$early_finish[7], 11)

  expect_equal(atb$early_start[8], 15)
  expect_equal(atb$early_finish[8], 20)

  expect_equal(atb$early_start[9], 20)
  expect_equal(atb$early_finish[9], 23)

  expect_equal(atb$early_start[10], 20)
  expect_equal(atb$early_finish[10], 25)

  expect_equal(atb$early_start[11], 20)
  expect_equal(atb$early_finish[11], 25)

  expect_equal(atb$early_start[12], 20)
  expect_equal(atb$early_finish[12], 23)

  expect_equal(atb$early_start[13], 20)
  expect_equal(atb$early_finish[13], 24)

  expect_equal(atb$early_start[14], 25)
  expect_equal(atb$early_finish[14], 27)

  expect_equal(atb$early_start[15], 24)
  expect_equal(atb$early_finish[15], 25)

  expect_equal(atb$early_start[16], 27)
  expect_equal(atb$early_finish[16], 29)

  expect_equal(atb$early_start[17], 27)
  expect_equal(atb$early_finish[17], 31)

})

test_that("Project 2: Durations changed! Late Start and Late Finish are correct!", {
  atb <- vanhoucke2014_project_2() %>%
    sch_change_activities_duration(new_durations) %>%
    sch_activities()

  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 1)

  expect_equal(atb$late_start[2], 1)
  expect_equal(atb$late_finish[2], 3)

  expect_equal(atb$late_start[3], 3)
  expect_equal(atb$late_finish[3], 8)

  expect_equal(atb$late_start[4], 8)
  expect_equal(atb$late_finish[4], 12)

  expect_equal(atb$late_start[5], 12)
  expect_equal(atb$late_finish[5], 15)

  expect_equal(atb$late_start[6], 12)
  expect_equal(atb$late_finish[6], 14)

  expect_equal(atb$late_start[7], 14)
  expect_equal(atb$late_finish[7], 15)

  expect_equal(atb$late_start[8], 15)
  expect_equal(atb$late_finish[8], 20)

  expect_equal(atb$late_start[9], 22)
  expect_equal(atb$late_finish[9], 25)

  expect_equal(atb$late_start[10], 20)
  expect_equal(atb$late_finish[10], 25)

  expect_equal(atb$late_start[11], 20)
  expect_equal(atb$late_finish[11], 25)

  expect_equal(atb$late_start[12], 22)
  expect_equal(atb$late_finish[12], 25)

  expect_equal(atb$late_start[13], 21)
  expect_equal(atb$late_finish[13], 25)

  expect_equal(atb$late_start[14], 25)
  expect_equal(atb$late_finish[14], 27)

  expect_equal(atb$late_start[15], 26)
  expect_equal(atb$late_finish[15], 27)

  expect_equal(atb$late_start[16], 29)
  expect_equal(atb$late_finish[16], 31)

  expect_equal(atb$late_start[17], 27)
  expect_equal(atb$late_finish[17], 31)

})
