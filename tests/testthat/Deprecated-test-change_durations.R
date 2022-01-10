vanhoucke2014_project_2 <- function() {
  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
  )

  relations <- data.frame(
    from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9, 10, 11, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14, 14, 14, 14, 14, 15, 16, 17, 16, 17)
  )

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "Project 2: Patient Transport System"
  schedule$reference <-
    "VANHOUCKE, Mario. Integrated project management and control:
  first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
  schedule
}

test_that("Project 2: Schedule duration is 25", {
  sch <- vanhoucke2014_project_2()

  expect_equal(sch$duration, 25)
})

test_that("Project 2: Schedule critical activities are identified", {
  sch <- vanhoucke2014_project_2()

  activities <- sch$activities
  critical_activities <- paste0(activities$id[activities$critical], collapse=",")
  expected <- paste0(c(1,2,3,4,5,6,7,8,10,14,16), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Project 2: Schedule NON critical activities are identified", {
  sch <- vanhoucke2014_project_2()

  activities <- sch$activities
  non_critical_activities <- paste0(activities$id[!activities$critical], collapse=",")
  expected <- paste0(c(9,11,12,13,15,17), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Project 2: Early Start and Early Finish are correct!", {
  sch <- vanhoucke2014_project_2()
  act <- sch$activities

  expect_equal(act$ES[1], 0)
  expect_equal(act$EF[1], 1)

  expect_equal(act$ES[2], 1)
  expect_equal(act$EF[2], 2)

  expect_equal(act$ES[3], 2)
  expect_equal(act$EF[3], 5)

  expect_equal(act$ES[4], 5)
  expect_equal(act$EF[4], 7)

  expect_equal(act$ES[5], 7)
  expect_equal(act$EF[5], 9)

  expect_equal(act$ES[6], 5)
  expect_equal(act$EF[6], 7)

  expect_equal(act$ES[7], 7)
  expect_equal(act$EF[7], 9)

  expect_equal(act$ES[8], 9)
  expect_equal(act$EF[8], 10)

  expect_equal(act$ES[9], 10)
  expect_equal(act$EF[9], 14)

  expect_equal(act$ES[10], 10)
  expect_equal(act$EF[10], 15)

  expect_equal(act$ES[11], 10)
  expect_equal(act$EF[11], 13)

  expect_equal(act$ES[12], 10)
  expect_equal(act$EF[12], 13)

  expect_equal(act$ES[13], 10)
  expect_equal(act$EF[13], 14)

  expect_equal(act$ES[14], 15)
  expect_equal(act$EF[14], 20)

  expect_equal(act$ES[15], 14)
  expect_equal(act$EF[15], 15)

  expect_equal(act$ES[16], 20)
  expect_equal(act$EF[16], 25)

  expect_equal(act$ES[17], 20)
  expect_equal(act$EF[17], 22)

})

test_that("Project 2: Late Start and Late Finish are correct!", {
  sch <- vanhoucke2014_project_2()
  act <- sch$activities

  expect_equal(act$LS[1], 0)
  expect_equal(act$LF[1], 1)

  expect_equal(act$LS[2], 1)
  expect_equal(act$LF[2], 2)

  expect_equal(act$LS[3], 2)
  expect_equal(act$LF[3], 5)

  expect_equal(act$LS[4], 5)
  expect_equal(act$LF[4], 7)

  expect_equal(act$LS[5], 7)
  expect_equal(act$LF[5], 9)

  expect_equal(act$LS[6], 5)
  expect_equal(act$LF[6], 7)

  expect_equal(act$LS[7], 7)
  expect_equal(act$LF[7], 9)

  expect_equal(act$LS[8], 9)
  expect_equal(act$LF[8], 10)

  expect_equal(act$LS[9], 11)
  expect_equal(act$LF[9], 15)

  expect_equal(act$LS[10], 10)
  expect_equal(act$LF[10], 15)

  expect_equal(act$LS[11], 12)
  expect_equal(act$LF[11], 15)

  expect_equal(act$LS[12], 12)
  expect_equal(act$LF[12], 15)

  expect_equal(act$LS[13], 11)
  expect_equal(act$LF[13], 15)

  expect_equal(act$LS[14], 15)
  expect_equal(act$LF[14], 20)

  expect_equal(act$LS[15], 19)
  expect_equal(act$LF[15], 20)

  expect_equal(act$LS[16], 20)
  expect_equal(act$LF[16], 25)

  expect_equal(act$LS[17], 23)
  expect_equal(act$LF[17], 25)

})

############ Durations changed!

old_durations <- c(1,1,3, 2,2, 2,2, 1, 4,5,3,3,4, 5,1, 5,2)
new_durations <- c(1,2,5, 4,3, 2,1, 5, 3,5,5,3,4, 2,1, 2,4)

test_that("Project 2: Durations changed! Schedule duration is 28", {
  sch <- vanhoucke2014_project_2()
  sch$change_durations(new_durations)

  expect_equal(sch$duration, 31)
})

test_that("Project 2: Durations changed! Schedule critical activities are identified", {
  sch <- vanhoucke2014_project_2()
  sch$change_durations(new_durations)

  activities <- sch$activities
  critical_activities <- base::paste0(activities$id[activities$critical], collapse=",")
  expected <- base::paste0(c(1,2,3, 4,5, 8, 10,11, 14, 17), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Project 2: Durations changed! Schedule NON critical activities are identified", {
  sch <- vanhoucke2014_project_2()
  sch$change_durations(new_durations)

  activities <- sch$activities
  non_critical_activities <- paste0(activities$id[!activities$critical], collapse=",")
  expected <- paste0(c(6,7, 9,12,13, 15,16), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Project 2: Durations changed! Early Start and Early Finish are correct!", {
  sch <- vanhoucke2014_project_2()
  sch$change_durations(new_durations)
  act <- sch$activities

  expect_equal(act$ES[1], 0)
  expect_equal(act$EF[1], 1)

  expect_equal(act$ES[2], 1)
  expect_equal(act$EF[2], 3)

  expect_equal(act$ES[3], 3)
  expect_equal(act$EF[3], 8)

  expect_equal(act$ES[4], 8)
  expect_equal(act$EF[4], 12)

  expect_equal(act$ES[5], 12)
  expect_equal(act$EF[5], 15)

  expect_equal(act$ES[6], 8)
  expect_equal(act$EF[6], 10)

  expect_equal(act$ES[7], 10)
  expect_equal(act$EF[7], 11)

  expect_equal(act$ES[8], 15)
  expect_equal(act$EF[8], 20)

  expect_equal(act$ES[9], 20)
  expect_equal(act$EF[9], 23)

  expect_equal(act$ES[10], 20)
  expect_equal(act$EF[10], 25)

  expect_equal(act$ES[11], 20)
  expect_equal(act$EF[11], 25)

  expect_equal(act$ES[12], 20)
  expect_equal(act$EF[12], 23)

  expect_equal(act$ES[13], 20)
  expect_equal(act$EF[13], 24)

  expect_equal(act$ES[14], 25)
  expect_equal(act$EF[14], 27)

  expect_equal(act$ES[15], 24)
  expect_equal(act$EF[15], 25)

  expect_equal(act$ES[16], 27)
  expect_equal(act$EF[16], 29)

  expect_equal(act$ES[17], 27)
  expect_equal(act$EF[17], 31)

})

test_that("Project 2: Durations changed! Late Start and Late Finish are correct!", {
  sch <- vanhoucke2014_project_2()
  sch$change_durations(new_durations)
  act <- sch$activities

  expect_equal(act$LS[1], 0)
  expect_equal(act$LF[1], 1)

  expect_equal(act$LS[2], 1)
  expect_equal(act$LF[2], 3)

  expect_equal(act$LS[3], 3)
  expect_equal(act$LF[3], 8)

  expect_equal(act$LS[4], 8)
  expect_equal(act$LF[4], 12)

  expect_equal(act$LS[5], 12)
  expect_equal(act$LF[5], 15)

  expect_equal(act$LS[6], 12)
  expect_equal(act$LF[6], 14)

  expect_equal(act$LS[7], 14)
  expect_equal(act$LF[7], 15)

  expect_equal(act$LS[8], 15)
  expect_equal(act$LF[8], 20)

  expect_equal(act$LS[9], 22)
  expect_equal(act$LF[9], 25)

  expect_equal(act$LS[10], 20)
  expect_equal(act$LF[10], 25)

  expect_equal(act$LS[11], 20)
  expect_equal(act$LF[11], 25)

  expect_equal(act$LS[12], 22)
  expect_equal(act$LF[12], 25)

  expect_equal(act$LS[13], 21)
  expect_equal(act$LF[13], 25)

  expect_equal(act$LS[14], 25)
  expect_equal(act$LF[14], 27)

  expect_equal(act$LS[15], 26)
  expect_equal(act$LF[15], 27)

  expect_equal(act$LS[16], 29)
  expect_equal(act$LF[16], 31)

  expect_equal(act$LS[17], 27)
  expect_equal(act$LF[17], 31)

})
