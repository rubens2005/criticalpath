activities <- data.frame(
  id        = 1:17,
  name      = paste("a", as.character(1:17), sep=""),
  duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
)

relations <- data.frame(
  from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,  7,  8,  9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
  to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11, 12, 13, 14, 15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
)

vanhoucke2014_project_1 <- function() {
  schedule <- Schedule$new(activities, relations)
  schedule$title <- "Project 1: Cost Information System"
  schedule$reference <- "VANHOUCKE, Mario. Integrated project management and control:
  first comes the theory, then the practice.
  Gent: Springer, 2014, p. 6"
  schedule
}

test_that("Creating a ONE activity schedule with ZERO duration", {
  activities <- data.frame(
    id        = 1,
    name      = "A",
    duration  = 0
  )

  schedule <- Schedule$new(activities)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  activities <- schedule$activities

  expect_equal(schedule$duration, 0)
  expect_equal(activities$ES[1], 0)
  expect_equal(activities$EF[1], 0)
  expect_equal(activities$LS[1], 0)
  expect_equal(activities$LF[1], 0)
  expect_true(activities$critical[1])
})

test_that("Creating a schedule with ONE activity", {
  activities <- data.frame(
    id        = 1,
    name      = "A",
    duration  = 3
  )

  schedule <- Schedule$new(activities)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  activities <- schedule$activities

  expect_equal(schedule$duration, 3)
  expect_equal(activities$ES[1], 0)
  expect_equal(activities$EF[1], 3)
  expect_equal(activities$LS[1], 0)
  expect_equal(activities$LF[1], 3)
  expect_true(activities$critical[1])
})

test_that("Creating a schedule only with activities list, without relations", {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  schedule <- Schedule$new(activities)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  activities <- schedule$activities

  expect_equal(schedule$duration, 4)

  expect_equal(activities$ES[1], 0)
  expect_equal(activities$EF[1], 3)
  expect_equal(activities$LS[1], 1)
  expect_equal(activities$LF[1], 4)
  expect_false(activities$critical[1])

  expect_equal(activities$ES[2], 0)
  expect_equal(activities$EF[2], 2)
  expect_equal(activities$LS[2], 2)
  expect_equal(activities$LF[2], 4)
  expect_false(activities$critical[2])

  expect_equal(activities$ES[3], 0)
  expect_equal(activities$EF[3], 4)
  expect_equal(activities$LS[3], 0)
  expect_equal(activities$LF[3], 4)
  expect_true(activities$critical[3])

})

test_that("Schedule duration is 11", {
  schedule <- vanhoucke2014_project_1()

  expect_equal(schedule$duration, 11)
})

test_that("Schedule critical activities are identified", {
  schedule <- vanhoucke2014_project_1()

  activities <- schedule$activities
  critical_activities <- paste0(activities$id[activities$critical], collapse=",")
  expected <- paste0(c(1, 2, 4, 11, 16), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Schedule NON critical activities are identified", {
  schedule <- vanhoucke2014_project_1()

  activities <- schedule$activities
  non_critical_activities <- paste0(activities$id[!activities$critical], collapse=",")
  expected <- paste0(c(3, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Early Start and Early Finish are correct!", {
  schedule <- vanhoucke2014_project_1()
  act <- schedule$activities

  expect_equal(act$ES[1], 0)
  expect_equal(act$EF[1], 1)

  expect_equal(act$ES[2], 1)
  expect_equal(act$EF[2], 3)

  expect_equal(act$ES[3], 1)
  expect_equal(act$EF[3], 3)

  expect_equal(act$ES[4], 3)
  expect_equal(act$EF[4], 7)

  expect_equal(act$ES[5], 3)
  expect_equal(act$EF[5], 6)

  expect_equal(act$ES[6], 3)
  expect_equal(act$EF[6], 6)

  expect_equal(act$ES[7], 3)
  expect_equal(act$EF[7], 6)

  expect_equal(act$ES[8], 3)
  expect_equal(act$EF[8], 5)

  expect_equal(act$ES[9], 3)
  expect_equal(act$EF[9], 4)

  expect_equal(act$ES[10], 3)
  expect_equal(act$EF[10], 4)

  expect_equal(act$ES[11], 7)
  expect_equal(act$EF[11], 9)

  expect_equal(act$ES[12], 6)
  expect_equal(act$EF[12], 7)

  expect_equal(act$ES[13], 5)
  expect_equal(act$EF[13], 6)

  expect_equal(act$ES[14], 4)
  expect_equal(act$EF[14], 5)

  expect_equal(act$ES[15], 4)
  expect_equal(act$EF[15], 5)

  expect_equal(act$ES[16], 9)
  expect_equal(act$EF[16], 11)

  expect_equal(act$ES[17], 9)
  expect_equal(act$EF[17], 10)

})

test_that("Late Start and Late Finish are correct!", {
  schedule <- vanhoucke2014_project_1()
  act <- schedule$activities

  expect_equal(act$LS[1], 0)
  expect_equal(act$LF[1], 1)

  expect_equal(act$LS[2], 1)
  expect_equal(act$LF[2], 3)

  expect_equal(act$LS[3], 3)
  expect_equal(act$LF[3], 5)

  expect_equal(act$LS[4], 3)
  expect_equal(act$LF[4], 7)

  expect_equal(act$LS[5], 4)
  expect_equal(act$LF[5], 7)

  expect_equal(act$LS[6], 4)
  expect_equal(act$LF[6], 7)

  expect_equal(act$LS[7], 5)
  expect_equal(act$LF[7], 8)

  expect_equal(act$LS[8], 6)
  expect_equal(act$LF[8], 8)

  expect_equal(act$LS[9], 7)
  expect_equal(act$LF[9], 8)

  expect_equal(act$LS[10], 7)
  expect_equal(act$LF[10], 8)

  expect_equal(act$LS[11], 7)
  expect_equal(act$LF[11], 9)

  expect_equal(act$LS[12], 8)
  expect_equal(act$LF[12], 9)

  expect_equal(act$LS[13], 8)
  expect_equal(act$LF[13], 9)

  expect_equal(act$LS[14], 8)
  expect_equal(act$LF[14], 9)

  expect_equal(act$LS[15], 8)
  expect_equal(act$LF[15], 9)

  expect_equal(act$LS[16], 9)
  expect_equal(act$LF[16], 11)

  expect_equal(act$LS[17], 10)
  expect_equal(act$LF[17], 11)

})

test_that("Brings the activities data frame by default!", {
  schedule <- vanhoucke2014_project_1()
  actual <- schedule$activities
  expected <- activities
  expect_equal(actual$id, expected$id)
})

test_that("Brings the activities data frame!", {
  schedule <- vanhoucke2014_project_1()
  actual <- schedule$activities
  expected <- activities
  expect_equal(actual$id, expected$id)
})

test_that("Brings the relations data frame!", {
  schedule <- vanhoucke2014_project_1()
  actual <- schedule$relations
  expected <- relations
  expect_equal(actual$from, expected$from)
  expect_equal(actual$to, expected$to)
})
