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
  sch <- sch_new() %>%
    sch_title("Project 1: Cost Information System") %>%
    sch_reference(
      "VANHOUCKE, Mario. Integrated project management and control:
  first comes the theory, then the practice.
  Gent: Springer, 2014, p. 6"
    ) %>%
    sch_add_activities(
      id = activities$id,
      name = activities$name,
      duration = as.integer(activities$duration)
    ) %>%
    sch_add_relations(
      from = as.integer(relations$from),
      to = as.integer(relations$to)
    ) %>%
    sch_plan()

  return(sch)
}

test_that("Creating a ONE activity schedule with ZERO duration", {
  sch <- sch_new() %>%
    sch_title("A project") %>%
    sch_reference("From criticalpath") %>%
    sch_add_activities(
      id        = 1L,
      name      = "A",
      duration  = 0L
    ) %>%
    sch_plan()

  atb <- sch_activities(sch)

  expect_equal(sch_duration(sch), 0)
  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 0)
  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 0)
  expect_true(atb$critical[1])
})

test_that("Creating a schedule with ONE activity", {
  sch <- sch_new() %>%
    sch_title("A project") %>%
    sch_reference("From criticalpath") %>%
    sch_add_activities(
      id        = 1L,
      name      = "A",
      duration  = 3L
    ) %>%
    sch_plan()

  atb <- sch_activities(sch)

  expect_equal(sch_duration(sch), 3)
  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 3)
  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 3)
  expect_true(atb$critical[1])
})

test_that("Creating a schedule only with activities list, without relations", {
  sch <- sch_new() %>%
    sch_title("A project") %>%
    sch_reference("From criticalpath") %>%
    sch_add_activities(
      id       = c( 1L,  2L,  3L),
      name     = c("A", "B", "C"),
      duration = c( 3L,  2L,  4L)
    ) %>%
    sch_plan()

  atb <- sch_activities(sch)

  expect_equal(sch_duration(sch), 4)

  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 3)
  expect_equal(atb$late_start[1], 1)
  expect_equal(atb$late_finish[1], 4)
  expect_false(atb$critical[1])

  expect_equal(atb$early_start[2], 0)
  expect_equal(atb$early_finish[2], 2)
  expect_equal(atb$late_start[2], 2)
  expect_equal(atb$late_finish[2], 4)
  expect_false(atb$critical[2])

  expect_equal(atb$early_start[3], 0)
  expect_equal(atb$early_finish[3], 4)
  expect_equal(atb$late_start[3], 0)
  expect_equal(atb$late_finish[3], 4)
  expect_true(atb$critical[3])

})

test_that("Schedule duration is 11", {
  sch <- vanhoucke2014_project_1()
  expect_equal(sch_duration(sch), 11)
})

test_that("Schedule critical activities are identified", {
  atb <- vanhoucke2014_project_1() %>%
    sch_critical_activites()

  critical_activities <- paste0(atb$id, collapse=",")
  expected <- paste0(c(1, 2, 4, 11, 16), collapse = ",")
  expect_equal(critical_activities, expected)
})

test_that("Schedule NON critical activities are identified", {
  atb <- vanhoucke2014_project_1() %>%
    sch_non_critical_activites()

  non_critical_activities <- paste0(atb$id, collapse=",")
  expected <- paste0(c(3, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 17), collapse = ",")
  expect_equal(non_critical_activities, expected)
})

test_that("Early Start and Early Finish are correct!", {
  atb <- vanhoucke2014_project_1() %>%
    sch_activities()

  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 1)

  expect_equal(atb$early_start[2], 1)
  expect_equal(atb$early_finish[2], 3)

  expect_equal(atb$early_start[3], 1)
  expect_equal(atb$early_finish[3], 3)

  expect_equal(atb$early_start[4], 3)
  expect_equal(atb$early_finish[4], 7)

  expect_equal(atb$early_start[5], 3)
  expect_equal(atb$early_finish[5], 6)

  expect_equal(atb$early_start[6], 3)
  expect_equal(atb$early_finish[6], 6)

  expect_equal(atb$early_start[7], 3)
  expect_equal(atb$early_finish[7], 6)

  expect_equal(atb$early_start[8], 3)
  expect_equal(atb$early_finish[8], 5)

  expect_equal(atb$early_start[9], 3)
  expect_equal(atb$early_finish[9], 4)

  expect_equal(atb$early_start[10], 3)
  expect_equal(atb$early_finish[10], 4)

  expect_equal(atb$early_start[11], 7)
  expect_equal(atb$early_finish[11], 9)

  expect_equal(atb$early_start[12], 6)
  expect_equal(atb$early_finish[12], 7)

  expect_equal(atb$early_start[13], 5)
  expect_equal(atb$early_finish[13], 6)

  expect_equal(atb$early_start[14], 4)
  expect_equal(atb$early_finish[14], 5)

  expect_equal(atb$early_start[15], 4)
  expect_equal(atb$early_finish[15], 5)

  expect_equal(atb$early_start[16], 9)
  expect_equal(atb$early_finish[16], 11)

  expect_equal(atb$early_start[17], 9)
  expect_equal(atb$early_finish[17], 10)

})

test_that("Late Start and Late Finish are correct!", {
  atb <- vanhoucke2014_project_1() %>%
    sch_activities()

  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 1)

  expect_equal(atb$late_start[2], 1)
  expect_equal(atb$late_finish[2], 3)

  expect_equal(atb$late_start[3], 3)
  expect_equal(atb$late_finish[3], 5)

  expect_equal(atb$late_start[4], 3)
  expect_equal(atb$late_finish[4], 7)

  expect_equal(atb$late_start[5], 4)
  expect_equal(atb$late_finish[5], 7)

  expect_equal(atb$late_start[6], 4)
  expect_equal(atb$late_finish[6], 7)

  expect_equal(atb$late_start[7], 5)
  expect_equal(atb$late_finish[7], 8)

  expect_equal(atb$late_start[8], 6)
  expect_equal(atb$late_finish[8], 8)

  expect_equal(atb$late_start[9], 7)
  expect_equal(atb$late_finish[9], 8)

  expect_equal(atb$late_start[10], 7)
  expect_equal(atb$late_finish[10], 8)

  expect_equal(atb$late_start[11], 7)
  expect_equal(atb$late_finish[11], 9)

  expect_equal(atb$late_start[12], 8)
  expect_equal(atb$late_finish[12], 9)

  expect_equal(atb$late_start[13], 8)
  expect_equal(atb$late_finish[13], 9)

  expect_equal(atb$late_start[14], 8)
  expect_equal(atb$late_finish[14], 9)

  expect_equal(atb$late_start[15], 8)
  expect_equal(atb$late_finish[15], 9)

  expect_equal(atb$late_start[16], 9)
  expect_equal(atb$late_finish[16], 11)

  expect_equal(atb$late_start[17], 10)
  expect_equal(atb$late_finish[17], 11)

})

test_that("Brings the activities data frame!", {
  atb <- vanhoucke2014_project_1() %>%
    sch_activities()
  expected <- activities
  expect_identical(atb$id, expected$id)
})

test_that("Brings the relations data frame!", {
  rtb <- vanhoucke2014_project_1() %>%
    sch_relations("insert")
  expected <- relations
  expect_identical(rtb$from, as.integer(expected$from))
  expect_identical(rtb$to, as.integer(expected$to))
})

