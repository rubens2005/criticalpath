###
# Testing tibbles' create methods without Schedule plan
###


test_that("Adding activities to an empty schedule.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:6,
      name      = paste("a", as.character(1:6), sep = ""),
      duration  = c(7L, 8L, 6L, 4L, 9L, 3L)
    )

  atb <- sch_activities(sch)
  expect_equal(atb$id, 1:6)
  expect_equal(atb$name, paste("a", as.character(1:6), sep = ""))
  expect_equal(atb$duration, c(7L, 8L, 6L, 4L, 9L, 3L))
})

test_that("Adding activities to a schedule with activities.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:3,
      name      = paste("a", as.character(1:3), sep = ""),
      duration  = c(7L, 8L, 6L)
    ) %>%
    sch_add_activities(
      id        = 4:6,
      name      = paste("a", as.character(4:6), sep = ""),
      duration  = c(4L, 9L, 3L)
    )

  atb <- sch_activities(sch)
  expect_equal(atb$id, 1:6)
  expect_equal(atb$name, paste("a", as.character(1:6), sep = ""))
  expect_equal(atb$duration, c(7L, 8L, 6L, 4L, 9L, 3L))
})

test_that("Adding activities, resources and cost to a schedule with activities (1).", {
  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:3,
      name      = paste("a", as.character(1:3), sep = ""),
      duration  = c(7L, 8L, 6L),
      resource  = c("Rubens", "José", "Rosa"),
      cost      = 123.45
    ) %>%
    sch_add_activities(
      id        = 4:6,
      name      = paste("a", as.character(4:6), sep = ""),
      duration  = c(4L, 9L, 3L)
    )

  atb <- sch_activities(sch)
  expect_equal(atb$resource, c("Rubens", "José", "Rosa", NA, NA, NA))
  expect_equal(atb$cost, c(123.45, 123.45, 123.45, NA, NA, NA))
})

test_that("Adding activities, resources and cost to a schedule with activities (2).", {
  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:3,
      name      = paste("a", as.character(1:3), sep = ""),
      duration  = c(7L, 8L, 6L)
    ) %>%
    sch_add_activities(
      id        = 4:6,
      name      = paste("a", as.character(4:6), sep = ""),
      duration  = c(4L, 9L, 3L),
      resource  = c("Rubens", "José", "Rosa"),
      cost      = 123.45
    )

  atb <- sch_activities(sch)
  expect_equal(atb$resource, c(NA, NA, NA, "Rubens", "José", "Rosa"))
  expect_equal(atb$cost, c(NA, NA, NA, 123.45, 123.45, 123.45))
})

test_that("Adding relations to an empty schedule.", {
  sch <- sch_new() %>%
    sch_add_relations(
      from = c(1L, 1L, 2L, 3L, 4L, 5L),
      to   = c(2L, 3L, 4L, 5L, 6L, 6L)
    )

  rtb <- sch_relations(sch)
  expect_equal(rtb$from, c(1L, 1L, 2L, 3L, 4L, 5L))
  expect_equal(rtb$to, c(2L, 3L, 4L, 5L, 6L, 6L))
  expect_equal(rtb$type, rep("FS", 6))
  expect_equal(rtb$lag, rep(0, 6))
})


test_that("Adding relations to a schedule with relations", {
  sch <- sch_new() %>%
    sch_add_relations(
      from = c(1L, 1L, 2L),
      to   = c(2L, 3L, 4L)
    ) %>%
    sch_add_relations(
      from = c(3L, 4L, 5L),
      to   = c(5L, 6L, 6L)
    )

  rtb <- sch_relations(sch)
  expect_equal(rtb$from, c(1L, 1L, 2L, 3L, 4L, 5L))
  expect_equal(rtb$to, c(2L, 3L, 4L, 5L, 6L, 6L))
  expect_equal(rtb$type, rep("FS", 6))
  expect_equal(rtb$lag, rep(0L, 6))
})



###
# Test Schedule from tibbles, with schedule plan and CPM calculation
###

# This is a transcription of [test-schedule_from_data_frame.R]

test_that("Create a complete schedule.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:6,
      name      = paste("a", as.character(1:6), sep = ""),
      duration  = c(7L, 8L, 6L, 4L, 9L, 3L)
    ) %>%
    sch_add_relations(
      from = c(1L, 1L, 2L, 3L, 4L, 5L),
      to   = c(2L, 3L, 4L, 5L, 6L, 6L)
    ) %>%
    sch_plan()

  duration <- sch_duration(sch)
  expect_true(is.integer(duration))
  expect_equal(duration, 25L)


  sch <- sch_new() %>%
    sch_add_activities(
      id        = 1:6,
      name      = paste("a", as.character(1:6), sep = ""),
      duration  = c(0L, 8L, 6L, 4L, 9L, 0L)
    ) %>%
    sch_add_relations(
      from = c(1L, 1L, 2L, 3L, 4L, 5L),
      to   = c(2L, 3L, 4L, 5L, 6L, 6L)
    ) %>%
    sch_plan()

  duration <- sch_duration(sch)
  expect_true(is.integer(duration))
  expect_equal(duration, 15L)
})

