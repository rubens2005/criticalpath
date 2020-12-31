test_that("Add activities works, with duration equal zero!", {
  schedule <- make_empty_schedule()

  schedule <- add_activity(schedule, 1)
  expect_equal(schedule$info$nr_activities, 1)
  expect_true(schedule$info$has_any_activity)

  schedule <- add_activity(schedule, 2)
  expect_equal(schedule$info$nr_activities, 2)
  expect_true(schedule$info$has_any_activity)

  schedule <- add_activity(schedule, 3)
  expect_equal(schedule$info$nr_activities, 3)
  expect_true(schedule$info$has_any_activity)

})

test_that("Add activities works, with duration", {
  schedule <- make_empty_schedule()

  schedule <- add_activity(schedule, 1, "A", 3)
  expect_equal(schedule$info$nr_activities, 1)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$duration, 3)


  schedule <- add_activity(schedule, 2, "B", 2)
  expect_equal(schedule$info$nr_activities, 2)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$duration, 3)

  schedule <- add_activity(schedule, 3, "C", 4)
  expect_equal(schedule$info$nr_activities, 3)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$duration, 4)

})


test_that("Add activities works, with mean duration equal SIX", {
  schedule <- make_empty_schedule()

  schedule <- add_activity(schedule, 1, "Task 1", 5)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 1)
  expect_equal(schedule$info$duration, 5)


  schedule <- add_activity(schedule, 2, "Task 2", 6)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 2)
  expect_equal(schedule$info$duration, 6)

  schedule <- add_activity(schedule, 3, "Task 3", 8)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 3)
  expect_equal(schedule$info$duration, 8)

  schedule <- add_activity(schedule, 4, "Task 4", 6)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 4)
  expect_equal(schedule$info$duration, 8)

  schedule <- add_activity(schedule, 5, "Task 5", 9)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 5)
  expect_equal(schedule$info$duration, 9)

  schedule <- add_activity(schedule, 6, "Task 6", 3)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 6)
  expect_equal(schedule$info$duration, 9)

  schedule <- add_activity(schedule, 7, "Task 7", 4)
  expect_true(schedule$info$has_any_activity)
  expect_equal(schedule$info$nr_activities, 7)
  expect_equal(schedule$info$duration, 9)

})


