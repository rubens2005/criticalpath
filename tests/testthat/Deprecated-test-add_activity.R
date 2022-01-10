test_that("Add activities works, with duration equal zero!", {
  schedule <- Schedule$new()

  schedule$add_activity(1)
  expect_equal(schedule$nr_activities, 1)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 0)

  schedule$add_activity(2)
  expect_equal(schedule$nr_activities, 2)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 0)

  schedule$add_activity(3)
  expect_equal(schedule$nr_activities, 3)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 0)

})

test_that("Add activities works, with duration", {
  schedule <- Schedule$new()

  schedule$add_activity(1, "A", 3)
  expect_equal(schedule$nr_activities, 1)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 3)

  schedule$add_activity(2, "B", 2)
  expect_equal(schedule$nr_activities, 2)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 3)

  schedule$add_activity(3, "C", 4)
  expect_equal(schedule$nr_activities, 3)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$duration, 4)

})


test_that("Add activities works, with mean duration equal SIX", {
  schedule <- Schedule$new()

  schedule$add_activity(1, "Task 1", 5)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 1)
  expect_equal(schedule$duration, 5)

  schedule$add_activity(2, "Task 2", 6)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 2)
  expect_equal(schedule$duration, 6)

  schedule$add_activity(3, "Task 3", 8)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 3)
  expect_equal(schedule$duration, 8)

  schedule$add_activity(4, "Task 4", 6)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 4)
  expect_equal(schedule$duration, 8)

  schedule$add_activity(5, "Task 5", 9)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 5)
  expect_equal(schedule$duration, 9)

  schedule$add_activity(6, "Task 6", 3)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 6)
  expect_equal(schedule$duration, 9)

  schedule$add_activity(7, "Task 7", 4)
  expect_true(schedule$has_any_activity)
  expect_equal(schedule$nr_activities, 7)
  expect_equal(schedule$duration, 9)

})

test_that("Add activities is equal to new Schedule with activities", {
  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
  )
  base <- Schedule$new(activities)

  schedule <- Schedule$new()
  schedule$add_activities(activities)

  expect_equal(base$activities$id, schedule$activities$id)
  expect_equal(base$activities$name, schedule$activities$name)
  expect_equal(base$activities$duration, schedule$activities$duration)

})
