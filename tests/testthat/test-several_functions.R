test_that("schedule is a Schedule.", {
  schedule <- make_empty_schedule()
  expect_true(is_schedule(schedule))
})

test_that("s is NOT a Schedule.", {
  schedule <- list(any="thing")
  expect_false(is_schedule(schedule))
})

test_that("Assert that is a Schedule object.", {
  schedule <- make_empty_schedule()
  expect_silent(assert_is_schedule(schedule))
})

test_that("s is NOT a Schedule.", {
  schedule <- list(any="thing")
  expect_error(assert_is_schedule_object(schedule))
})

