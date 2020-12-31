test_that("Empty schedule is, well, an empty schedule.", {
  schedule <- make_empty_schedule()

  expect_length(schedule$activities$id, 0)
  expect_length(schedule$relations$from, 0)

  expect_equal(schedule$info$nr_activities, 0)
  expect_equal(schedule$info$nr_relations, 0)

  expect_false(schedule$info$has_any_activity)
  expect_false(schedule$info$has_any_relation)

})
