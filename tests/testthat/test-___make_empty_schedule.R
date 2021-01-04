test_that("Empty schedule is, well, an empty schedule.", {
  schedule <- Schedule$new()

  expect_error((schedule$nr_activities = 20))
  expect_error((schedule$nr_relations = 20))
  expect_error((schedule$has_any_activity = TRUE))
  expect_error((schedule$has_any_relation = TRUE))


  expect_equal(schedule$nr_activities, 0)
  expect_equal(schedule$nr_relations, 0)

  expect_false(schedule$has_any_activity)
  expect_false(schedule$has_any_relation)

})
