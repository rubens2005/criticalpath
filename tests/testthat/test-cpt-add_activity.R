test_that("Add activities works, with duration equal zero!", {
  sch <- sch_new()

  sch %<>%
    sch_add_activity(1L, "1", 0L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 1)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 0)

  sch %<>%
    sch_add_activity(2L, "2", 0L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 2)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 0)

  sch %<>%
    sch_add_activity(3L, "3", 0L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 3)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 0)
})

test_that("Add activities works, with duration", {
  sch <- sch_new()

  sch %<>%
    sch_add_activity(1L, "A", 3L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 1)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 3)

  sch %<>%
    sch_add_activity(2L, "B", 2L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 2)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 3)

  sch %<>%
    sch_add_activity(3L, "C", 4L) %>%
    sch_plan()
  expect_equal(sch_nr_activities(sch), 3)
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_duration(sch), 4)

})


test_that("Add activities works, with mean duration equal SIX", {
  sch <- sch_new()

  sch %<>% sch_add_activity(1L, "Task 1", 5L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 1)
  expect_equal(sch_duration(sch), 5)

  sch %<>% sch_add_activity(2L, "Task 2", 6L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 2)
  expect_equal(sch_duration(sch), 6)

  sch %<>% sch_add_activity(3L, "Task 3", 8L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 3)
  expect_equal(sch_duration(sch), 8)

  sch %<>% sch_add_activity(4L, "Task 4", 6L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 4)
  expect_equal(sch_duration(sch), 8)

  sch %<>% sch_add_activity(5L, "Task 5", 9L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 5)
  expect_equal(sch_duration(sch), 9)

  sch %<>% sch_add_activity(6L, "Task 6", 3L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 6)
  expect_equal(sch_duration(sch), 9)

  sch %<>% sch_add_activity(7L, "Task 7", 4L) %>% sch_plan()
  expect_true(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 7)
  expect_equal(sch_duration(sch), 9)
})

