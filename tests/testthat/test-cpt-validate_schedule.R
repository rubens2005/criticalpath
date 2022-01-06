test_that("There is no error.", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L)

  validation <- sch_validate(sch)
  expect_true(validation$is_valid)
  expect_false(validation$is_error_with_activities)
  expect_false(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_silent(
    sch1 <- sch_plan(sch)
  )
})

test_that("Identify activity id duplicated.", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L) %>%
    sch_add_activity(5L, "Duplicated Task 5", 9L) %>%
    sch_add_activity(2L, "Duplicated Task 2", 6L)

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_true(validation$is_error_with_activities)
  expect_false(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )

})

test_that("Identify `from` and `to` do not exist.", {
  s <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L)

  # Id 'from' does not exist.
  sch <- s %>%
    sch_add_relation(99L, 5L)

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_false(validation$is_error_with_activities)
  expect_true(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )


  # Id 'to' does not exist.
  sch <- s %>%
    sch_add_relation(5L, 99L)

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_false(validation$is_error_with_activities)
  expect_true(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )

  # Id "from"  and "to" does not exist.
  sch <- s %>%
    sch_add_relation(88L, 99L)

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_false(validation$is_error_with_activities)
  expect_true(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )
})

test_that("Identify relation `from -> to` is duplicated.", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L) %>%
    sch_add_relation(1L, 3L) %>% # Duplicated!!
    sch_add_relation(5L, 6L)     # Duplicated!!

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_false(validation$is_error_with_activities)
  expect_true(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )

})

test_that("Identify a cycle.", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L) %>%
    sch_add_relation(7L, 3L) # cycle

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)

  expect_false(validation$is_error_with_activities)
  expect_false(validation$is_error_with_relations)
  expect_true(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )

})

test_that("Identify activity and relations are invalids.", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
    sch_add_activity(2L, "Task 2", 6L, 4L) %>%
    sch_add_activity(3L, "Task 3", 8L, 5L) %>%
    sch_add_activity(4L, "Task 4", 6L, 6L) %>%
    sch_add_activity(5L, "Task 5", 9L, 6L) %>%
    sch_add_activity(6L, "Task 6", 3L, 7L) %>%
    sch_add_activity(7L, "Task 7", 4L) %>%
    sch_add_activity(5L, "Duplicated Task 5", 9L) %>%
    sch_add_activity(2L, "Duplicated Task 2", 6L) %>%
    sch_add_relation(5L, 99L) %>%
    sch_add_relation(77L, 5L) %>%
    sch_add_relation(1L, 3L) %>%
    sch_add_relation(88L, 66L) %>%
    sch_add_relation(5L, 6L)

  validation <- sch_validate(sch)
  expect_false(validation$is_valid)
  expect_true(validation$is_error_with_activities)
  expect_true(validation$is_error_with_relations)
  expect_false(validation$is_error_with_dag)

  expect_error(
    sch1 <- sch_plan(sch)
  )

})
