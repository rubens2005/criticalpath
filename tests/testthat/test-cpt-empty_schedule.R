test_that("Schedule is empty.", {
  sch <- sch_new()

  expect_error(
    sch_plan(sch)
  )

  expect_true(cpt_is_schedule_empty(sch))
  expect_equal(sch_title(sch), "")
  expect_equal(sch_reference(sch), "")

  expect_false(sch_has_any_activity(sch))
  expect_equal(sch_nr_activities(sch), 0L)

  expect_false(sch_has_any_relation(sch))
  expect_equal(sch_nr_relations(sch), 0L)

  expect_error(
    sch_duration(sch)
  )
})

test_that("Not integer id is detected.", {
  expect_error({
    sch <- sch_new() %>%
      create_activity_tib(
        id = c(1, 2)
      )
  })
})

test_that("Integer id is correct.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id = c(1L, 2L),
      name = c("AA","BB"),
      duration = c(2L, 5L)
    )
  expect_false(cpt_is_schedule_empty(sch))
})

test_that("Schedule is not empty.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id = c(1L, 2L),
      name = c("AA","BB"),
      duration = c(2L, 5L)
    )
  expect_false(cpt_is_schedule_empty(sch))
  expect_true(cpt_is_schedule_with_some_activity(sch))
})

test_that("Activities tibble have extras columns.", {
  sch <- sch_new() %>%
    sch_add_activities(
      id = c(1L, 2L),
      name = c("AA","BB"),
      duration = c(2L, 5L),
      resource = "Rubens",
      cost = c(12.34, 56.78)
    )
  expect_false(cpt_is_schedule_empty(sch))
  expect_true(cpt_is_schedule_with_some_activity(sch))

  expect_true(all(sch_activities(sch)$resource == c("Rubens", "Rubens")))
  expect_true(all(sch_activities(sch)$cost == c(12.34, 56.78)))
})

