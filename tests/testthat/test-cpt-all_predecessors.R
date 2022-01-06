test_that("all predecessors list corrects activities", {
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference(
      "VANHOUCKE, Mario. Measuring time:
    improving project performance using earned value management.
    Gent: Springer, 2009, p. 18"
    ) %>%
    sch_add_activity( 1L, "a1" , 0L, 2,3,4, 8) %>%
    sch_add_activity( 2L, "a2" , 4L, 5) %>%
    sch_add_activity( 3L, "a3" , 9L, 10) %>%
    sch_add_activity( 4L, "a4" , 1L, 6,7) %>%
    sch_add_activity( 5L, "a5" , 4L, 9) %>%
    sch_add_activity( 6L, "a6" , 5L, 7,11) %>%
    sch_add_activity( 7L, "a7" , 1L, 8,11) %>%
    sch_add_activity( 8L, "a8" , 7L, 12) %>%
    sch_add_activity( 9L, "a9" , 8L, 12) %>%
    sch_add_activity(10L, "a10", 3L, 12) %>%
    sch_add_activity(11L, "a11", 3L, 12) %>%
    sch_add_activity(12L, "a12", 0L)

  result <- sort(sch_all_predecessors(sch, 1))
  expect_equal(result, numeric())

  result <- sort(sch_all_predecessors(sch, 2))
  expect_equal(result, c(1))

  result <- sort(sch_all_predecessors(sch, 3))
  expect_equal(result, c(1))

  result <- sort(sch_all_predecessors(sch, 4))
  expect_equal(result, c(1))

  result <- sort(sch_all_predecessors(sch, 5))
  expect_equal(result, c(1, 2))

  result <- sort(sch_all_predecessors(sch, 6))
  expect_equal(result, c(1, 4))

  result <- sort(sch_all_predecessors(sch, 7))
  expect_equal(result, c(1, 4, 6))

  result <- sort(sch_all_predecessors(sch, 8))
  expect_equal(result, c(1, 4, 6, 7))

  result <- sort(sch_all_predecessors(sch, 9))
  expect_equal(result, c(1, 2, 5))

  result <- sort(sch_all_predecessors(sch, 10))
  expect_equal(result, c(1, 3))

  result <- sort(sch_all_predecessors(sch, 11))
  expect_equal(result, c(1, 4, 6, 7))

  result <- sort(sch_all_predecessors(sch, 12))
  expect_equal(result, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

})
