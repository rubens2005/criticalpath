test_that("Inconsistences are identified", {
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18") %>%
    sch_add_activity( 1L, "a1" , 0L, 2,3,4,8) %>%
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

  expect_error(sch_is_redundant(sch, 1, 999))
  expect_error(sch_is_redundant(sch, 999, 2))
  expect_error(sch_is_redundant(sch, 2, 1))

})

test_that("Redundant relations are identified.", {
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18") %>%
    sch_add_activity( 1L, "a1" , 0L, 2,3,4,8) %>%
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

  expect_false(sch_is_redundant(sch, 1, 2))
  expect_false(sch_is_redundant(sch, 1, 3))
  expect_false(sch_is_redundant(sch, 1, 4))
  expect_true (sch_is_redundant(sch, 1, 8))
  expect_false(sch_is_redundant(sch, 2, 5))
  expect_false(sch_is_redundant(sch, 3, 10))
  expect_false(sch_is_redundant(sch, 4, 6))
  expect_true (sch_is_redundant(sch, 4, 7))
  expect_false(sch_is_redundant(sch, 5, 9))
  expect_false(sch_is_redundant(sch, 6, 7))
  expect_true (sch_is_redundant(sch, 6, 11))
  expect_false(sch_is_redundant(sch, 7, 8))
  expect_false(sch_is_redundant(sch, 7, 11))
  expect_false(sch_is_redundant(sch, 8, 12))
  expect_false(sch_is_redundant(sch, 9, 12))
  expect_false(sch_is_redundant(sch, 10, 12))
  expect_false(sch_is_redundant(sch, 11, 12))

})
