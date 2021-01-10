test_that("Inconsistences are identified", {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <- "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"

  schedule$add_act_rel( 1, "a1" , 0, c(2,3,4,8))
  schedule$add_act_rel( 2, "a2" , 4, c(5))
  schedule$add_act_rel( 3, "a3" , 9, c(10))
  schedule$add_act_rel( 4, "a4" , 1, c(6,7))
  schedule$add_act_rel( 5, "a5" , 4, c(9))
  schedule$add_act_rel( 6, "a6" , 5, c(7,11))
  schedule$add_act_rel( 7, "a7" , 1, c(8,11))
  schedule$add_act_rel( 8, "a8" , 7, c(12))
  schedule$add_act_rel( 9, "a9" , 8, c(12))
  schedule$add_act_rel(10, "a10", 3, c(12))
  schedule$add_act_rel(11, "a11", 3, c(12))
  schedule$add_act_rel(12, "a12", 0)

  expect_error(schedule$is_redundant(1, 999))
  expect_error(schedule$is_redundant(999, 2))
  expect_error(schedule$is_redundant(2, 1))

})

test_that("Redundant relations are identified.", {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <-"VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18"

  schedule$add_act_rel( 1, "a1" , 0, c(2,3,4,8))
  schedule$add_act_rel( 2, "a2" , 4, c(5))
  schedule$add_act_rel( 3, "a3" , 9, c(10))
  schedule$add_act_rel( 4, "a4" , 1, c(6,7))
  schedule$add_act_rel( 5, "a5" , 4, c(9))
  schedule$add_act_rel( 6, "a6" , 5, c(7,11))
  schedule$add_act_rel( 7, "a7" , 1, c(8,11))
  schedule$add_act_rel( 8, "a8" , 7, c(12))
  schedule$add_act_rel( 9, "a9" , 8, c(12))
  schedule$add_act_rel(10, "a10", 3, c(12))
  schedule$add_act_rel(11, "a11", 3, c(12))
  schedule$add_act_rel(12, "a12", 0)

  expect_false(schedule$is_redundant(1, 2))
  expect_false(schedule$is_redundant(1, 3))
  expect_false(schedule$is_redundant(1, 4))
  expect_true (schedule$is_redundant(1, 8))
  expect_false(schedule$is_redundant(2, 5))
  expect_false(schedule$is_redundant(3, 10))
  expect_false(schedule$is_redundant(4, 6))
  expect_true (schedule$is_redundant(4, 7))
  expect_false(schedule$is_redundant(5, 9))
  expect_false(schedule$is_redundant(6, 7))
  expect_true (schedule$is_redundant(6, 11))
  expect_false(schedule$is_redundant(7, 8))
  expect_false(schedule$is_redundant(7, 11))
  expect_false(schedule$is_redundant(8, 12))
  expect_false(schedule$is_redundant(9, 12))
  expect_false(schedule$is_redundant(10, 12))
  expect_false(schedule$is_redundant(11, 12))

})
