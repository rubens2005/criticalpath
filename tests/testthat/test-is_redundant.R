test_that("Inconsistences are identified", {
  schedule <- make_empty_schedule(
    "Fictitious Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"
  )

  schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4,8))
  schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
  schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
  schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6,7))
  schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
  schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7,11))
  schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
  schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
  schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
  schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
  schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
  schedule <- add_act_rel(schedule, 12, "a12", 0)

  expect_error(is_redundant(schedule, 1, 999))
  expect_error(is_redundant(schedule, 999, 2))
  expect_error(is_redundant(schedule, 2, 1))

})

test_that("Redundant relations are identified.", {
  schedule <- make_empty_schedule(
    "Fictitious Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"
  )

  schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4,8))
  schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
  schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
  schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6,7))
  schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
  schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7,11))
  schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
  schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
  schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
  schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
  schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
  schedule <- add_act_rel(schedule, 12, "a12", 0)

  expect_false(is_redundant(schedule, 1, 2))
  expect_false(is_redundant(schedule, 1, 3))
  expect_false(is_redundant(schedule, 1, 4))
  expect_true(is_redundant(schedule, 1, 8))
  expect_false(is_redundant(schedule, 2, 5))
  expect_false(is_redundant(schedule, 3, 10))
  expect_false(is_redundant(schedule, 4, 6))
  expect_true(is_redundant(schedule, 4, 7))
  expect_false(is_redundant(schedule, 5, 9))
  expect_false(is_redundant(schedule, 6, 7))
  expect_true(is_redundant(schedule, 6, 11))
  expect_false(is_redundant(schedule, 7, 8))
  expect_false(is_redundant(schedule, 7, 11))
  expect_false(is_redundant(schedule, 8, 12))
  expect_false(is_redundant(schedule, 9, 12))
  expect_false(is_redundant(schedule, 10, 12))
  expect_false(is_redundant(schedule, 11, 12))

})
