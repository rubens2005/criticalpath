test_that("all successors list corrects activities", {
  schedule <- make_empty_schedule(
    "Fictitious Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"
  )

  schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4, 8))
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

  result <- sort(all_successors(schedule, 1))
  expect_equal(result, c(2,3,4,5,6,7,8,9,10,11,12))

  result <- sort(all_successors(schedule, 2))
  expect_equal(result, c(5, 9, 12))

  result <- sort(all_successors(schedule, 3))
  expect_equal(result, c(10, 12))

  result <- sort(all_successors(schedule, 4))
  expect_equal(result, c(6, 7, 8, 11, 12))

  result <- sort(all_successors(schedule, 5))
  expect_equal(result, c(9, 12))

  result <- sort(all_successors(schedule, 6))
  expect_equal(result, c(7, 8, 11, 12))

  result <- sort(all_successors(schedule, 7))
  expect_equal(result, c(8, 11, 12))

  result <- sort(all_successors(schedule, 8))
  expect_equal(result, c(12))

  result <- sort(all_successors(schedule, 9))
  expect_equal(result, c(12))

  result <- sort(all_successors(schedule, 10))
  expect_equal(result, c(12))

  result <- sort(all_successors(schedule, 11))
  expect_equal(result, c(12))

  result <- sort(all_successors(schedule, 12))
  expect_equal(result, numeric())

})
