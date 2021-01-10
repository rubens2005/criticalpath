test_that("all predecessors list corrects activities", {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <-
    "VANHOUCKE, Mario. Measuring time:
    improving project performance using earned value management.
    Gent: Springer, 2009, p. 18"


  schedule$add_act_rel( 1, "a1" , 0, c(2,3,4, 8))
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

  result <- sort(schedule$all_predecessors(1))
  expect_equal(result, numeric())

  result <- sort(schedule$all_predecessors(2))
  expect_equal(result, c(1))

  result <- sort(schedule$all_predecessors(3))
  expect_equal(result, c(1))

  result <- sort(schedule$all_predecessors(4))
  expect_equal(result, c(1))

  result <- sort(schedule$all_predecessors(5))
  expect_equal(result, c(1, 2))

  result <- sort(schedule$all_predecessors(6))
  expect_equal(result, c(1, 4))

  result <- sort(schedule$all_predecessors(7))
  expect_equal(result, c(1, 4, 6))

  result <- sort(schedule$all_predecessors(8))
  expect_equal(result, c(1, 4, 6, 7))

  result <- sort(schedule$all_predecessors(9))
  expect_equal(result, c(1, 2, 5))

  result <- sort(schedule$all_predecessors(10))
  expect_equal(result, c(1, 3))

  result <- sort(schedule$all_predecessors(11))
  expect_equal(result, c(1, 4, 6, 7))

  result <- sort(schedule$all_predecessors(12))
  expect_equal(result, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))

})
