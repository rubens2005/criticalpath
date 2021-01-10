test_that("all successors list corrects activities", {
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

  result <- sort(schedule$all_successors(1))
  expect_equal(result, c(2,3,4,5,6,7,8,9,10,11,12))

  result <- sort(schedule$all_successors(2))
  expect_equal(result, c(5, 9, 12))

  result <- sort(schedule$all_successors(3))
  expect_equal(result, c(10, 12))

  result <- sort(schedule$all_successors(4))
  expect_equal(result, c(6, 7, 8, 11, 12))

  result <- sort(schedule$all_successors(5))
  expect_equal(result, c(9, 12))

  result <- sort(schedule$all_successors(6))
  expect_equal(result, c(7, 8, 11, 12))

  result <- sort(schedule$all_successors(7))
  expect_equal(result, c(8, 11, 12))

  result <- sort(schedule$all_successors(8))
  expect_equal(result, c(12))

  result <- sort(schedule$all_successors(9))
  expect_equal(result, c(12))

  result <- sort(schedule$all_successors(10))
  expect_equal(result, c(12))

  result <- sort(schedule$all_successors(11))
  expect_equal(result, c(12))

  result <- sort(schedule$all_successors(12))
  expect_equal(result, numeric())

})
