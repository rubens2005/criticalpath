vanhoucke_2009_p18 <- function() {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <- "VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18"

  schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
  schedule$add_act_rel(  2, "a2" , 4, c(5))
  schedule$add_act_rel(  3, "a3" , 9, c(10))
  schedule$add_act_rel(  4, "a4" , 1, c(6))
  schedule$add_act_rel(  5, "a5" , 4, c(9))
  schedule$add_act_rel(  6, "a6" , 5, c(7))
  schedule$add_act_rel(  7, "a7" , 1, c(8,11))
  schedule$add_act_rel(  8, "a8" , 7, c(12))
  schedule$add_act_rel(  9, "a9" , 8, c(12))
  schedule$add_act_rel( 10, "a10", 3, c(12))
  schedule$add_act_rel( 11, "a11", 3, c(12))
  schedule$add_act_rel( 12, "a12", 0)
  schedule
}

test_that("Creating a schedule step-by-step, with activities and relations together", {
  schedule <- vanhoucke_2009_p18()
  activities <- schedule$activities

  expect_equal(schedule$duration, 16)

  expect_equal(activities$ES[1], 0)
  expect_equal(activities$EF[1], 0)
  expect_equal(activities$LS[1], 0)
  expect_equal(activities$LF[1], 0)
  expect_true(activities$critical[1])

  expect_equal(activities$ES[2], 0)
  expect_equal(activities$EF[2], 4)
  expect_equal(activities$LS[2], 0)
  expect_equal(activities$LF[2], 4)
  expect_true(activities$critical[2])

  expect_equal(activities$ES[3], 0)
  expect_equal(activities$EF[3], 9)
  expect_equal(activities$LS[3], 4)
  expect_equal(activities$LF[3], 13)
  expect_false(activities$critical[3])

  expect_equal(activities$ES[4], 0)
  expect_equal(activities$EF[4], 1)
  expect_equal(activities$LS[4], 2)
  expect_equal(activities$LF[4], 3)
  expect_false(activities$critical[4])

  expect_equal(activities$ES[5], 4)
  expect_equal(activities$EF[5], 8)
  expect_equal(activities$LS[5], 4)
  expect_equal(activities$LF[5], 8)
  expect_true(activities$critical[5])

  expect_equal(activities$ES[6], 1)
  expect_equal(activities$EF[6], 6)
  expect_equal(activities$LS[6], 3)
  expect_equal(activities$LF[6], 8)
  expect_false(activities$critical[6])

  expect_equal(activities$ES[7], 6)
  expect_equal(activities$EF[7], 7)
  expect_equal(activities$LS[7], 8)
  expect_equal(activities$LF[7], 9)
  expect_false(activities$critical[7])

  expect_equal(activities$ES[8], 7)
  expect_equal(activities$EF[8], 14)
  expect_equal(activities$LS[8], 9)
  expect_equal(activities$LF[8], 16)
  expect_false(activities$critical[8])

  expect_equal(activities$ES[9], 8)
  expect_equal(activities$EF[9], 16)
  expect_equal(activities$LS[9], 8)
  expect_equal(activities$LF[9], 16)
  expect_true(activities$critical[9])

  expect_equal(activities$ES[10], 9)
  expect_equal(activities$EF[10], 12)
  expect_equal(activities$LS[10], 13)
  expect_equal(activities$LF[10], 16)
  expect_false(activities$critical[10])

  expect_equal(activities$ES[11], 7)
  expect_equal(activities$EF[11], 10)
  expect_equal(activities$LS[11], 13)
  expect_equal(activities$LF[11], 16)
  expect_false(activities$critical[11])

  expect_equal(activities$ES[12], 16)
  expect_equal(activities$EF[12], 16)
  expect_equal(activities$LS[12], 16)
  expect_equal(activities$LF[12], 16)
  expect_true(activities$critical[12])


})


test_that("Creating a schedule step-by-step, with activities and relations together: float tests", {
  schedule <- vanhoucke_2009_p18()
  activities <- schedule$activities

  expect_equal(activities$total_float[1], 0)
  expect_equal(activities$total_float[2], 0)
  expect_equal(activities$total_float[3], 4)
  expect_equal(activities$total_float[4], 2)
  expect_equal(activities$total_float[5], 0)
  expect_equal(activities$total_float[6], 2)
  expect_equal(activities$total_float[7], 2)
  expect_equal(activities$total_float[8], 2)
  expect_equal(activities$total_float[9], 0)
  expect_equal(activities$total_float[10], 4)
  expect_equal(activities$total_float[11], 6)
  expect_equal(activities$total_float[12], 0)

})

test_that("Does not return Inf!!!", {
  # Create a schedule
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <- "VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18"

  # Add activities and relations to it.
  schedule$add_act_rel(  2, "a2" , 4, c(5))
  schedule$add_act_rel(  3, "a3" , 9, c(10))
  schedule$add_act_rel(  4, "a4" , 1, c(6))
  schedule$add_act_rel(  5, "a5" , 4, c(9))


  schedule$add_act_rel(  6, "a6" , 5, c(7))
  schedule$add_act_rel(  7, "a7" , 1, c(8,11))
  schedule$add_act_rel(  8, "a8" , 7, c(12))
  schedule$add_act_rel(  9, "a9" , 8, c(12))
  schedule$add_act_rel( 10, "a10", 3, c(12))
  schedule$add_act_rel( 11, "a11", 3, c(12))
  schedule$add_act_rel( 12, "a12", 0)

  expect_true(all(!is.infinite(schedule$activities$LS)))
  expect_true(all(!is.infinite(schedule$activities$LF)))

  expect_true(!is.na(schedule$topoi_sp()))
  expect_true(!is.na(schedule$topoi_ad()))
  expect_true(!is.na(schedule$topoi_la()))
  expect_true(!is.na(schedule$topoi_tf()))

  expect_equal(schedule$duration, 16)

  id <- 2
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 0)
  expect_equal(atv$EF, 4)
  expect_equal(atv$LS, 0)
  expect_equal(atv$LF, 4)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 3
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 0)
  expect_equal(atv$EF, 9)
  expect_equal(atv$LS, 4)
  expect_equal(atv$LF, 13)
  expect_equal(atv$total_float, 4)
  expect_equal(atv$free_float, 0)

  id <- 4
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 0)
  expect_equal(atv$EF, 1)
  expect_equal(atv$LS, 2)
  expect_equal(atv$LF, 3)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 5
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 4)
  expect_equal(atv$EF, 8)
  expect_equal(atv$LS, 4)
  expect_equal(atv$LF, 8)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 6
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 1)
  expect_equal(atv$EF, 6)
  expect_equal(atv$LS, 3)
  expect_equal(atv$LF, 8)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 7
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 6)
  expect_equal(atv$EF, 7)
  expect_equal(atv$LS, 8)
  expect_equal(atv$LF, 9)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 8
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 7)
  expect_equal(atv$EF, 14)
  expect_equal(atv$LS, 9)
  expect_equal(atv$LF, 16)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 2)

  id <- 9
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 8)
  expect_equal(atv$EF, 16)
  expect_equal(atv$LS, 8)
  expect_equal(atv$LF, 16)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 10
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 9)
  expect_equal(atv$EF, 12)
  expect_equal(atv$LS, 13)
  expect_equal(atv$LF, 16)
  expect_equal(atv$total_float, 4)
  expect_equal(atv$free_float, 4)

  id <- 11
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 7)
  expect_equal(atv$EF, 10)
  expect_equal(atv$LS, 13)
  expect_equal(atv$LF, 16)
  expect_equal(atv$total_float, 6)
  expect_equal(atv$free_float, 6)

  id <- 12
  atv <- schedule$get_activity(id)
  expect_equal(atv$ES, 16)
  expect_equal(atv$EF, 16)
  expect_equal(atv$LS, 16)
  expect_equal(atv$LF, 16)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)


})
