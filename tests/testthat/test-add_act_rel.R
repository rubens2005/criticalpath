vanhoucke_2009_p18 <- function() {
  schedule <- make_empty_schedule(
    "Fictitious Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"
  )

  schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4))
  schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
  schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
  schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6))
  schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
  schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7))
  schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
  schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
  schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
  schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
  schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
  schedule <- add_act_rel(schedule, 12, "a12", 0)
  schedule
}

test_that("Creating a schedule step-by-step, with activities and relations together", {
  schedule <- vanhoucke_2009_p18()
  activities <- schedule$activities[order(schedule$activities$id), ]

  expect_equal(schedule$info$duration, 16)

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
