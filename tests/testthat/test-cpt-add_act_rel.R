vanhoucke_2009_p18 <- function() {
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18") %>%
    sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
    sch_add_activity(  2L, "a2" , 4L, 5) %>%
    sch_add_activity(  3L, "a3" , 9L, 10) %>%
    sch_add_activity(  4L, "a4" , 1L, 6) %>%
    sch_add_activity(  5L, "a5" , 4L, 9) %>%
    sch_add_activity(  6L, "a6" , 5L, 7) %>%
    sch_add_activity(  7L, "a7" , 1L, 8, 11) %>%
    sch_add_activity(  8L, "a8" , 7L, 12) %>%
    sch_add_activity(  9L, "a9" , 8L, 12) %>%
    sch_add_activity( 10L, "a10", 3L, 12) %>%
    sch_add_activity( 11L, "a11", 3L, 12) %>%
    sch_add_activity( 12L, "a12", 0L) %>%
    sch_plan()
  return(sch)
}

test_that("Creating a sch step-by-step, with activities and relations together", {
  sch <- vanhoucke_2009_p18()
  atb <- sch_activities(sch)

  expect_equal(sch_duration(sch), 16)

  expect_equal(atb$early_start[1], 0)
  expect_equal(atb$early_finish[1], 0)
  expect_equal(atb$late_start[1], 0)
  expect_equal(atb$late_finish[1], 0)
  expect_true(atb$critical[1])

  expect_equal(atb$early_start[2], 0)
  expect_equal(atb$early_finish[2], 4)
  expect_equal(atb$late_start[2], 0)
  expect_equal(atb$late_finish[2], 4)
  expect_true(atb$critical[2])

  expect_equal(atb$early_start[3], 0)
  expect_equal(atb$early_finish[3], 9)
  expect_equal(atb$late_start[3], 4)
  expect_equal(atb$late_finish[3], 13)
  expect_false(atb$critical[3])

  expect_equal(atb$early_start[4], 0)
  expect_equal(atb$early_finish[4], 1)
  expect_equal(atb$late_start[4], 2)
  expect_equal(atb$late_finish[4], 3)
  expect_false(atb$critical[4])

  expect_equal(atb$early_start[5], 4)
  expect_equal(atb$early_finish[5], 8)
  expect_equal(atb$late_start[5], 4)
  expect_equal(atb$late_finish[5], 8)
  expect_true(atb$critical[5])

  expect_equal(atb$early_start[6], 1)
  expect_equal(atb$early_finish[6], 6)
  expect_equal(atb$late_start[6], 3)
  expect_equal(atb$late_finish[6], 8)
  expect_false(atb$critical[6])

  expect_equal(atb$early_start[7], 6)
  expect_equal(atb$early_finish[7], 7)
  expect_equal(atb$late_start[7], 8)
  expect_equal(atb$late_finish[7], 9)
  expect_false(atb$critical[7])

  expect_equal(atb$early_start[8], 7)
  expect_equal(atb$early_finish[8], 14)
  expect_equal(atb$late_start[8], 9)
  expect_equal(atb$late_finish[8], 16)
  expect_false(atb$critical[8])

  expect_equal(atb$early_start[9], 8)
  expect_equal(atb$early_finish[9], 16)
  expect_equal(atb$late_start[9], 8)
  expect_equal(atb$late_finish[9], 16)
  expect_true(atb$critical[9])

  expect_equal(atb$early_start[10], 9)
  expect_equal(atb$early_finish[10], 12)
  expect_equal(atb$late_start[10], 13)
  expect_equal(atb$late_finish[10], 16)
  expect_false(atb$critical[10])

  expect_equal(atb$early_start[11], 7)
  expect_equal(atb$early_finish[11], 10)
  expect_equal(atb$late_start[11], 13)
  expect_equal(atb$late_finish[11], 16)
  expect_false(atb$critical[11])

  expect_equal(atb$early_start[12], 16)
  expect_equal(atb$early_finish[12], 16)
  expect_equal(atb$late_start[12], 16)
  expect_equal(atb$late_finish[12], 16)
  expect_true(atb$critical[12])


})


test_that("Creating a sch step-by-step, with activities and relations together: float tests", {
  sch <- vanhoucke_2009_p18()
  atb <- sch_activities(sch)

  expect_equal(atb$total_float[1], 0)
  expect_equal(atb$total_float[2], 0)
  expect_equal(atb$total_float[3], 4)
  expect_equal(atb$total_float[4], 2)
  expect_equal(atb$total_float[5], 0)
  expect_equal(atb$total_float[6], 2)
  expect_equal(atb$total_float[7], 2)
  expect_equal(atb$total_float[8], 2)
  expect_equal(atb$total_float[9], 0)
  expect_equal(atb$total_float[10], 4)
  expect_equal(atb$total_float[11], 6)
  expect_equal(atb$total_float[12], 0)

})

test_that("Does not return Inf!!!", {
  # Create a sch
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18") %>%
    sch_add_activity(  2L, "a2" , 4L, 5) %>%
    sch_add_activity(  3L, "a3" , 9L, 10) %>%
    sch_add_activity(  4L, "a4" , 1L, 6) %>%
    sch_add_activity(  5L, "a5" , 4L, 9) %>%
    sch_add_activity(  6L, "a6" , 5L, 7) %>%
    sch_add_activity(  7L, "a7" , 1L, 8,11) %>%
    sch_add_activity(  8L, "a8" , 7L, 12) %>%
    sch_add_activity(  9L, "a9" , 8L, 12) %>%
    sch_add_activity( 10L, "a10", 3L, 12) %>%
    sch_add_activity( 11L, "a11", 3L, 12) %>%
    sch_add_activity( 12L, "a12", 0L) %>%
    sch_plan()

  expect_true(all(!is.infinite(sch$activities$late_start)))
  expect_true(all(!is.infinite(sch$activities$late_finish)))

  expect_true(all(!is.na(sch$activities$late_start)))
  expect_true(all(!is.na(sch$activities$late_finish)))

  expect_equal(sch_duration(sch), 16)

  id <- 2
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 0)
  expect_equal(atv$early_finish, 4)
  expect_equal(atv$late_start, 0)
  expect_equal(atv$late_finish, 4)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 3
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 0)
  expect_equal(atv$early_finish, 9)
  expect_equal(atv$late_start, 4)
  expect_equal(atv$late_finish, 13)
  expect_equal(atv$total_float, 4)
  expect_equal(atv$free_float, 0)

  id <- 4
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 0)
  expect_equal(atv$early_finish, 1)
  expect_equal(atv$late_start, 2)
  expect_equal(atv$late_finish, 3)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 5
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 4)
  expect_equal(atv$early_finish, 8)
  expect_equal(atv$late_start, 4)
  expect_equal(atv$late_finish, 8)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 6
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 1)
  expect_equal(atv$early_finish, 6)
  expect_equal(atv$late_start, 3)
  expect_equal(atv$late_finish, 8)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 7
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 6)
  expect_equal(atv$early_finish, 7)
  expect_equal(atv$late_start, 8)
  expect_equal(atv$late_finish, 9)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 0)

  id <- 8
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 7)
  expect_equal(atv$early_finish, 14)
  expect_equal(atv$late_start, 9)
  expect_equal(atv$late_finish, 16)
  expect_equal(atv$total_float, 2)
  expect_equal(atv$free_float, 2)

  id <- 9
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 8)
  expect_equal(atv$early_finish, 16)
  expect_equal(atv$late_start, 8)
  expect_equal(atv$late_finish, 16)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)

  id <- 10
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 9)
  expect_equal(atv$early_finish, 12)
  expect_equal(atv$late_start, 13)
  expect_equal(atv$late_finish, 16)
  expect_equal(atv$total_float, 4)
  expect_equal(atv$free_float, 4)

  id <- 11
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 7)
  expect_equal(atv$early_finish, 10)
  expect_equal(atv$late_start, 13)
  expect_equal(atv$late_finish, 16)
  expect_equal(atv$total_float, 6)
  expect_equal(atv$free_float, 6)

  id <- 12
  atv <- sch_get_activity(sch, id)
  expect_equal(atv$early_start, 16)
  expect_equal(atv$early_finish, 16)
  expect_equal(atv$late_start, 16)
  expect_equal(atv$late_finish, 16)
  expect_equal(atv$total_float, 0)
  expect_equal(atv$free_float, 0)


})
