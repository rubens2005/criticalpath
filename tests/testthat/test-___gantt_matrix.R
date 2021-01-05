s0 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(),
    to   = c()
  )

  schedule_from_data_frame_R6(activities, relations)
}

s1 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1),
    to   = c(2)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s2 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1),
    to   = c(3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s3 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1, 1),
    to   = c(2, 3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s4 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(2),
    to   = c(3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s5 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1, 2),
    to   = c(2, 3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s6 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1, 2),
    to   = c(3, 3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

s7 <- function() {
  activities <- data.frame(
    id        = c(  1,   2,   3),
    name      = c("A", "B", "C"),
    duration  = c(  3,   2,   4)
  )

  relations <- data.frame(
    from = c(1, 1, 2),
    to   = c(2, 3, 3)
  )

  schedule_from_data_frame_R6(activities, relations)
}

test_that("There is no Gantt matrix for a schedule with zero duration!", {
    schedule <- Schedule$new()
    schedule$add_activity(1)
    schedule$add_activity(2)
    schedule$add_activity(3)
    expect_error(schedule$gantt_matrix())
})

test_that("A,B,C", {
  schedule <- s0()
  expect_equal(schedule$duration, 4)

  mtx <- base::matrix(
    c(
      1,1,1,0,
      1,1,0,0,
      1,1,1,1
    ),
    byrow = TRUE,
    ncol = 4
  )

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], mtx[1,]))
  expect_true(identical(gantt[2,], mtx[2,]))
  expect_true(identical(gantt[3,], mtx[3,]))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, C", {
  schedule <- s1()
  expect_equal(schedule$duration, 5)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0)))
  expect_true(identical(gantt[2,], c(0,0,0,1,1)))
  expect_true(identical(gantt[3,], c(1,1,1,1,0)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B", {
  schedule <- s2()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0,0)))
  expect_true(identical(gantt[2,], c(1,1,0,0,0,0,0)))
  expect_true(identical(gantt[3,], c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C", {
  schedule <- s3()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0,0)))
  expect_true(identical(gantt[2,], c(0,0,0,1,1,0,0)))
  expect_true(identical(gantt[3,], c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("B->C, A", {
  schedule <- s4()
  expect_equal(schedule$duration, 6)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0)))
  expect_true(identical(gantt[2,], c(1,1,0,0,0,0)))
  expect_true(identical(gantt[3,], c(0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, B->C", {
  schedule <- s5()
  expect_equal(schedule$duration, 9)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(gantt[2,], c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(gantt[3,], c(0,0,0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B->C", {
  schedule <- s6()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0,0)))
  expect_true(identical(gantt[2,], c(1,1,0,0,0,0,0)))
  expect_true(identical(gantt[3,], c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C, B->C", {
  schedule <- s7()
  expect_equal(schedule$duration, 9)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(gantt[1,], c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(gantt[2,], c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(gantt[3,], c(0,0,0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})
