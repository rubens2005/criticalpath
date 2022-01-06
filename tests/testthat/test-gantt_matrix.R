################## Setup

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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
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

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"
  schedule
}

################## Tests

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
  expect_true(identical(as.numeric(gantt[1,]), mtx[1,]))
  expect_true(identical(as.numeric(gantt[2,]), mtx[2,]))
  expect_true(identical(as.numeric(gantt[3,]), mtx[3,]))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, C", {
  schedule <- s1()
  expect_equal(schedule$duration, 5)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1)))
  expect_true(identical(as.numeric(gantt[3,]), c(1,1,1,1,0)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B", {
  schedule <- s2()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C", {
  schedule <- s3()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("B->C, A", {
  schedule <- s4()
  expect_equal(schedule$duration, 6)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, B->C", {
  schedule <- s5()
  expect_equal(schedule$duration, 9)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B->C", {
  schedule <- s6()
  expect_equal(schedule$duration, 7)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C, B->C", {
  schedule <- s7()
  expect_equal(schedule$duration, 9)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,0,0,1,1,1,1)))

  xygantt <- schedule$xy_gantt_matrix(gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("First and last activity's durations equal zero ", {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <- "VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18"

  # Add activities and relations to it.
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

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[ 1,]), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 2,]), c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 3,]), c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 4,]), c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 5,]), c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 6,]), c(0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 7,]), c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 8,]), c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[ 9,]), c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[10,]), c(0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[11,]), c(0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[12,]), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
})

test_that("First and last activity's durations equal zero, and durations chagend.", {
  schedule <- Schedule$new()
  schedule$title <- "Fictitious Project Example"
  schedule$reference <- "VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18"

  # Add activities and relations to it.
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

  new_durations <- c(0, 6, 11, 1, 4, 3, 2, 8, 8, 5, 4, 0)
  schedule$change_durations(new_durations)

  gantt <- schedule$gantt_matrix()
  expect_true(identical(as.numeric(gantt[ 1,]), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 2,]), c(1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 3,]), c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 4,]), c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 5,]), c(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 6,]), c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 7,]), c(0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 8,]), c(0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[ 9,]), c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[10,]), c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[11,]), c(0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[12,]), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
})




test_that("Período com número negativo deve funcionar.", {
  activities <- data.frame(
    id       = c(  1L,  2L,  3L),
    name     = c( "A", "B", "C"),
    duration = c(  5L,  8L, 10L)
  )

  relations <- data.frame(
    from = c(  1L,   2L),
    to   = c(  2L,   3L),
    type = c("FF", "FF"),
    lag  = c(  0L,   0L)
  )

  schedule <- Schedule$new(activities, relations)
  schedule$title <- "A project"
  schedule$reference <- "From criticalpath"

  gantt <- schedule$gantt_matrix()

  expect_true(identical(as.numeric(colnames(gantt)), c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5)))

  expect_true(identical(as.numeric(gantt[ 1,]), c(0,0,0,0,0,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[ 2,]), c(0,0,1,1,1,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[ 3,]), c(1,1,1,1,1,1,1,1,1,1)))
})
