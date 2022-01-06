################## Setup

s0 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_plan()
  )
}

s1 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L),
        to   = c(2L)
      ) %>%
      sch_plan()
  )
}

s2 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L),
        to   = c(3L)
      ) %>%
      sch_plan()
  )
}

s3 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L, 1L),
        to   = c(2L, 3L)
      ) %>%
      sch_plan()
  )
}

s4 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(2L),
        to   = c(3L)
      ) %>%
      sch_plan()
  )


}

s5 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L, 2L),
        to   = c(2L, 3L)
      ) %>%
      sch_plan()
  )
}

s6 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L, 2L),
        to   = c(3L, 3L)
      ) %>%
      sch_plan()
  )
}

s7 <- function() {
  return(
    sch_new() %>%
      sch_title("A project") %>%
      sch_reference("From criticalpath") %>%
      sch_add_activities(
        id        = c( 1L,  2L,  3L),
        name      = c("A", "B", "C"),
        duration  = c( 3L,  2L,  4L)
      ) %>%
      sch_add_relations(
        from = c(1L, 1L, 2L),
        to   = c(2L, 3L, 3L)
      ) %>%
      sch_plan()
  )
}

################## Tests

test_that("There is no Gantt matrix for a sch with zero duration!", {
  sch <- sch_new() %>%
    sch_add_activity(1L, "", 0L) %>%
    sch_add_activity(2L, "", 0L) %>%
    sch_add_activity(3L, "", 0L) %>%
    sch_plan()
  expect_error(sch_gantt_matrix(sch))
})

test_that("A,B,C", {
  sch <- s0()
  expect_equal(sch_duration(sch), 4)

  mtx <- base::matrix(
    c(
      1,1,1,0,
      1,1,0,0,
      1,1,1,1
    ),
    byrow = TRUE,
    ncol = 4
  )

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), mtx[1,]))
  expect_true(identical(as.numeric(gantt[2,]), mtx[2,]))
  expect_true(identical(as.numeric(gantt[3,]), mtx[3,]))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, C", {
  sch <- s1()
  expect_equal(sch_duration(sch), 5)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1)))
  expect_true(identical(as.numeric(gantt[3,]), c(1,1,1,1,0)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B", {
  sch <- s2()
  expect_equal(sch_duration(sch), 7)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C", {
  sch <- s3()
  expect_equal(sch_duration(sch), 7)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("B->C, A", {
  sch <- s4()
  expect_equal(sch_duration(sch), 6)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, B->C", {
  sch <- s5()
  expect_equal(sch_duration(sch), 9)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->C, B->C", {
  sch <- s6()
  expect_equal(sch_duration(sch), 7)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(1,1,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("A->B, A->C, B->C", {
  sch <- s7()
  expect_equal(sch_duration(sch), 9)

  gantt <- sch_gantt_matrix(sch)
  expect_true(identical(as.numeric(gantt[1,]), c(1,1,1,0,0,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[2,]), c(0,0,0,1,1,0,0,0,0)))
  expect_true(identical(as.numeric(gantt[3,]), c(0,0,0,0,0,1,1,1,1)))

  xygantt <- sch_xy_gantt_matrix(sch, gantt)
  expect_equal(nrow(xygantt), sum(gantt))
})

test_that("First and last activity's durations equal zero ", {
  sch <- sch_new() %>%
    sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18") %>%
    # Add activities and relations to it.
    sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
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

  gantt <- sch_gantt_matrix(sch)
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
  sch <- sch_new() %>%
  sch_title("Fictitious Project Example") %>%
  sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 18") %>%
  # Add activities and relations to it.
  sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
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

  new_durations <- as.integer(c(0, 6, 11, 1, 4, 3, 2, 8, 8, 5, 4, 0))
  sch %<>% sch_change_activities_duration(new_durations)

  gantt <- sch_gantt_matrix(sch)
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
  sch <- sch_new() %>%
    sch_title("A project") %>%
    sch_reference("From criticalpath") %>%
    sch_add_activities(
      id       = c(  1L,  2L,  3L),
      name     = c( "A", "B", "C"),
      duration = c(  5L,  8L, 10L)
    ) %>%
    sch_add_relations(
      from = c(  1L,   2L),
      to   = c(  2L,   3L),
      type = c("FF", "FF"),
      lag  = c(  0L,   0L)
    ) %>%
    sch_plan()

  gantt <- sch_gantt_matrix(sch)

  expect_true(identical(as.numeric(colnames(gantt)), c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5)))

  expect_true(identical(as.numeric(gantt[ 1,]), c(0,0,0,0,0,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[ 2,]), c(0,0,1,1,1,1,1,1,1,1)))
  expect_true(identical(as.numeric(gantt[ 3,]), c(1,1,1,1,1,1,1,1,1,1)))
})

