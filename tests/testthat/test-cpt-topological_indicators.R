vanhoucke_2009_p61 <- function() {
  return(
    sch_new() %>%
      sch_title("Fictitious Project Example") %>%
      sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 61")
  )
}

vanhoucke_2009_p18 <- function() {
  return(
    sch_new() %>%
      sch_title("Fictitious Project Example") %>%
    sch_reference("VANHOUCKE, Mario. Measuring time:
  improving project performance using earned value management.
  Gent: Springer, 2009, p. 61") %>%
    sch_add_activity( 1L, "a2" , 4L, 4) %>%
    sch_add_activity( 2L, "a3" , 9L, 9) %>%
    sch_add_activity( 3L, "a4" , 1L, 5) %>%
    sch_add_activity( 4L, "a5" , 4L, 8) %>%
    sch_add_activity( 5L, "a6" , 5L, 6) %>%
    sch_add_activity( 6L, "a7" , 1L, 7, 10) %>%
    sch_add_activity( 7L, "a8" , 7L) %>%
    sch_add_activity( 8L, "a9" , 8L) %>%
    sch_add_activity( 9L, "a10", 3L) %>%
    sch_add_activity(10L, "a11", 3L)
  )
}

test_that("Topological Indicator with sch with ZERO activities", {
  sch <- vanhoucke_2009_p61()

  expect_error(sch_duration(sch))

  expect_true(is.na(sch_topoi_sp(sch)))
  expect_true(is.na(sch_topoi_ad(sch)))
  expect_true(is.na(sch_topoi_la(sch)))
  expect_true(is.na(sch_topoi_tf(sch)))
})

test_that("Topological Indicator with sch with ONE activity", {
  sch <- vanhoucke_2009_p61() %>%
    sch_add_activity( 1L, "a1", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 1)

  expect_false(is.na(sch_topoi_sp(sch)))
  expect_false(is.na(sch_topoi_ad(sch)))
  expect_false(is.na(sch_topoi_la(sch)))
  expect_false(is.na(sch_topoi_tf(sch)))

  expect_equal(sch_topoi_sp(sch), 1)
  expect_equal(sch_topoi_ad(sch), 0)
  expect_equal(sch_topoi_la(sch), 1)
  expect_equal(sch_topoi_tf(sch), 0)
})

test_that("Topological Indicator with sch with TWO PARALLEL activities", {
  sch <- vanhoucke_2009_p61() %>%
    sch_add_activity( 1L, "a1" , 1L) %>%
    sch_add_activity( 2L, "a2" , 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 1)

  expect_equal(sch_topoi_sp(sch), 0)
  expect_equal(sch_topoi_ad(sch), 0)
  expect_equal(sch_topoi_la(sch), 1)
  expect_equal(sch_topoi_tf(sch), 0)
})

test_that("Topological Indicator with sch with TWO SERIAL activities", {
  sch <- vanhoucke_2009_p61() %>%
    sch_add_activity( 1L, "a1" , 1L, 2) %>%
    sch_add_activity( 2L, "a2" , 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 2)

  expect_equal(sch_topoi_sp(sch), 1)
  expect_equal(sch_topoi_ad(sch), 0)
  expect_equal(sch_topoi_la(sch), 1)
  expect_equal(sch_topoi_tf(sch), 0)
})

test_that("Topological Indicator with sch with THREE PARALLEL activities", {
  sch <- vanhoucke_2009_p61() %>%
    sch_add_activity( 1L, "a1" , 1L) %>%
    sch_add_activity( 2L, "a2" , 1L) %>%
    sch_add_activity( 3L, "a3" , 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 1)

  expect_equal(sch_topoi_sp(sch), 0)
  expect_equal(sch_topoi_ad(sch), 0)
  expect_equal(sch_topoi_la(sch), 1)
  expect_equal(sch_topoi_tf(sch), 0)
})

test_that("Topological Indicator with sch with THREE SERIAL activities", {
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1" , 1L, 2) %>%
  sch_add_activity( 2L, "a2" , 1L, 3) %>%
  sch_add_activity( 3L, "a3" , 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(sch_topoi_sp(sch), 1)
  expect_equal(sch_topoi_ad(sch), 0)
  expect_equal(sch_topoi_la(sch), 1)
  expect_equal(sch_topoi_tf(sch), 0)
})

test_that("Topological indicator are correct.", {
  sch <- vanhoucke_2009_p18() %>%
    sch_plan()

  expect_equal(sch_duration(sch), 16)

  expect_equal(round(sch_topoi_sp(sch) * 100), 33)
  expect_equal(round(sch_topoi_ad(sch) * 100), 22)
  expect_equal(round(sch_topoi_la(sch) * 100), 0)
  expect_equal(round(sch_topoi_tf(sch) * 100), 39)
})


test_that("(a) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # a)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 4) %>%
  sch_add_activity( 2L, "a2", 1L, 5) %>%
  sch_add_activity( 3L, "a3", 1L, 6) %>%
  sch_add_activity( 4L, "a4", 1L, 7) %>%
  sch_add_activity( 5L, "a5", 1L, 8) %>%
  sch_add_activity( 6L, "a6", 1L, 9) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 0)
  expect_equal(round(sch_topoi_la(sch) * 100), 0)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

test_that("(b) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # b)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 4, 5) %>%
  sch_add_activity( 2L, "a2", 1L, 5, 6) %>%
  sch_add_activity( 3L, "a3", 1L, 5, 6) %>%
  sch_add_activity( 4L, "a4", 1L, 7, 8) %>%
  sch_add_activity( 5L, "a5", 1L, 7, 8, 9) %>%
  sch_add_activity( 6L, "a6", 1L, 9) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 0)
  expect_equal(round(sch_topoi_la(sch) * 100), 50)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

test_that("(c) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # c)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 4, 5, 6) %>%
  sch_add_activity( 2L, "a2", 1L, 4, 5, 6) %>%
  sch_add_activity( 3L, "a3", 1L, 4, 5, 6) %>%
  sch_add_activity( 4L, "a4", 1L, 7, 8, 9) %>%
  sch_add_activity( 5L, "a5", 1L, 7, 8, 9) %>%
  sch_add_activity( 6L, "a6", 1L, 7, 8, 9) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 0)
  expect_equal(round(sch_topoi_la(sch) * 100), 100)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

test_that("(d) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # d)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 5) %>%
  sch_add_activity( 2L, "a2", 1L, 6) %>%
  sch_add_activity( 3L, "a3", 1L, 7) %>%
  sch_add_activity( 4L, "a4", 1L) %>%
  sch_add_activity( 5L, "a5", 1L, 8) %>%
  sch_add_activity( 6L, "a6", 1L, 9) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 25)
  expect_equal(round(sch_topoi_la(sch) * 100), 0)
  expect_equal(round(sch_topoi_tf(sch) * 100), 33)
  # In the book, the value is 25, but it's wrong.
  # The correct value é 33.
})

test_that("(e) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # e)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 4, 5, 6) %>%
  sch_add_activity( 2L, "a2", 1L, 4, 5, 6) %>%
  sch_add_activity( 3L, "a3", 1L, 5, 6, 7) %>%
  sch_add_activity( 4L, "a4", 1L, 8) %>%
  sch_add_activity( 5L, "a5", 1L, 8) %>%
  sch_add_activity( 6L, "a6", 1L, 9) %>%
  sch_add_activity( 7L, "a7", 1L, 9) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 25)
  expect_equal(round(sch_topoi_la(sch) * 100), 50)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

test_that("(f) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # f)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 5, 6, 7) %>%
  sch_add_activity( 2L, "a2", 1L, 5, 6, 7) %>%
  sch_add_activity( 3L, "a3", 1L, 5, 6, 7) %>%
  sch_add_activity( 4L, "a4", 1L, 5, 6, 7) %>%
  sch_add_activity( 5L, "a5", 1L, 8, 9) %>%
  sch_add_activity( 6L, "a6", 1L, 8, 9) %>%
  sch_add_activity( 7L, "a7", 1L, 8, 9) %>%
  sch_add_activity( 8L, "a8", 1L) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 25)
  expect_equal(round(sch_topoi_la(sch) * 100), 100)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

test_that("(g) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # g)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L) %>%
  sch_add_activity( 2L, "a2", 1L) %>%
  sch_add_activity( 3L, "a3", 1L) %>%
  sch_add_activity( 4L, "a4", 1L, 8) %>%
  sch_add_activity( 5L, "a5", 1L) %>%
  sch_add_activity( 6L, "a6", 1L) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L, 9) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 100)
  expect_equal(round(sch_topoi_la(sch) * 100), 0)
  expect_equal(round(sch_topoi_tf(sch) * 100), 100)
})

test_that("(h) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # h)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 8) %>%
  sch_add_activity( 2L, "a2", 1L, 8) %>%
  sch_add_activity( 3L, "a3", 1L, 8) %>%
  sch_add_activity( 4L, "a4", 1L, 8)%>%
  sch_add_activity( 5L, "a5", 1L) %>%
  sch_add_activity( 6L, "a6", 1L) %>%
  sch_add_activity( 7L, "a7", 1L) %>%
  sch_add_activity( 8L, "a8", 1L, 9) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 100)
  expect_equal(round(sch_topoi_la(sch) * 100), 50)
  expect_equal(round(sch_topoi_tf(sch) * 100), 50)
})

test_that("(h - Fig. 4.4) Example wetworks with and SP value of 0.25", {
  # h - Fig. 4.4)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, 8) %>%
  sch_add_activity( 2L, "a2", 1L, 8) %>%
  sch_add_activity( 3L, "a3", 1L, 8) %>%
  sch_add_activity( 4L, "a4", 1L, 8) %>%
  sch_add_activity( 5L, "a5", 1L, 9) %>%
  sch_add_activity( 6L, "a6", 1L, 9) %>%
  sch_add_activity( 7L, "a7", 1L, 9) %>%
  sch_add_activity( 8L, "a8", 1L, 9) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 100)
  expect_equal(round(sch_topoi_la(sch) * 100), 50)
  expect_equal(round(sch_topoi_tf(sch) * 100), 25)
})

test_that("(i) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # i)
  sch <- vanhoucke_2009_p61() %>%
  sch_add_activity( 1L, "a1", 1L, c(8)) %>%
  sch_add_activity( 2L, "a2", 1L, c(8)) %>%
  sch_add_activity( 3L, "a3", 1L, c(8)) %>%
  sch_add_activity( 4L, "a4", 1L, c(8)) %>%
  sch_add_activity( 5L, "a5", 1L, c(8)) %>%
  sch_add_activity( 6L, "a6", 1L, c(8)) %>%
  sch_add_activity( 7L, "a7", 1L, c(8)) %>%
  sch_add_activity( 8L, "a8", 1L, c(9)) %>%
  sch_add_activity( 9L, "a9", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 3)

  expect_equal(round(sch_topoi_sp(sch) * 100), 25)
  expect_equal(round(sch_topoi_ad(sch) * 100), 100)
  expect_equal(round(sch_topoi_la(sch) * 100), 100)
  expect_equal(round(sch_topoi_tf(sch) * 100), 0)
})

