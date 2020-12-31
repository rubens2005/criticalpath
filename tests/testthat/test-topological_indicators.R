vanhoucke_2009_p61 <- function() {
  make_empty_schedule(
    "Fictitous Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 61"
  )
}

vanhoucke_2009_p18 <- function() {
  schedule <- make_empty_schedule(
    "Fictitous Project Example",
    "VANHOUCKE, Mario. Measuring time: improving project performance using earned value management. Gent: Springer, 2009, p. 18"
  )

  schedule <- add_act_rel(schedule,  1, "a2" , 4, c(4))
  schedule <- add_act_rel(schedule,  2, "a3" , 9, c(9))
  schedule <- add_act_rel(schedule,  3, "a4" , 1, c(5))
  schedule <- add_act_rel(schedule,  4, "a5" , 4, c(8))
  schedule <- add_act_rel(schedule,  5, "a6" , 5, c(6))
  schedule <- add_act_rel(schedule,  6, "a7" , 1, c(7, 10))
  schedule <- add_act_rel(schedule,  7, "a8" , 7)
  schedule <- add_act_rel(schedule,  8, "a9" , 8)
  schedule <- add_act_rel(schedule,  9, "a10", 3)
  schedule <- add_act_rel(schedule, 10, "a11", 3)
  schedule
}

test_that("Topological Indicator with schedule with ZERO activities", {
  schedule <- vanhoucke_2009_p61()
  expect_equal(schedule$info$duration, 0)

  expect_true(is.na(topoi_sp(schedule)))
  expect_true(is.na(topoi_ad(schedule)))
  expect_true(is.na(topoi_la(schedule)))
  expect_true(is.na(topoi_tf(schedule)))

})

test_that("Topological Indicator with schedule with ONE activity", {
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1)
  expect_equal(schedule$info$duration, 1)

  expect_false(is.na(topoi_sp(schedule)))
  expect_false(is.na(topoi_ad(schedule)))
  expect_false(is.na(topoi_la(schedule)))
  expect_false(is.na(topoi_tf(schedule)))

  expect_equal(topoi_sp(schedule), 1)
  expect_equal(topoi_ad(schedule), 0)
  expect_equal(topoi_la(schedule), 1)
  expect_equal(topoi_tf(schedule), 0)

})

test_that("Topological Indicator with schedule with TWO PARALLEL activities",
          {
            schedule <- vanhoucke_2009_p61()
            schedule <- add_act_rel(schedule,  1, "a1" , 1)
            schedule <- add_act_rel(schedule,  2, "a2" , 1)
            expect_equal(schedule$info$duration, 1)

            expect_equal(topoi_sp(schedule), 0)
            expect_equal(topoi_ad(schedule), 0)
            expect_equal(topoi_la(schedule), 1)
            expect_equal(topoi_tf(schedule), 0)

          })

test_that("Topological Indicator with schedule with TWO SERIAL activities",
          {
            schedule <- vanhoucke_2009_p61()
            schedule <- add_act_rel(schedule,  1, "a1" , 1, c(2))
            schedule <- add_act_rel(schedule,  2, "a2" , 1)
            expect_equal(schedule$info$duration, 2)

            expect_equal(topoi_sp(schedule), 1)
            expect_equal(topoi_ad(schedule), 0)
            expect_equal(topoi_la(schedule), 1)
            expect_equal(topoi_tf(schedule), 0)

          })

test_that("Topological Indicator with schedule with THREE PARALLEL activities",
          {
            schedule <- vanhoucke_2009_p61()
            schedule <- add_act_rel(schedule,  1, "a1" , 1)
            schedule <- add_act_rel(schedule,  2, "a2" , 1)
            schedule <- add_act_rel(schedule,  3, "a3" , 1)
            expect_equal(schedule$info$duration, 1)

            expect_equal(topoi_sp(schedule), 0)
            expect_equal(topoi_ad(schedule), 0)
            expect_equal(topoi_la(schedule), 1)
            expect_equal(topoi_tf(schedule), 0)

          })

test_that("Topological Indicator with schedule with THREE SERIAL activities",
          {
            schedule <- vanhoucke_2009_p61()
            schedule <- add_act_rel(schedule,  1, "a1" , 1, c(2))
            schedule <- add_act_rel(schedule,  2, "a2" , 1, c(3))
            schedule <- add_act_rel(schedule,  3, "a3" , 1)
            expect_equal(schedule$info$duration, 3)

            expect_equal(topoi_sp(schedule), 1)
            expect_equal(topoi_ad(schedule), 0)
            expect_equal(topoi_la(schedule), 1)
            expect_equal(topoi_tf(schedule), 0)

          })

test_that("Topological indicator are correct.", {
  schedule <- vanhoucke_2009_p18()
  expect_equal(schedule$info$duration, 16)

  expect_equal(round(topoi_sp(schedule) * 100), 33)
  expect_equal(round(topoi_ad(schedule) * 100), 22)
  expect_equal(round(topoi_la(schedule) * 100), 0)
  expect_equal(round(topoi_tf(schedule) * 100), 39)

})


test_that("(a) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # a)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(4))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(5))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(6))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(7))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(8))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(9))
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 0)
  expect_equal(round(topoi_la(schedule) * 100), 0)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})

test_that("(b) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # b)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(4, 5))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(5, 6))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(5, 6))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(7, 8))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(7, 8, 9))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(9))
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 0)
  expect_equal(round(topoi_la(schedule) * 100), 50)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})

test_that("(c) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # c)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(4, 5, 6))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(4, 5, 6))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(4, 5, 6))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(7, 8, 9))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(7, 8, 9))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(7, 8, 9))
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 0)
  expect_equal(round(topoi_la(schedule) * 100), 100)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})

test_that("(d) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # d)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(5))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(6))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(7))
  schedule <- add_act_rel(schedule,  4, "a4", 1)
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(8))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(9))
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 25)
  expect_equal(round(topoi_la(schedule) * 100), 0)
  expect_equal(round(topoi_tf(schedule) * 100), 33)
  # In the book, the value is 25, but it's wrong.
  # The correct value é 33.

})

test_that("(e) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # e)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(4, 5, 6))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(4, 5, 6))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(5, 6, 7))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(8))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(8))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(9))
  schedule <- add_act_rel(schedule,  7, "a7", 1, c(9))
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 25)
  expect_equal(round(topoi_la(schedule) * 100), 50)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})

test_that("(f) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # f)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(5, 6, 7))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(5, 6, 7))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(5, 6, 7))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(5, 6, 7))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(8, 9))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(8, 9))
  schedule <- add_act_rel(schedule,  7, "a7", 1, c(8, 9))
  schedule <- add_act_rel(schedule,  8, "a8", 1)
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 25)
  expect_equal(round(topoi_la(schedule) * 100), 100)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})

test_that("(g) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # g)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1)
  schedule <- add_act_rel(schedule,  2, "a2", 1)
  schedule <- add_act_rel(schedule,  3, "a3", 1)
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(8))
  schedule <- add_act_rel(schedule,  5, "a5", 1)
  schedule <- add_act_rel(schedule,  6, "a6", 1)
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1, c(9))
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 100)
  expect_equal(round(topoi_la(schedule) * 100), 0)
  expect_equal(round(topoi_tf(schedule) * 100), 100)

})

test_that("(h) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # h)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(8))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(8))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(8))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(8))
  schedule <- add_act_rel(schedule,  5, "a5", 1)
  schedule <- add_act_rel(schedule,  6, "a6", 1)
  schedule <- add_act_rel(schedule,  7, "a7", 1)
  schedule <- add_act_rel(schedule,  8, "a8", 1, c(9))
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 100)
  expect_equal(round(topoi_la(schedule) * 100), 50)
  expect_equal(round(topoi_tf(schedule) * 100), 50)

})

test_that("(h - Fig. 4.4) Example wetworks with and SP value of 0.25", {
  # h - Fig. 4.4)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(8))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(8))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(8))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(8))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(9))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(9))
  schedule <- add_act_rel(schedule,  7, "a7", 1, c(9))
  schedule <- add_act_rel(schedule,  8, "a8", 1, c(9))
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 100)
  expect_equal(round(topoi_la(schedule) * 100), 50)
  expect_equal(round(topoi_tf(schedule) * 100), 25)

})

test_that("(i) Fig. 4.3 - 9 example wetworks with and SP value of 0.25", {
  # i)
  schedule <- vanhoucke_2009_p61()
  schedule <- add_act_rel(schedule,  1, "a1", 1, c(8))
  schedule <- add_act_rel(schedule,  2, "a2", 1, c(8))
  schedule <- add_act_rel(schedule,  3, "a3", 1, c(8))
  schedule <- add_act_rel(schedule,  4, "a4", 1, c(8))
  schedule <- add_act_rel(schedule,  5, "a5", 1, c(8))
  schedule <- add_act_rel(schedule,  6, "a6", 1, c(8))
  schedule <- add_act_rel(schedule,  7, "a7", 1, c(8))
  schedule <- add_act_rel(schedule,  8, "a8", 1, c(9))
  schedule <- add_act_rel(schedule,  9, "a9", 1)
  expect_equal(schedule$info$duration, 3)

  expect_equal(round(topoi_sp(schedule) * 100), 25)
  expect_equal(round(topoi_ad(schedule) * 100), 100)
  expect_equal(round(topoi_la(schedule) * 100), 100)
  expect_equal(round(topoi_tf(schedule) * 100), 0)

})
