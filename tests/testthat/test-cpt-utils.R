###
# cpt_convert_to_integer
###

test_that("Vector cannot be NULL or empty.", {
  v <- NULL
  expect_error({
    new_v <- cpt_convert_to_integer(v)
  })

  v <- integer()
  expect_error({
    new_v <- cpt_convert_to_integer(v)
  })

  v <- numeric()
  expect_error({
    new_v <- cpt_convert_to_integer(v)
  })
})

test_that("Convert a number to integer", {
  v <- 1
  expect_false(inherits(v, "integer"))
  expect_true(inherits(v, "numeric"))

  new_v <- cpt_convert_to_integer(v)
  expect_true(inherits(new_v, "integer"))
  expect_false(inherits(new_v, "numeric"))
  expect_equal(new_v, 1L)

  v <- 1.2
  expect_false(inherits(v, "integer"))
  expect_true(inherits(v, "numeric"))

  new_v <- cpt_convert_to_integer(v)
  expect_true(inherits(new_v, "integer"))
  expect_false(inherits(new_v, "numeric"))
  expect_equal(new_v, 1L)
})

test_that("Convert a NULL or NA to integer.", {
  v <- c(NA, 0.3)
  expect_false(inherits(v, "integer"))
  expect_true(inherits(v, "numeric"))

  new_v <- cpt_convert_to_integer(v)
  expect_true(inherits(new_v, "integer"))
  expect_false(inherits(new_v, "numeric"))
  expect_equal(new_v, c(0, 0))
})

test_that("Not Convert a NULL or NA to integer.", {
  v <- c(NA, 0.3)
  expect_false(inherits(v, "integer"))
  expect_true(inherits(v, "numeric"))

  new_v <- cpt_convert_to_integer(v, FALSE)
  expect_true(inherits(new_v, "integer"))
  expect_false(inherits(new_v, "numeric"))
  expect_equal(new_v, c(NA, 0))
})

test_that("Convert a vector to integer.", {
  v <- c(1, 1.2, 2.5, 5, 0.3)
  expect_false(inherits(v, "integer"))
  expect_true(inherits(v, "numeric"))

  new_v <- cpt_convert_to_integer(v)
  expect_true(inherits(new_v, "integer"))
  expect_false(inherits(new_v, "numeric"))
  expect_equal(new_v, c(1L, 1L, 2L, 5L, 0L))
})

###
# cpt_assert_activity_id_is_valid
###

test_that("Null activity id is detected", {
  activity_id <- NULL
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(NULL, NULL, NULL)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })
})

test_that("NA activity id is detected", {
  activity_id <- NA
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(NA, NA, NA)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(NA, 2L, 3L)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(1L, NA, 3L)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(1L, 2L, NA)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })
})

test_that("Non activity integer id is detected", {
  activity_id <- 1
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(1L, 2L, 3)
  expect_error({
    cpt_assert_activity_id_is_valid(activity_id)
  })
})

test_that("Activity id is correct", {
  activity_id <- 1L
  expect_silent({
    cpt_assert_activity_id_is_valid(activity_id)
  })

  activity_id <- c(1L, 2L, 3L)
  expect_silent({
    cpt_assert_activity_id_is_valid(activity_id)
  })
})

