##############################
# FINISH-START relation type #
##############################

test_that("FS type | A_duration == B_duration | lag == 0", {
  begin_id <- 31L
  a_id <- 32L
  b_id <- 33L
  end_id <- 34L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(4, b_act$early_start);
  expect_equal(7, b_act$early_finish);
  expect_equal(7, end_act$early_start);
  expect_equal(8, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(4, b_act$late_start);
  expect_equal(7, b_act$late_finish);
  expect_equal(7, end_act$late_start);
  expect_equal(8, end_act$late_finish);

})

test_that("FS type | A_duration == B_duration | lag > 0", {
  begin_id <- 36L
  a_id <- 37L
  b_id <- 38L
  end_id <- 39L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(10, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(6, b_act$early_start);
  expect_equal(9, b_act$early_finish);
  expect_equal(9, end_act$early_start);
  expect_equal(10, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(6, b_act$late_start);
  expect_equal(9, b_act$late_finish);
  expect_equal(9, end_act$late_start);
  expect_equal(10, end_act$late_finish);

})

test_that("FS type | A_duration == B_duration | lag < 0", {
  begin_id <- 41L
  a_id <- 42L
  b_id <- 43L
  end_id <- 44L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(2, b_act$early_start);
  expect_equal(5, b_act$early_finish);
  expect_equal(5, end_act$early_start);
  expect_equal(6, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

test_that("FS type | A_duration < B_duration | lag == 0", {
  begin_id <- 79L
  a_id <- 80L
  b_id <- 81L
  end_id <- 82L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(10, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(4, b_act$early_start);
  expect_equal(9, b_act$early_finish);
  expect_equal(9, end_act$early_start);
  expect_equal(10, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(4, b_act$late_start);
  expect_equal(9, b_act$late_finish);
  expect_equal(9, end_act$late_start);
  expect_equal(10, end_act$late_finish);

})

test_that("FS type | A_duration < B_duration | lag > 0", {
  begin_id <- 84L
  a_id <- 85L
  b_id <- 86L
  end_id <- 87L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(12, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(6, b_act$early_start);
  expect_equal(11, b_act$early_finish);
  expect_equal(11, end_act$early_start);
  expect_equal(12, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(6, b_act$late_start);
  expect_equal(11, b_act$late_finish);
  expect_equal(11, end_act$late_start);
  expect_equal(12, end_act$late_finish);

})

test_that("FS type | A_duration < B_duration | lag < 0", {
  begin_id <- 89L
  a_id <- 90L
  b_id <- 91L
  end_id <- 92L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(2, b_act$early_start);
  expect_equal(7, b_act$early_finish);
  expect_equal(7, end_act$early_start);
  expect_equal(8, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(7, b_act$late_finish);
  expect_equal(7, end_act$late_start);
  expect_equal(8, end_act$late_finish);

})

test_that("FS type | A_duration > B_duration | lag == 0", {
  begin_id <- 95L
  a_id <- 96L
  b_id <- 97L
  end_id <- 98L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(10, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(6, b_act$early_start);
  expect_equal(9, b_act$early_finish);
  expect_equal(9, end_act$early_start);
  expect_equal(10, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(6, b_act$late_start);
  expect_equal(9, b_act$late_finish);
  expect_equal(9, end_act$late_start);
  expect_equal(10, end_act$late_finish);

})

test_that("FS type | A_duration > B_duration | lag > 0", {
  begin_id <- 100L
  a_id <- 101L
  b_id <- 102L
  end_id <- 104L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(12, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(8, b_act$early_start);
  expect_equal(11, b_act$early_finish);
  expect_equal(11, end_act$early_start);
  expect_equal(12, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(8, b_act$late_start);
  expect_equal(11, b_act$late_finish);
  expect_equal(11, end_act$late_start);
  expect_equal(12, end_act$late_finish);

})

test_that("FS type | A_duration > B_duration | lag < 0", {
  begin_id <- 105L
  a_id <- 106L
  b_id <- 107L
  end_id <- 108L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(4, b_act$early_start);
  expect_equal(7, b_act$early_finish);
  expect_equal(7, end_act$early_start);
  expect_equal(8, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(4, b_act$late_start);
  expect_equal(7, b_act$late_finish);
  expect_equal(7, end_act$late_start);
  expect_equal(8, end_act$late_finish);

})

###############################
# FINISH-FINISH relation type #
###############################

test_that("FF type | A_duration == B_duration | lag == 0", {
  begin_id <- 112L
  a_id <- 113L
  b_id <- 114L
  end_id <- 115L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(5, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(1, b_act$late_start);
  expect_equal(4, b_act$late_finish);
  expect_equal(4, end_act$late_start);
  expect_equal(5, end_act$late_finish);

})

test_that("FF type | A_duration == B_duration | lag > 0", {
  begin_id <- 117L
  a_id <- 118L
  b_id <- 119L
  end_id <- 120L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(3, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(3, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("FF type | A_duration == B_duration | lag < 0", {
  begin_id <- 122L
  a_id <- 123L
  b_id <- 124L
  end_id <- 125L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(5, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-1, b_act$early_start);
  expect_equal(2, b_act$early_finish);
  expect_equal(2, end_act$early_start);
  expect_equal(3, end_act$early_finish);

  expect_equal(1, begin_act$late_start);
  expect_equal(2, begin_act$late_finish);
  expect_equal(2, a_act$late_start);
  expect_equal(5, a_act$late_finish);
  expect_equal(0, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("FF type | A_duration < B_duration | lag == 0", {
  begin_id <- 128L
  a_id <- 129L
  b_id <- 130L
  end_id <- 131L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(-1, b_act$late_start);
  expect_equal(4, b_act$late_finish);
  expect_equal(4, end_act$late_start);
  expect_equal(5, end_act$late_finish);

})

test_that("FF type | A_duration < B_duration | lag > 0", {
  begin_id <- 133L
  a_id <- 134L
  b_id <- 135L
  end_id <- 136L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(1, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("FF type | A_duration < B_duration | lag < 0", {
  begin_id <- 138L
  a_id <- 139L
  b_id <- 140L
  end_id <- 141L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-3, b_act$early_start);
  expect_equal(2, b_act$early_finish);
  expect_equal(2, end_act$early_start);
  expect_equal(3, end_act$early_finish);

  expect_equal(1, begin_act$late_start);
  expect_equal(2, begin_act$late_finish);
  expect_equal(2, a_act$late_start);
  expect_equal(5, a_act$late_finish);
  expect_equal(-2, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("FF type | A_duration > B_duration | lag == 0", {
  begin_id <- 144L
  a_id <- 145L
  b_id <- 146L
  end_id <- 147L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(3, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(3, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("FF type | A_duration > B_duration | lag > 0", {
  begin_id <- 149L
  a_id <- 150L
  b_id <- 151L
  end_id <- 152L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(9, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(5, b_act$early_start);
  expect_equal(8, b_act$early_finish);
  expect_equal(8, end_act$early_start);
  expect_equal(9, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(5, b_act$late_start);
  expect_equal(8, b_act$late_finish);
  expect_equal(8, end_act$late_start);
  expect_equal(9, end_act$late_finish);

})

test_that("FF type | A_duration > B_duration | lag < 0", {
  begin_id <- 154L
  a_id <- 155L
  b_id <- 156L
  end_id <- 157L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "FF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(1, begin_act$late_start);
  expect_equal(2, begin_act$late_finish);
  expect_equal(2, a_act$late_start);
  expect_equal(7, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

#############################
# START-START relation type #
#############################

test_that("SS type | A_duration == B_duration | lag == 0", {
  begin_id <- 161L
  a_id <- 162L
  b_id <- 163L
  end_id <- 164L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(5, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(1, b_act$late_start);
  expect_equal(4, b_act$late_finish);
  expect_equal(4, end_act$late_start);
  expect_equal(5, end_act$late_finish);

})

test_that("SS type | A_duration == B_duration | lag > 0", {
  begin_id <- 166L
  a_id <- 167L
  b_id <- 168L
  end_id <- 169L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(3, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(3, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("SS type | A_duration == B_duration | lag < 0", {
  begin_id <- 171L
  a_id <- 172L
  b_id <- 173L
  end_id <- 174L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(5, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-1, b_act$early_start);
  expect_equal(2, b_act$early_finish);
  expect_equal(2, end_act$early_start);
  expect_equal(3, end_act$early_finish);

  expect_equal(1, begin_act$late_start);
  expect_equal(2, begin_act$late_finish);
  expect_equal(2, a_act$late_start);
  expect_equal(5, a_act$late_finish);
  expect_equal(0, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SS type | A_duration < B_duration | lag == 0", {
  begin_id <- 177L
  a_id <- 178L
  b_id <- 179L
  end_id <- 180L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(1, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("SS type | A_duration < B_duration | lag > 0", {
  begin_id <- 182L
  a_id <- 183L
  b_id <- 184L
  end_id <- 185L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(9, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(3, b_act$early_start);
  expect_equal(8, b_act$early_finish);
  expect_equal(8, end_act$early_start);
  expect_equal(9, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(3, b_act$late_start);
  expect_equal(8, b_act$late_finish);
  expect_equal(8, end_act$late_start);
  expect_equal(9, end_act$late_finish);

})

test_that("SS type | A_duration < B_duration | lag < 0", {
  begin_id <- 187L
  a_id <- 188L
  b_id <- 189L
  end_id <- 190L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(-1, b_act$late_start);
  expect_equal(4, b_act$late_finish);
  expect_equal(4, end_act$late_start);
  expect_equal(5, end_act$late_finish);

})

test_that("SS type | A_duration > B_duration | lag == 0", {
  begin_id <- 193L
  a_id <- 194L
  b_id <- 195L
  end_id <- 196L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(1, b_act$early_start);
  expect_equal(4, b_act$early_finish);
  expect_equal(4, end_act$early_start);
  expect_equal(5, end_act$early_finish);

  expect_equal(1, begin_act$late_start);
  expect_equal(2, begin_act$late_finish);
  expect_equal(2, a_act$late_start);
  expect_equal(7, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

test_that("SS type | A_duration > B_duration | lag > 0", {
  begin_id <- 198L
  a_id <- 199L
  b_id <- 200L
  end_id <- 201L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(3, b_act$early_start);
  expect_equal(6, b_act$early_finish);
  expect_equal(6, end_act$early_start);
  expect_equal(7, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(3, b_act$late_start);
  expect_equal(6, b_act$late_finish);
  expect_equal(6, end_act$late_start);
  expect_equal(7, end_act$late_finish);

})

test_that("SS type | A_duration > B_duration | lag < 0", {
  begin_id <- 203L
  a_id <- 204L
  b_id <- 205L
  end_id <- 206L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SS", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(7, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(-1, b_act$early_start);
  expect_equal(2, b_act$early_finish);
  expect_equal(2, end_act$early_start);
  expect_equal(3, end_act$early_finish);

  expect_equal(3, begin_act$late_start);
  expect_equal(4, begin_act$late_finish);
  expect_equal(4, a_act$late_start);
  expect_equal(9, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

##############################
# START-FINISH relation type #
##############################

test_that("SF type | A_duration == B_duration | lag == 0", {
  begin_id <- 210L
  a_id <- 211L
  b_id <- 212L
  end_id <- 213L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-2, b_act$early_start);
  expect_equal(1, b_act$early_finish);
  expect_equal(1, end_act$early_start);
  expect_equal(2, end_act$early_finish);

  expect_equal(2, begin_act$late_start);
  expect_equal(3, begin_act$late_finish);
  expect_equal(3, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(0, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration == B_duration | lag > 0", {
  begin_id <- 215L
  a_id <- 216L
  b_id <- 217L
  end_id <- 218L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(4, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(0, b_act$early_start);
  expect_equal(3, b_act$early_finish);
  expect_equal(3, end_act$early_start);
  expect_equal(4, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(0, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration == B_duration | lag < 0", {
  begin_id <- 220L
  a_id <- 221L
  b_id <- 222L
  end_id <- 223L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-4, b_act$early_start);
  expect_equal(-1, b_act$early_finish);
  expect_equal(-1, end_act$early_start);
  expect_equal(0, end_act$early_finish);

  expect_equal(4, begin_act$late_start);
  expect_equal(5, begin_act$late_finish);
  expect_equal(5, a_act$late_start);
  expect_equal(8, a_act$late_finish);
  expect_equal(0, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration < B_duration | lag == 0", {
  begin_id <- 226L
  a_id <- 227L
  b_id <- 228L
  end_id <- 229L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-4, b_act$early_start);
  expect_equal(1, b_act$early_finish);
  expect_equal(1, end_act$early_start);
  expect_equal(2, end_act$early_finish);

  expect_equal(2, begin_act$late_start);
  expect_equal(3, begin_act$late_finish);
  expect_equal(3, a_act$late_start);
  expect_equal(6, a_act$late_finish);
  expect_equal(-2, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration < B_duration | lag > 0", {
  begin_id <- 231L
  a_id <- 232L
  b_id <- 233L
  end_id <- 234L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-2, b_act$early_start);
  expect_equal(3, b_act$early_finish);
  expect_equal(3, end_act$early_start);
  expect_equal(4, end_act$early_finish);

  expect_equal(0, begin_act$late_start);
  expect_equal(1, begin_act$late_finish);
  expect_equal(1, a_act$late_start);
  expect_equal(4, a_act$late_finish);
  expect_equal(-2, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration < B_duration | lag < 0", {
  begin_id <- 236L
  a_id <- 237L
  b_id <- 238L
  end_id <- 239L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 3L) %>%
    sch_add_activity(b_id, "B", 5L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(10, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(4, a_act$early_finish);
  expect_equal(-6, b_act$early_start);
  expect_equal(-1, b_act$early_finish);
  expect_equal(-1, end_act$early_start);
  expect_equal(0, end_act$early_finish);

  expect_equal(4, begin_act$late_start);
  expect_equal(5, begin_act$late_finish);
  expect_equal(5, a_act$late_start);
  expect_equal(8, a_act$late_finish);
  expect_equal(-2, b_act$late_start);
  expect_equal(3, b_act$late_finish);
  expect_equal(3, end_act$late_start);
  expect_equal(4, end_act$late_finish);

})

test_that("SF type | A_duration > B_duration | lag == 0", {
  begin_id <- 242L
  a_id <- 243L
  b_id <- 244L
  end_id <- 245L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 0L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(8, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(-2, b_act$early_start);
  expect_equal(1, b_act$early_finish);
  expect_equal(1, end_act$early_start);
  expect_equal(2, end_act$early_finish);

  expect_equal(4, begin_act$late_start);
  expect_equal(5, begin_act$late_finish);
  expect_equal(5, a_act$late_start);
  expect_equal(10, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

test_that("SF type | A_duration > B_duration | lag > 0", {
  begin_id <- 247L
  a_id <- 248L
  b_id <- 249L
  end_id <- 250L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", 2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(6, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(0, b_act$early_start);
  expect_equal(3, b_act$early_finish);
  expect_equal(3, end_act$early_start);
  expect_equal(4, end_act$early_finish);

  expect_equal(2, begin_act$late_start);
  expect_equal(3, begin_act$late_finish);
  expect_equal(3, a_act$late_start);
  expect_equal(8, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})

test_that("SF type | A_duration > B_duration | lag < 0", {
  begin_id <- 252L
  a_id <- 253L
  b_id <- 254L
  end_id <- 255L

  sch <- sch_new() %>%
    sch_add_activity(begin_id, "Begin", 1L) %>%
    sch_add_activity(a_id, "A", 5L) %>%
    sch_add_activity(b_id, "B", 3L) %>%
    sch_add_activity(end_id, "End", 1L) %>%

    sch_add_relation(begin_id, a_id,   "FS", 0L) %>%
    sch_add_relation(a_id,     b_id,   "SF", -2L) %>%
    sch_add_relation(b_id,     end_id, "FS", 0L) %>%
    sch_plan()

  begin_act <- sch_get_activity(sch, begin_id)
  a_act     <- sch_get_activity(sch, a_id    )
  b_act     <- sch_get_activity(sch, b_id    )
  end_act   <- sch_get_activity(sch, end_id  )

  expect_equal(10, sch_duration(sch))

  expect_equal(0, begin_act$early_start);
  expect_equal(1, begin_act$early_finish);
  expect_equal(1, a_act$early_start);
  expect_equal(6, a_act$early_finish);
  expect_equal(-4, b_act$early_start);
  expect_equal(-1, b_act$early_finish);
  expect_equal(-1, end_act$early_start);
  expect_equal(0, end_act$early_finish);

  expect_equal(6, begin_act$late_start);
  expect_equal(7, begin_act$late_finish);
  expect_equal(7, a_act$late_start);
  expect_equal(12, a_act$late_finish);
  expect_equal(2, b_act$late_start);
  expect_equal(5, b_act$late_finish);
  expect_equal(5, end_act$late_start);
  expect_equal(6, end_act$late_finish);

})
