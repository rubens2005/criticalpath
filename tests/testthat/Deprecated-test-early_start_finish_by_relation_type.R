##############################
# FINISH-START relation type #
##############################

test_that("FS type | A_duration == B_duration | lag == 0", {
  begin_id <- 31
  a_id <- 32
  b_id <- 33
  end_id <- 34

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(4, b_act$ES);
  expect_equal(7, b_act$EF);
  expect_equal(7, end_act$ES);
  expect_equal(8, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(4, b_act$LS);
  expect_equal(7, b_act$LF);
  expect_equal(7, end_act$LS);
  expect_equal(8, end_act$LF);

})

test_that("FS type | A_duration == B_duration | lag > 0", {
  begin_id <- 36
  a_id <- 37
  b_id <- 38
  end_id <- 39

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(10, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(6, b_act$ES);
  expect_equal(9, b_act$EF);
  expect_equal(9, end_act$ES);
  expect_equal(10, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(6, b_act$LS);
  expect_equal(9, b_act$LF);
  expect_equal(9, end_act$LS);
  expect_equal(10, end_act$LF);

})

test_that("FS type | A_duration == B_duration | lag < 0", {
  begin_id <- 41
  a_id <- 42
  b_id <- 43
  end_id <- 44

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(2, b_act$ES);
  expect_equal(5, b_act$EF);
  expect_equal(5, end_act$ES);
  expect_equal(6, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

test_that("FS type | A_duration < B_duration | lag == 0", {
  begin_id <- 79
  a_id <- 80
  b_id <- 81
  end_id <- 82

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(10, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(4, b_act$ES);
  expect_equal(9, b_act$EF);
  expect_equal(9, end_act$ES);
  expect_equal(10, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(4, b_act$LS);
  expect_equal(9, b_act$LF);
  expect_equal(9, end_act$LS);
  expect_equal(10, end_act$LF);

})

test_that("FS type | A_duration < B_duration | lag > 0", {
  begin_id <- 84
  a_id <- 85
  b_id <- 86
  end_id <- 87

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(12, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(6, b_act$ES);
  expect_equal(11, b_act$EF);
  expect_equal(11, end_act$ES);
  expect_equal(12, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(6, b_act$LS);
  expect_equal(11, b_act$LF);
  expect_equal(11, end_act$LS);
  expect_equal(12, end_act$LF);

})

test_that("FS type | A_duration < B_duration | lag < 0", {
  begin_id <- 89
  a_id <- 90
  b_id <- 91
  end_id <- 92

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(2, b_act$ES);
  expect_equal(7, b_act$EF);
  expect_equal(7, end_act$ES);
  expect_equal(8, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(7, b_act$LF);
  expect_equal(7, end_act$LS);
  expect_equal(8, end_act$LF);

})

test_that("FS type | A_duration > B_duration | lag == 0", {
  begin_id <- 95
  a_id <- 96
  b_id <- 97
  end_id <- 98

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(10, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(6, b_act$ES);
  expect_equal(9, b_act$EF);
  expect_equal(9, end_act$ES);
  expect_equal(10, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(6, b_act$LS);
  expect_equal(9, b_act$LF);
  expect_equal(9, end_act$LS);
  expect_equal(10, end_act$LF);

})

test_that("FS type | A_duration > B_duration | lag > 0", {
  begin_id <- 100
  a_id <- 101
  b_id <- 102
  end_id <- 104

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(12, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(8, b_act$ES);
  expect_equal(11, b_act$EF);
  expect_equal(11, end_act$ES);
  expect_equal(12, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(8, b_act$LS);
  expect_equal(11, b_act$LF);
  expect_equal(11, end_act$LS);
  expect_equal(12, end_act$LF);

})

test_that("FS type | A_duration > B_duration | lag < 0", {
  begin_id <- 105
  a_id <- 106
  b_id <- 107
  end_id <- 108

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(4, b_act$ES);
  expect_equal(7, b_act$EF);
  expect_equal(7, end_act$ES);
  expect_equal(8, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(4, b_act$LS);
  expect_equal(7, b_act$LF);
  expect_equal(7, end_act$LS);
  expect_equal(8, end_act$LF);

})

###############################
# FINISH-FINISH relation type #
###############################

test_that("FF type | A_duration == B_duration | lag == 0", {
  begin_id <- 112
  a_id <- 113
  b_id <- 114
  end_id <- 115

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(5, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(1, b_act$LS);
  expect_equal(4, b_act$LF);
  expect_equal(4, end_act$LS);
  expect_equal(5, end_act$LF);

})

test_that("FF type | A_duration == B_duration | lag > 0", {
  begin_id <- 117
  a_id <- 118
  b_id <- 119
  end_id <- 120

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(3, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(3, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("FF type | A_duration == B_duration | lag < 0", {
  begin_id <- 122
  a_id <- 123
  b_id <- 124
  end_id <- 125

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(5, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-1, b_act$ES);
  expect_equal(2, b_act$EF);
  expect_equal(2, end_act$ES);
  expect_equal(3, end_act$EF);

  expect_equal(1, begin_act$LS);
  expect_equal(2, begin_act$LF);
  expect_equal(2, a_act$LS);
  expect_equal(5, a_act$LF);
  expect_equal(0, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("FF type | A_duration < B_duration | lag == 0", {
  begin_id <- 128
  a_id <- 129
  b_id <- 130
  end_id <- 131

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(-1, b_act$LS);
  expect_equal(4, b_act$LF);
  expect_equal(4, end_act$LS);
  expect_equal(5, end_act$LF);

})

test_that("FF type | A_duration < B_duration | lag > 0", {
  begin_id <- 133
  a_id <- 134
  b_id <- 135
  end_id <- 136

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(1, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("FF type | A_duration < B_duration | lag < 0", {
  begin_id <- 138
  a_id <- 139
  b_id <- 140
  end_id <- 141

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-3, b_act$ES);
  expect_equal(2, b_act$EF);
  expect_equal(2, end_act$ES);
  expect_equal(3, end_act$EF);

  expect_equal(1, begin_act$LS);
  expect_equal(2, begin_act$LF);
  expect_equal(2, a_act$LS);
  expect_equal(5, a_act$LF);
  expect_equal(-2, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("FF type | A_duration > B_duration | lag == 0", {
  begin_id <- 144
  a_id <- 145
  b_id <- 146
  end_id <- 147

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(3, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(3, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("FF type | A_duration > B_duration | lag > 0", {
  begin_id <- 149
  a_id <- 150
  b_id <- 151
  end_id <- 152

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(9, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(5, b_act$ES);
  expect_equal(8, b_act$EF);
  expect_equal(8, end_act$ES);
  expect_equal(9, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(5, b_act$LS);
  expect_equal(8, b_act$LF);
  expect_equal(8, end_act$LS);
  expect_equal(9, end_act$LF);

})

test_that("FF type | A_duration > B_duration | lag < 0", {
  begin_id <- 154
  a_id <- 155
  b_id <- 156
  end_id <- 157

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "FF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(1, begin_act$LS);
  expect_equal(2, begin_act$LF);
  expect_equal(2, a_act$LS);
  expect_equal(7, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

#############################
# START-START relation type #
#############################

test_that("SS type | A_duration == B_duration | lag == 0", {
  begin_id <- 161
  a_id <- 162
  b_id <- 163
  end_id <- 164

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(5, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(1, b_act$LS);
  expect_equal(4, b_act$LF);
  expect_equal(4, end_act$LS);
  expect_equal(5, end_act$LF);

})

test_that("SS type | A_duration == B_duration | lag > 0", {
  begin_id <- 166
  a_id <- 167
  b_id <- 168
  end_id <- 169

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(3, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(3, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("SS type | A_duration == B_duration | lag < 0", {
  begin_id <- 171
  a_id <- 172
  b_id <- 173
  end_id <- 174

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(5, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-1, b_act$ES);
  expect_equal(2, b_act$EF);
  expect_equal(2, end_act$ES);
  expect_equal(3, end_act$EF);

  expect_equal(1, begin_act$LS);
  expect_equal(2, begin_act$LF);
  expect_equal(2, a_act$LS);
  expect_equal(5, a_act$LF);
  expect_equal(0, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SS type | A_duration < B_duration | lag == 0", {
  begin_id <- 177
  a_id <- 178
  b_id <- 179
  end_id <- 180

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(1, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("SS type | A_duration < B_duration | lag > 0", {
  begin_id <- 182
  a_id <- 183
  b_id <- 184
  end_id <- 185

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(9, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(3, b_act$ES);
  expect_equal(8, b_act$EF);
  expect_equal(8, end_act$ES);
  expect_equal(9, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(3, b_act$LS);
  expect_equal(8, b_act$LF);
  expect_equal(8, end_act$LS);
  expect_equal(9, end_act$LF);

})

test_that("SS type | A_duration < B_duration | lag < 0", {
  begin_id <- 187
  a_id <- 188
  b_id <- 189
  end_id <- 190

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(-1, b_act$LS);
  expect_equal(4, b_act$LF);
  expect_equal(4, end_act$LS);
  expect_equal(5, end_act$LF);

})

test_that("SS type | A_duration > B_duration | lag == 0", {
  begin_id <- 193
  a_id <- 194
  b_id <- 195
  end_id <- 196

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(1, b_act$ES);
  expect_equal(4, b_act$EF);
  expect_equal(4, end_act$ES);
  expect_equal(5, end_act$EF);

  expect_equal(1, begin_act$LS);
  expect_equal(2, begin_act$LF);
  expect_equal(2, a_act$LS);
  expect_equal(7, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

test_that("SS type | A_duration > B_duration | lag > 0", {
  begin_id <- 198
  a_id <- 199
  b_id <- 200
  end_id <- 201

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(3, b_act$ES);
  expect_equal(6, b_act$EF);
  expect_equal(6, end_act$ES);
  expect_equal(7, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(3, b_act$LS);
  expect_equal(6, b_act$LF);
  expect_equal(6, end_act$LS);
  expect_equal(7, end_act$LF);

})

test_that("SS type | A_duration > B_duration | lag < 0", {
  begin_id <- 203
  a_id <- 204
  b_id <- 205
  end_id <- 206

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SS", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(7, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(-1, b_act$ES);
  expect_equal(2, b_act$EF);
  expect_equal(2, end_act$ES);
  expect_equal(3, end_act$EF);

  expect_equal(3, begin_act$LS);
  expect_equal(4, begin_act$LF);
  expect_equal(4, a_act$LS);
  expect_equal(9, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

##############################
# START-FINISH relation type #
##############################

test_that("SF type | A_duration == B_duration | lag == 0", {
  begin_id <- 210
  a_id <- 211
  b_id <- 212
  end_id <- 213

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-2, b_act$ES);
  expect_equal(1, b_act$EF);
  expect_equal(1, end_act$ES);
  expect_equal(2, end_act$EF);

  expect_equal(2, begin_act$LS);
  expect_equal(3, begin_act$LF);
  expect_equal(3, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(0, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration == B_duration | lag > 0", {
  begin_id <- 215
  a_id <- 216
  b_id <- 217
  end_id <- 218

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(4, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(0, b_act$ES);
  expect_equal(3, b_act$EF);
  expect_equal(3, end_act$ES);
  expect_equal(4, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(0, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration == B_duration | lag < 0", {
  begin_id <- 220
  a_id <- 221
  b_id <- 222
  end_id <- 223

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-4, b_act$ES);
  expect_equal(-1, b_act$EF);
  expect_equal(-1, end_act$ES);
  expect_equal(0, end_act$EF);

  expect_equal(4, begin_act$LS);
  expect_equal(5, begin_act$LF);
  expect_equal(5, a_act$LS);
  expect_equal(8, a_act$LF);
  expect_equal(0, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration < B_duration | lag == 0", {
  begin_id <- 226
  a_id <- 227
  b_id <- 228
  end_id <- 229

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-4, b_act$ES);
  expect_equal(1, b_act$EF);
  expect_equal(1, end_act$ES);
  expect_equal(2, end_act$EF);

  expect_equal(2, begin_act$LS);
  expect_equal(3, begin_act$LF);
  expect_equal(3, a_act$LS);
  expect_equal(6, a_act$LF);
  expect_equal(-2, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration < B_duration | lag > 0", {
  begin_id <- 231
  a_id <- 232
  b_id <- 233
  end_id <- 234

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-2, b_act$ES);
  expect_equal(3, b_act$EF);
  expect_equal(3, end_act$ES);
  expect_equal(4, end_act$EF);

  expect_equal(0, begin_act$LS);
  expect_equal(1, begin_act$LF);
  expect_equal(1, a_act$LS);
  expect_equal(4, a_act$LF);
  expect_equal(-2, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration < B_duration | lag < 0", {
  begin_id <- 236
  a_id <- 237
  b_id <- 238
  end_id <- 239

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 3)
  schedule$add_activity(b_id, "B", 5)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(10, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(4, a_act$EF);
  expect_equal(-6, b_act$ES);
  expect_equal(-1, b_act$EF);
  expect_equal(-1, end_act$ES);
  expect_equal(0, end_act$EF);

  expect_equal(4, begin_act$LS);
  expect_equal(5, begin_act$LF);
  expect_equal(5, a_act$LS);
  expect_equal(8, a_act$LF);
  expect_equal(-2, b_act$LS);
  expect_equal(3, b_act$LF);
  expect_equal(3, end_act$LS);
  expect_equal(4, end_act$LF);

})

test_that("SF type | A_duration > B_duration | lag == 0", {
  begin_id <- 242
  a_id <- 243
  b_id <- 244
  end_id <- 245

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 0)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(8, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(-2, b_act$ES);
  expect_equal(1, b_act$EF);
  expect_equal(1, end_act$ES);
  expect_equal(2, end_act$EF);

  expect_equal(4, begin_act$LS);
  expect_equal(5, begin_act$LF);
  expect_equal(5, a_act$LS);
  expect_equal(10, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

test_that("SF type | A_duration > B_duration | lag > 0", {
  begin_id <- 247
  a_id <- 248
  b_id <- 249
  end_id <- 250

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", 2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(6, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(0, b_act$ES);
  expect_equal(3, b_act$EF);
  expect_equal(3, end_act$ES);
  expect_equal(4, end_act$EF);

  expect_equal(2, begin_act$LS);
  expect_equal(3, begin_act$LF);
  expect_equal(3, a_act$LS);
  expect_equal(8, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})

test_that("SF type | A_duration > B_duration | lag < 0", {
  begin_id <- 252
  a_id <- 253
  b_id <- 254
  end_id <- 255

  schedule <- Schedule$new()
  schedule$add_activity(begin_id, "Begin", 1)
  schedule$add_activity(a_id, "A", 5)
  schedule$add_activity(b_id, "B", 3)
  schedule$add_activity(end_id, "End", 1)

  schedule$add_relation(begin_id, a_id,   "FS", 0)
  schedule$add_relation(a_id,     b_id,   "SF", -2)
  schedule$add_relation(b_id,     end_id, "FS", 0)

  begin_act <- schedule$get_activity(begin_id)
  a_act     <- schedule$get_activity(a_id    )
  b_act     <- schedule$get_activity(b_id    )
  end_act   <- schedule$get_activity(end_id  )

  expect_equal(10, schedule$duration)

  expect_equal(0, begin_act$ES);
  expect_equal(1, begin_act$EF);
  expect_equal(1, a_act$ES);
  expect_equal(6, a_act$EF);
  expect_equal(-4, b_act$ES);
  expect_equal(-1, b_act$EF);
  expect_equal(-1, end_act$ES);
  expect_equal(0, end_act$EF);

  expect_equal(6, begin_act$LS);
  expect_equal(7, begin_act$LF);
  expect_equal(7, a_act$LS);
  expect_equal(12, a_act$LF);
  expect_equal(2, b_act$LS);
  expect_equal(5, b_act$LF);
  expect_equal(5, end_act$LS);
  expect_equal(6, end_act$LF);

})
