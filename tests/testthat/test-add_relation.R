test_that("Creating a schedule step-by-step: first with activities and then with relations", {

  schedule <- make_empty_schedule(
    "Project 3: Old Carriage House Renovation",
    "VANHOUCKE, Mario. Integrated project management and control: first comes the theory, then the practice. Gent: Springer, 2014, p. 11"
  )

  schedule <- add_activity(schedule,  1, "a1" , 2)
  schedule <- add_activity(schedule,  2, "a2" , 2)
  schedule <- add_activity(schedule,  3, "a3" , 4)
  schedule <- add_activity(schedule,  4, "a4" , 3)
  schedule <- add_activity(schedule,  5, "a5" , 4)
  schedule <- add_activity(schedule,  6, "a6" , 1)
  schedule <- add_activity(schedule,  7, "a7" , 1)
  schedule <- add_activity(schedule,  8, "a8" , 1)
  schedule <- add_activity(schedule,  9, "a9" , 1)
  schedule <- add_activity(schedule, 10, "a10", 1)
  schedule <- add_activity(schedule, 11, "a11", 3)
  schedule <- add_activity(schedule, 12, "a12", 2)
  schedule <- add_activity(schedule, 13, "a13", 1)
  schedule <- add_activity(schedule, 14, "a14", 1)
  schedule <- add_activity(schedule, 15, "a15", 2)
  schedule <- add_activity(schedule, 16, "a16", 1)
  schedule <- add_activity(schedule, 17, "a17", 1)

  expect_equal(schedule$info$duration, 4)

  schedule <- add_relation(schedule, 1, 2)
  expect_equal(schedule$info$duration, 4)

  schedule <- add_relation(schedule, 2, 3)
  expect_equal(schedule$info$duration, 8)

  schedule <- add_relation(schedule, 3, 4)
  expect_equal(schedule$info$duration, 11)

  schedule <- add_relation(schedule, 4, 5)
  expect_equal(schedule$info$duration, 15)

  schedule <- add_relation(schedule, 5, 6)
  expect_equal(schedule$info$duration, 16)

  schedule <- add_relation(schedule, 6, 7)
  expect_equal(schedule$info$duration, 17)

  schedule <- add_relation(schedule, 6, 8)
  expect_equal(schedule$info$duration, 17)

  schedule <- add_relation(schedule, 6, 9)
  expect_equal(schedule$info$duration, 17)

  schedule <- add_relation(schedule, 7, 10)
  expect_equal(schedule$info$duration, 18)

  schedule <- add_relation(schedule, 8, 10)
  expect_equal(schedule$info$duration, 18)

  schedule <- add_relation(schedule, 9, 10)
  expect_equal(schedule$info$duration, 18 )

  schedule <- add_relation(schedule, 10, 11)
  expect_equal(schedule$info$duration, 21)

  schedule <- add_relation(schedule, 10, 13)
  expect_equal(schedule$info$duration, 21)

  schedule <- add_relation(schedule, 11, 12)
  expect_equal(schedule$info$duration, 23)

  schedule <- add_relation(schedule, 12, 15)
  expect_equal(schedule$info$duration, 25)

  schedule <- add_relation(schedule, 13, 14)
  expect_equal(schedule$info$duration, 25)

  schedule <- add_relation(schedule, 14, 15)
  expect_equal(schedule$info$duration, 25)

  schedule <- add_relation(schedule, 15, 16)
  expect_equal(schedule$info$duration, 26)

  schedule <- add_relation(schedule, 16, 17)
  expect_equal(schedule$info$duration, 27)

})
