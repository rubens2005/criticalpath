test_that("Creating a schedule step-by-step: first with activities and then with relations", {

  schedule <- Schedule$new()
  schedule$title <- "Project 3: Old Carriage House Renovation"
  schedule$reference <-
  "VANHOUCKE, Mario. Integrated project management and control:
  first comes the theory, then the practice. Gent: Springer, 2014, p. 11"

  schedule$add_activity(1, "a1" , 2)
  schedule$add_activity(2, "a2" , 2)
  schedule$add_activity(3, "a3" , 4)
  schedule$add_activity(4, "a4" , 3)
  schedule$add_activity(5, "a5" , 4)
  schedule$add_activity(6, "a6" , 1)
  schedule$add_activity(7, "a7" , 1)
  schedule$add_activity(8, "a8" , 1)
  schedule$add_activity(9, "a9" , 1)
  schedule$add_activity(10, "a10", 1)
  schedule$add_activity(11, "a11", 3)
  schedule$add_activity(12, "a12", 2)
  schedule$add_activity(13, "a13", 1)
  schedule$add_activity(14, "a14", 1)
  schedule$add_activity(15, "a15", 2)
  schedule$add_activity(16, "a16", 1)
  schedule$add_activity(17, "a17", 1)

  expect_equal(schedule$duration, 4)

  schedule$add_relation( 1, 2)
  expect_equal(schedule$duration, 4)

  schedule$add_relation( 2, 3)
  expect_equal(schedule$duration, 8)

  schedule$add_relation( 3, 4)
  expect_equal(schedule$duration, 11)

  schedule$add_relation( 4, 5)
  expect_equal(schedule$duration, 15)

  schedule$add_relation( 5, 6)
  expect_equal(schedule$duration, 16)

  schedule$add_relation( 6, 7)
  expect_equal(schedule$duration, 17)

  schedule$add_relation( 6, 8)
  expect_equal(schedule$duration, 17)

  schedule$add_relation( 6, 9)
  expect_equal(schedule$duration, 17)

  schedule$add_relation( 7, 10)
  expect_equal(schedule$duration, 18)

  schedule$add_relation( 8, 10)
  expect_equal(schedule$duration, 18)

  schedule$add_relation( 9, 10)
  expect_equal(schedule$duration, 18 )

  schedule$add_relation( 10, 11)
  expect_equal(schedule$duration, 21)

  schedule$add_relation( 10, 13)
  expect_equal(schedule$duration, 21)

  schedule$add_relation( 11, 12)
  expect_equal(schedule$duration, 23)

  schedule$add_relation( 12, 15)
  expect_equal(schedule$duration, 25)

  schedule$add_relation( 13, 14)
  expect_equal(schedule$duration, 25)

  schedule$add_relation( 14, 15)
  expect_equal(schedule$duration, 25)

  schedule$add_relation( 15, 16)
  expect_equal(schedule$duration, 26)

  schedule$add_relation( 16, 17)
  expect_equal(schedule$duration, 27)

})
