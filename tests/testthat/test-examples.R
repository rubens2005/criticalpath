test_that("Examples: make_empty_schedule", {
  # Create an empty schedule
  sch1 <- make_empty_schedule("P1", "From criticalpath package")
  # Add activities to it
  sch1 <- add_activity(sch1, 1, "A", 5)
  sch1 <- add_activity(sch1, 2, "B", 4)
  sch1 <- add_activity(sch1, 3, "C", 2)
  sch1 <- add_activity(sch1, 4, "D", 3)
  sch1 <- add_activity(sch1, 5, "E", 4)
  sch1 <- add_activity(sch1, 6, "F", 3)
  # Add relations between activities
  sch1 <- add_relation(sch1, 1, 2)
  sch1 <- add_relation(sch1, 1, 3)
  sch1 <- add_relation(sch1, 2, 4)
  sch1 <- add_relation(sch1, 3, 5)
  sch1 <- add_relation(sch1, 4, 6)
  sch1 <- add_relation(sch1, 5, 6)
  # Get schedule info
  # What is the project duration?
  message(paste("Project duration:", sch1$info$duration))
  # Which activities are critical?
  sch1$activities$name[sch1$activities$critical]

  # Tests: not to be included in examples
  expect_equal(sch1$info$duration, 15)

  # Create an empty schedule
  sch2 <- make_empty_schedule("P2", "From criticalpath package")
  # Add activities to it and relations between activities in the same time
  sch2 <- add_act_rel(sch2, 1, "A", 5, c(2,3))
  sch2 <- add_act_rel(sch2, 2, "B", 4, c(4))
  sch2 <- add_act_rel(sch2, 3, "C", 2, c(5))
  sch2 <- add_act_rel(sch2, 4, "D", 3, c(6))
  sch2 <- add_act_rel(sch2, 5, "E", 4, c(6))
  sch2 <- add_act_rel(sch2, 6, "F", 3)
  # What is the critical path?
  sch2$relations[sch2$relations$critical, ]

  # Critical activities from projects P1 and P2 are the same.
  all.equal(sch1$activities$critical, sch2$activities$critical)

  # Tests: not to be included in examples
  expect_equal(sch1$info$duration, 15)
  expect_equal(sch1$activities$critical, sch2$activities$critical)

})

test_that("Examples: add_activity", {
  # Create an empty schedule
  sch <- make_empty_schedule("A Project", "From criticalpath package")
  # Add activities to it
  sch <- add_activity(sch, 1, "A", 5)
  sch <- add_activity(sch, 2, "B", 4)
  sch <- add_activity(sch, 3, "C", 2)
  sch <- add_activity(sch, 4, "D", 3)
  sch <- add_activity(sch, 5, "E", 5)
  sch <- add_activity(sch, 6, "F", 3)
  # Get schedule info
  # What is the project duration?
  message(paste("Project duration:", sch$info$duration))
  # Which activities are critical?
  sch$activities$name[sch$activities$critical]

  # Tests: not to be included in examples
  expect_equal(sch$info$duration, 5)
})

test_that("Examples: add_relation", {
  # Create an empty schedule
  sch <- make_empty_schedule("A Project", "From criticalpath package")
  # Add activities to it
  sch <- add_activity(sch, 1, "A", 5)
  sch <- add_activity(sch, 2, "B", 4)
  sch <- add_activity(sch, 3, "C", 2)
  sch <- add_activity(sch, 4, "D", 3)
  sch <- add_activity(sch, 5, "E", 4)
  sch <- add_activity(sch, 6, "F", 3)
  # Add relations between activities
  sch <- add_relation(sch, 1, 2)
  sch <- add_relation(sch, 1, 3)
  sch <- add_relation(sch, 2, 4)
  sch <- add_relation(sch, 3, 5)
  sch <- add_relation(sch, 4, 6)
  sch <- add_relation(sch, 5, 6)
  # Get schedule info
  # What is the project duration?
  message(paste("Project duration:", sch$info$duration))
  # Which activities are critical?
  sch$activities$name[sch$activities$critical]

  # Tests: not to be included in examples
  expect_equal(sch$info$duration, 15)
})

test_that("Examples: add_act_rel", {
  schedule <- make_empty_schedule(
    "Fictitious Project Example",
    "VANHOUCKE, Mario. Measuring time:
    improving project performance using earned value management.
    Gent: Springer, 2009, p. 18"
  )
  schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4))
  schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
  schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
  schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6))
  schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
  schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7))
  schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
  schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
  schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
  schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
  schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
  schedule <- add_act_rel(schedule, 12, "a12", 0)
  message("Schedule info:")
  schedule$info
  message("Activities info:")
  schedule$activities

  expect_equal(schedule$info$duration, 16)
})

test_that("Examples: schedule_from_data_frame", {

  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
  )
  relations <- data.frame(
    from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,  7,  8,  9,
             10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11, 12, 13, 14,
             15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
  )
  schedule <- schedule_from_data_frame(
    activities,
    relations,
    "Project 1: Cost Information System",
    "VANHOUCKE, Mario. Integrated project management and control:
    first comes the theory, then the practice. Gent: Springer, 2014, p. 6"
  )

  expect_equal(schedule$info$duration, 11)

})

test_that("Examples: change_durations", {
  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
  )
  relations <- data.frame(
    from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9,
             10, 11, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14,
             14, 14, 14, 14, 15, 16, 17, 16, 17)
  )
  schedule <- schedule_from_data_frame(
    activities,
    relations,
    "Project 2: Patient Transport System",
    "VANHOUCKE, Mario. Integrated project management and control:
    first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
  )
  message(paste("Schedule duration is", schedule$info$duration))

  expect_equal(schedule$info$duration, 25)

  new_durations <- c(1,2,5,4,3,2,1,5,3,5,5,3,4,2,1,2,4)
  schedule <- change_durations(schedule, new_durations)
  message(paste("Now, schedule duration is", schedule$info$duration))

  expect_equal(schedule$info$duration, 31)
})

test_that("Examples: gantt_matrix", {
  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
  )
  relations <- data.frame(
    from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9,
             10, 11, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14,
             14, 14, 14, 14, 15, 16, 17, 16, 17)
  )
  schedule <- schedule_from_data_frame(
    activities,
    relations,
    "Project 2: Patient Transport System",
    "VANHOUCKE, Mario. Integrated project management and control:
    first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
  )
  gantt <- gantt_matrix(schedule)
  effort_by_period <- colSums(gantt)
  s_curve <- cumsum(effort_by_period)
  #plot(s_curve, type="l", lwd=3)

  expect_equal(schedule$info$duration, 25)

})

test_that("Examples: xy_gantt_matrix", {
  activities <- data.frame(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
  )
  relations <- data.frame(
    from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9, 10,
             11, 12, 13, 13, 14, 14, 15, 15),
    to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14, 14,
             14, 14, 14, 15, 16, 17, 16, 17)
  )
  schedule <- schedule_from_data_frame(
    activities,
    relations,
    "Project 2: Patient Transport System",
    "VANHOUCKE, Mario. Integrated project management and control:
    first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
  )
  gantt <- gantt_matrix(schedule)
  xygantt <- xy_gantt_matrix(gantt)
  #plot(xygantt[,1], -xygantt[,2])

  expect_equal(schedule$info$duration, 25)

})

test_that("Examples: topological indicators", {
  schedule <- make_empty_schedule("P2", "From criticalpath package")
  schedule <- add_act_rel(schedule, 1, "A", 5, c(2,3))
  schedule <- add_act_rel(schedule, 2, "B", 4, c(4))
  schedule <- add_act_rel(schedule, 3, "C", 2, c(5))
  schedule <- add_act_rel(schedule, 4, "D", 3, c(6))
  schedule <- add_act_rel(schedule, 5, "E", 4, c(6))
  schedule <- add_act_rel(schedule, 6, "F", 3)
  topoi_sp(schedule)
  topoi_ad(schedule)
  topoi_la(schedule)
  topoi_tf(schedule)
})
