test_that("Creating a sch step-by-step: first with activities and then with relations.", {
  sch <- sch_new() %>%
    sch_title("Project 3: Old Carriage House Renovation") %>%
    sch_reference(
      "VANHOUCKE, Mario. Integrated project management and control:
       first comes the theory, then the practice. Gent: Springer, 2014, p. 11"
    ) %>%
    sch_add_activity( 1L, "a1" , 2L) %>%
    sch_add_activity( 2L, "a2" , 2L) %>%
    sch_add_activity( 3L, "a3" , 4L) %>%
    sch_add_activity( 4L, "a4" , 3L) %>%
    sch_add_activity( 5L, "a5" , 4L) %>%
    sch_add_activity( 6L, "a6" , 1L) %>%
    sch_add_activity( 7L, "a7" , 1L) %>%
    sch_add_activity( 8L, "a8" , 1L) %>%
    sch_add_activity( 9L, "a9" , 1L) %>%
    sch_add_activity(10L, "a10", 1L) %>%
    sch_add_activity(11L, "a11", 3L) %>%
    sch_add_activity(12L, "a12", 2L) %>%
    sch_add_activity(13L, "a13", 1L) %>%
    sch_add_activity(14L, "a14", 1L) %>%
    sch_add_activity(15L, "a15", 2L) %>%
    sch_add_activity(16L, "a16", 1L) %>%
    sch_add_activity(17L, "a17", 1L) %>%
    sch_plan()

  expect_equal(sch_duration(sch), 4)

  sch %<>% sch_add_relation( 1L, 2L) %>% sch_plan()
  expect_equal(sch_duration(sch), 4)

  sch %<>% sch_add_relation( 2L, 3L) %>% sch_plan()
  expect_equal(sch_duration(sch), 8)

  sch %<>% sch_add_relation( 3L, 4L) %>% sch_plan()
  expect_equal(sch_duration(sch), 11)

  sch %<>% sch_add_relation( 4L, 5L) %>% sch_plan()
  expect_equal(sch_duration(sch), 15)

  sch %<>% sch_add_relation( 5L, 6L) %>% sch_plan()
  expect_equal(sch_duration(sch), 16)

  sch %<>% sch_add_relation( 6L, 7L) %>% sch_plan()
  expect_equal(sch_duration(sch), 17)

  sch %<>% sch_add_relation( 6L, 8L) %>% sch_plan()
  expect_equal(sch_duration(sch), 17)

  sch %<>% sch_add_relation( 6L, 9L) %>% sch_plan()
  expect_equal(sch_duration(sch), 17)

  sch %<>% sch_add_relation( 7L, 10L) %>% sch_plan()
  expect_equal(sch_duration(sch), 18)

  sch %<>% sch_add_relation( 8L, 10L) %>% sch_plan()
  expect_equal(sch_duration(sch), 18)

  sch %<>% sch_add_relation( 9L, 10L) %>% sch_plan()
  expect_equal(sch_duration(sch), 18 )

  sch %<>% sch_add_relation( 10L, 11L) %>% sch_plan()
  expect_equal(sch_duration(sch), 21)

  sch %<>% sch_add_relation( 10L, 13L) %>% sch_plan()
  expect_equal(sch_duration(sch), 21)

  sch %<>% sch_add_relation( 11L, 12L) %>% sch_plan()
  expect_equal(sch_duration(sch), 23)

  sch %<>% sch_add_relation( 12L, 15L) %>% sch_plan()
  expect_equal(sch_duration(sch), 25)

  sch %<>% sch_add_relation( 13L, 14L) %>% sch_plan()
  expect_equal(sch_duration(sch), 25)

  sch %<>% sch_add_relation( 14L, 15L) %>% sch_plan()
  expect_equal(sch_duration(sch), 25)

  sch %<>% sch_add_relation( 15L, 16L) %>% sch_plan()
  expect_equal(sch_duration(sch), 26)

  sch %<>% sch_add_relation( 16L, 17L) %>% sch_plan()
  expect_equal(sch_duration(sch), 27)

})

test_that("Add relations tibble.", {
  atb <- tibble::tibble(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
  )
  rtb <- data.frame(
    from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
             7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
    to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
             12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
  )
  sch <- sch_new() %>%
    sch_add_activities_tibble(atb) %>%
    sch_add_relations_tibble(rtb) %>%
    sch_plan()
  expect_equal(sch_duration(sch), 11L)
})


test_that("Evaluete redundancy.", {
  atb <- tibble::tibble(
    id        = 1:17,
    name      = paste("a", as.character(1:17), sep=""),
    duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
  )
  rtb <- data.frame(
    from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
             7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
    to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
             12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
  )
  sch <- sch_new() %>%
    sch_title("Project 1: Cost Information System") %>%
    sch_reference("VANHOUCKE, Mario.
      Integrated project management and control:
      first comes the theory, then the practice.
      Gent: Springer, 2014, p. 6") %>%
    sch_add_activities_tibble(atb) %>%
    sch_add_relations_tibble(rtb) %>%
    sch_plan() %>%
    sch_evaluate_redundancy()

  expect_equal(sch_duration(sch), 11L)

  rtb <- sch_relations(sch)
  expect_equal(sum(rtb$redundant), 0)

  sch1 <- sch %>%
    sch_add_relation(1L, 6L) %>%
    sch_add_relation(3L, 16L) %>%
    sch_add_relation(4L, 17L) %>%
    sch_plan() %>%
    sch_evaluate_redundancy()

  expect_equal(sch_duration(sch1), 11L)

  rtb <- sch_relations(sch1)
  expect_equal(sum(rtb$redundant), 3)

})

