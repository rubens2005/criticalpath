#' SP Serial or Parallel Topological Indicator
#'
#' Shows the closeness of a network to a serial or parallel graph.
#' As the network becomes serial, the SP increase, until one;
#' As the network becomes parallel, the SP decrease until zero.
#'
#' @param sch A schedule object.
#'
#' @return A number between 0 and 1, inclusive.
#'
#' @seealso [sch_topoi_tf()], [sch_activities()], [sch_topoi_ad()],
#' [sch_xy_gantt_matrix()], [sch_relations()], [sch_topoi_la()],
#' [sch_add_activities()], [sch_add_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2L,3L,4L) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5L) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6L) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9L) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7L) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8L,11L) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12L) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12L) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_topoi_sp(sch) # 0.4545455
#'
#' @export
sch_topoi_sp <- function(sch) {
  max_level <- sch$info$max_level
  nr_act <- sch$info$nr_activities

  if(nr_act == 0L) {
    return(NA)
  }

  if(nr_act == 1L) {
    return(1)
  }

  return(((max_level - 1) / (nr_act - 1)))
}

#' AD Activity Distribution Topological Indicator
#'
#' Measures the distribution of the activities over the levels.
#' If AD is approximately equal zero, each level has same numbers of activities.
#' Otherwise, if AD is equal one, the quantity of each level is not
#' uniformly distributed.
#'
#' @param sch A schedule object.
#'
#' @return A number between 0 and 1, inclusive.
#'
#' @seealso [sch_topoi_sp()], [sch_topoi_la()], [sch_topoi_tf()],
#' [sch_xy_gantt_matrix()], [sch_add_relations()], [sch_add_activities()],
#' [sch_relations()], [sch_activities()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2L,3L,4L) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5L) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6L) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9L) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7L) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8L,11L) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12L) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12L) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_topoi_ad(sch) # 0.4
#'
#' @export
sch_topoi_ad <- function(sch) {
  max_level <- sch$info$max_level
  nr_act <- sch$info$nr_activities

  if(nr_act == 0L) {
    return(NA)
  }

  if(max_level == 1L || max_level == nr_act) {
    return(0);
  }

  # Activities quantities by level: they are called width, width by level
  wi <- table(sch$activities$progr_level)
  # w mean
  wbar <- nr_act / max_level
  absolute_mean_deviation <- base::sum(base::abs(wi - wbar))

  return(absolute_mean_deviation / (2 * (max_level - 1) * (wbar - 1)))
}

#' LA Length of Arcs Topological Indicator
#'
#' Measures the presence of long arcs based on the difference between
#' the progressive level of the end activity and the start node
#' of each relation.
#' If LA is approximately equal zero, the progressive level between
#' activities is as far as possible.
#' Otherwise, if LA is equal one, the relation distance are one.
#'
#' @param sch A schedule object.
#'
#' @return A number between 0 and 1, inclusive.
#'
#' @seealso [sch_topoi_sp()], [sch_add_relations()], [sch_topoi_ad()],
#' [sch_relations()], [sch_xy_gantt_matrix()], [sch_activities()],
#' [sch_topoi_tf()], [sch_add_activities()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2L,3L,4L) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5L) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6L) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9L) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7L) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8L,11L) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12L) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12L) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_topoi_la(sch) # 0.07692308
#'
#' @export
sch_topoi_la <- function(sch) {
  max_level <- sch$info$max_level
  nr_act <- sch$info$nr_activities

  if(nr_act == 0L) {
    return(NA)
  }

  # Activities quantities by level: they are called width, width by level
  wi <- as.integer(base::table(sch$activities$progr_level))

  D <- base::sum(wi[-base::length(wi)] * wi[-1L])

  if(D == nr_act - wi[1L]) {
    return(1)
  }

  len <- 1L
  levels_from <- sch$activities$progr_level[sch$relations$i_from]
  levels_to <- sch$activities$progr_level[sch$relations$i_to]
  arcs_qty <- base::sum(levels_to - levels_from == len)

  return((arcs_qty - nr_act + wi[1L]) / (D - nr_act + wi[1L]))
}

#' TF Topological Float Indicator
#'
#' Measures the topological float of each activity.
#' If TF = 0, there is no float between activities.
#' If TF = 1, there is float between activities
#' and they be shift without affecting other activities.
#'
#' @param sch A schedule object.
#'
#' @return A number between 0 and 1, inclusive.
#'
#' @seealso [sch_topoi_ad()], [sch_add_activities()], [sch_add_relations()],
#' [sch_xy_gantt_matrix()], [sch_topoi_la()], [sch_activities()],
#' [sch_relations()], [sch_topoi_sp()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2L,3L,4L) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5L) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6L) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9L) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7L) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8L,11L) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12L) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12L) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_topoi_tf(sch) # 0.2333333
#'
#' @export
sch_topoi_tf <- function(sch) {
  max_level <- sch$info$max_level
  nr_act <- sch$info$nr_activities

  if(nr_act == 0L) {
    return(NA)
  }

  if(max_level == 1L || max_level == nr_act) {
    return(0);
  }

  level_diff <- sch$activities$regr_level - sch$activities$progr_level

  return(base::sum(level_diff) / ((max_level - 1) * (nr_act - max_level)))
}
