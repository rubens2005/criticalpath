#' TF Topological Float Indicator
#'
#' Measures the topological float of each activity (1, 20).
#'
#' @usage
#' topoi_tf(schedule)
#'
#' @param schedule A Schedule object.
#'
#' @return A number between 0 and 1, inclusive.
#'
#' @references
#'
#' Csardi, G. & Nepusz, T. (2005).
#' The Igraph Software Package for Complex Network Research.
#' *InterJournal*. Complex Systems. 1695.
#' [Article](https://www.researchgate.net/publication/221995787_The_Igraph_Software_Package_for_Complex_Network_Research)
#'  / [igraph](https://igraph.org/)
#'
#' Vanhoucke, M. (2009) *Measuring Time*:
#' Improving Project Performance Using Earned Value Management.
#' Springer-Verlag US.
#' doi: [10.1007/978-1-4419-1014-1](https://doi.org/10.1007/978-1-4419-1014-1).
#'
#' Vanhoucke, M. (2013) *Project Management with Dynamic Scheduling*:
#' Baseline Scheduling, Risk Analysis and Project Control.
#' Springer-Verlag Berlin Heidelberg.
#' doi: [10.1007/978-3-642-40438-2](https://doi.org/10.1007/978-3-642-40438-2)
#'
#' Vanhoucke, M. (2014) *Integrated Project Management and Control*:
#' First Comes the Theory, then the Practice.
#' Springer International Publishing Switzerland.
#' doi: [10.1007/978-3-319-04331-9](https://doi.org/10.1007/978-3-319-04331-9)
#'
#' @seealso
#'
#' Creating a schedule:
#' [schedule_from_data_frame()]
#' [make_empty_schedule()]
#' [add_activity()]
#' [add_relation()]
#' [add_act_rel()]
#'
#' Changing the activities duration:
#' [change_durations()]
#'
#' Gantt Matrix:
#' [gantt_matrix()]
#' [xy_gantt_matrix()]
#'
#' Topological indicators:
#' [topoi_sp()]
#' [topoi_ad()]
#' [topoi_la()]
#' [topoi_tf()]
#'
#' @export
#'
#' @examples
#' schedule <- make_empty_schedule("P2", "From criticalpath package")
#' schedule <- add_act_rel(schedule, 1, "A", 5, c(2,3))
#' schedule <- add_act_rel(schedule, 2, "B", 4, c(4))
#' schedule <- add_act_rel(schedule, 3, "C", 2, c(5))
#' schedule <- add_act_rel(schedule, 4, "D", 3, c(6))
#' schedule <- add_act_rel(schedule, 5, "E", 4, c(6))
#' schedule <- add_act_rel(schedule, 6, "F", 3)
#' topoi_tf(schedule)
#'
topoi_tf <- function(schedule) {
  assert_is_schedule(schedule)

  max_level <- schedule$info$max_level
  nr_activities <- schedule$info$nr_activities

  if(nr_activities == 0) {
    return(NA)
  }

  if(max_level == 1 || max_level == nr_activities) {
    return(0);
  }

  level_diff <- schedule$activities$regr_level - schedule$activities$progr_level

  sum(level_diff) / ((max_level - 1) * (nr_activities - max_level))
}
