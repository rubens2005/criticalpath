#' Change Activities Duration
#'
#' Change activities duration and calculate critical path.
#' This way is faster than creating a new schedule with new durations.
#'
#' @param schedule
#' Schedule in which the activities' durations will be changed.
#'
#' @param durations A vector with new durations of activities.
#'
#' @return A Schedule object.
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
#' activities <- data.frame(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
#' )
#' relations <- data.frame(
#'   from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9,
#'            10, 11, 12, 13, 13, 14, 14, 15, 15),
#'   to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14,
#'            14, 14, 14, 14, 15, 16, 17, 16, 17)
#' )
#' schedule <- schedule_from_data_frame(
#'   activities,
#'   relations,
#'   "Project 2: Patient Transport System",
#'   "VANHOUCKE, Mario. Integrated project management and control:
#'     first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
#' )
#' message(paste("Schedule duration is", schedule$info$duration))
#'
#' new_durations <- c(1,2,5,4,3,2,1,5,3,5,5,3,4,2,1,2,4)
#' schedule <- change_durations(schedule, new_durations)
#' message(paste("Now, schedule duration is", schedule$info$duration))
#'
change_durations <- function(schedule, durations) {
  assert_is_schedule(schedule)

  schedule$activities$duration <- durations
  calculate_critical_path(schedule)
}
