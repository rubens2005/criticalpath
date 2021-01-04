#' List All Predecessors
#'
#' List all predecessors from an activity: direct or indirect predecessors.
#'
#' @usage
#' all_predecessors(schedule, id, ign_from)
#'
#' @param schedule A schedule
#'
#' @param id Activity id to listed
#'
#' @param ign_from An arc to be ignored: ign_from -> id
#'
#' @return A vector
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
#' schedule <- make_empty_schedule(
#'   "Fictitious Project Example",
#'   "VANHOUCKE, Mario. Measuring time:
#'     improving project performance using earned value management.
#'     Gent: Springer, 2009, p. 18"
#' )
#' schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4))
#' schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
#' schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
#' schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6))
#' schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
#' schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7))
#' schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
#' schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
#' schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
#' schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
#' schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
#' schedule <- add_act_rel(schedule, 12, "a12", 0)
#' message("Schedule info:")
#' schedule$info
#' message("Activities info:")
#' schedule$activities
#'
#'all_predecessors(schedule, 6)
#'
all_predecessors <- function(schedule, id, ign_from=NULL) {
  assert_is_schedule(schedule)
  assert_activity_id_exist(schedule, id)
  if(!is.null(ign_from)) {
    assert_activity_id_exist(schedule, ign_from)
    assert_relation_exist(schedule, ign_from, id)
  }

  temp_relation <- schedule$relations
  if(!is.null(ign_from)) {
    u <- which(temp_relation$from == ign_from & temp_relation$to == id)
    temp_relation <- temp_relation[-u, ]
  }

  a_list <- c(id)
  for(i in nrow(temp_relation):1) {
    if(temp_relation$to[i] %in% a_list) {
      a_list <- c(a_list, temp_relation$from[i])
    }
  }
  rev(unique(rev(a_list[-1])))
}
