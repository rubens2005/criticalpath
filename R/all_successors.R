#' List All Successors
#'
#' List all successors from an activity: direct or indirect successors.
#'
#' @usage
#' all_successors(schedule, id, ign_to)
#'
#' @param schedule A schedule
#'
#' @param id Activity id to listed
#'
#' @param ign_to An arc to be ignored: id -> ign_to
#'
#' @return A vector
#'
#' @references DOCUMENTAÇÃO SOBRE O MÉTODO: livros, artigos ou sites.
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
all_successors <- function(schedule, id, ign_to=NULL) {
  assert_is_schedule(schedule)
  assert_activity_id_exist(schedule, id)
  if(!is.null(ign_to)) {
    assert_activity_id_exist(schedule, ign_to)
    assert_relation_exist(schedule, id, ign_to)
  }

  temp_relation <- schedule$relations
  if(!is.null(ign_to)) {
    u <- which(temp_relation$from == id & temp_relation$to == ign_to)
    temp_relation <- temp_relation[-u, ]
  }

  a_list <- c(id)
  for(i in 1:nrow(temp_relation)) {
    if(temp_relation$from[i] %in% a_list) {
      a_list <- c(a_list, temp_relation$to[i])
    }
  }
  rev(unique(rev(a_list[-1])))
}
