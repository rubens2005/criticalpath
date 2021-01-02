#' Is an arc redundant
#'
#' Verifify if an arc between two activities, id_from -> id_to, is redundant.
#'
#' @param schedule A Schedule.
#'
#' @param id_from Activity id from.
#'
#' @param id_to Activity id to.
#'
#' @return A logical TRUE if an arc is redundant; FALSE if not.
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
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#' )
#'
#' schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4,8))
#' schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
#' schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
#' schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6,7))
#' schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
#' schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7,11))
#' schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
#' schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
#' schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
#' schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
#' schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
#' schedule <- add_act_rel(schedule, 12, "a12", 0)
#'
#' is_redundant(schedule, 1, 8) # TRUE
#' is_redundant(schedule, 4, 7) # TRUE
#' is_redundant(schedule, 6, 11) # TRUE
#'
is_redundant <- function(schedule, id_from, id_to) {
  assert_is_schedule(schedule)
  assert_activity_id_exist(schedule, id_from)
  assert_activity_id_exist(schedule, id_to)
  assert_relation_exist(schedule, id_from, id_to)

  succ <- all_successors(schedule, id_from, id_to)
  id_to %in% succ
}
