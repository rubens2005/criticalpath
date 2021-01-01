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
#' is_redundant(schedule, 1, 8)
#'
is_redundant <- function(schedule, id_from, id_to) {
  assert_is_schedule(schedule)
  assert_activity_id_exist(schedule, id_from)
  assert_activity_id_exist(schedule, id_to)
  assert_relation_exist(schedule, id_from, id_to)

  succ <- all_successors(schedule, id_from, id_to)
  id_to %in% succ
}
