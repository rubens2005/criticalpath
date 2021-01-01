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
#'all_predecessors(schedule, 5)
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
