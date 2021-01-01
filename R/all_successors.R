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
#'all_successors(schedule, 1)
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
