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
