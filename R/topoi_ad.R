#' AD Activity Distribution Topological Indicator
#'
#' Measures the distribution of the activities over the levels (1, 20).
#'
#' @usage
#' topoi_ad(schedule)
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
#' topoi_ad(schedule)
#'
topoi_ad <- function(schedule) {
  assert_is_schedule(schedule)

  max_level <- schedule$info$max_level
  nr_activities <- schedule$info$nr_activities

  if(nr_activities == 0) {
    return(NA)
  }

  if(max_level == 1 || max_level == nr_activities) {
    return(0);
  }

  # Activities quantities by level: they are called width, width by level
  wi <- table(schedule$activities$progr_level)
  # w mean
  wbar <- nr_activities / max_level
  absolute_mean_deviation <- sum(abs(wi - wbar))

  absolute_mean_deviation / (2 * (max_level - 1) * (wbar - 1));
}
