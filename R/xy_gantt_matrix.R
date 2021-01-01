#' Transform a Gantt matrix in x,y coordinates
#'
#' Transform a Gantt matrix in x,y coordinates.
#' Each point greater than zero in a Gantt matrix becomes a x, y coordinate.
#'
#' @param ganttm A Gantt Matrix.
#'
#' @return A matrix x, y and weight.
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
#' activities <- data.frame(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1,1,3,2, 2,2,2,1, 4,5,3,3, 4,5,1,5,2)
#' )
#' relations <- data.frame(
#'   from = c(1, 2, 3, 3, 4, 5, 6, 7, 8,  8,  8,  8,  8,  9, 10,
#'            11, 12, 13, 13, 14, 14, 15, 15),
#'   to   = c(2, 3, 4, 6, 5, 8, 7, 8, 9, 10, 11, 12, 13, 14, 14,
#'            14, 14, 14, 15, 16, 17, 16, 17)
#' )
#' schedule <- schedule_from_data_frame(
#'   activities,
#'   relations,
#'   "Project 2: Patient Transport System",
#'   "VANHOUCKE, Mario. Integrated project management and control:
#'     first comes the theory, then the practice. Gent: Springer, 2014, p. 9"
#' )
#' gantt <- gantt_matrix(schedule)
#' xygantt <- xy_gantt_matrix(gantt)
#' #plot(xygantt[,1], -xygantt[,2])
#'
xy_gantt_matrix <- function(gantt) {
  assert_is_gantt(gantt)

  qtdatvs <- nrow(gantt)
  pdur <- ncol(gantt)
  v <- as.numeric(t(gantt))
  ii <- which(v > 0) - 1
  y <- floor(ii / pdur)
  x <- ii - y * pdur
  peso <- v[ii + 1]
  matrix(c(x, y + 1, peso), ncol=3)
}
