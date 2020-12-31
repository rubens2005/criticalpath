#' Gantt Matrix
#'
#' Create a matrix that representa a Gantt chart.
#'
#' @param schedule
#' Schedule to crate Gantt mattix.
#'
#' @return Uma matriz, onde 1 indicada que a atividade esta em execucao
#'    Nessa matriz as linhas representam as atividades
#'    As colunas representam o periodo de execucao da atividade.
#' @return A matrix, where row represent activity and columns representa time period.
#' A cell represents a activity in execution in period:  1 indicate that a activity is in execution.
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
#' gantt <- gantt_matrix(schedule)
#' effort_by_period <- colSums(gantt)
#' s_curve <- cumsum(effort_by_period)
#' #plot(s_curve, type="l", lwd=3)
#'
gantt_matrix <- function(schedule) {
  atvs <- schedule$activities
  duration <- schedule$info$duration
  qtdatvs <- nrow(atvs)
  ganttm <- matrix(rep(0, duration * qtdatvs), nrow=qtdatvs)
  for(i in 1:qtdatvs) {
    inicio <- atvs$ES[i] + 1
    termino <- atvs$EF[i]
    ganttm[i, inicio:termino] <- 1
  }

  ganttm
}
