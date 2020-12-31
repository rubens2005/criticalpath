#' Create a schedule from data frame
#'
#' Create a schedule from data frames and apply Critical Path Method (CPM)
#'
#' @usage
#' schedule_from_data_frame(activities, relations, title, reference)
#'
#' @param activities data frame with activities.
#'
#' @param relations data frame with precedence relations between activities.
#'
#' @param title A project title for identification.
#'
#' @param reference A reference from project origin,
#' for example, a book, a paper, a corporation, or nothing.
#'
#'
#' @return A Schedule object with an activity added and critical path calculated.
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
#'   duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
#' )
#' relations <- data.frame(
#'   from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,  7,  8,  9,
#'            10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
#'   to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11, 12, 13, 14,
#'            15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
#' )
#' schedule <- schedule_from_data_frame(
#'   activities,
#'   relations,
#'   "Project 1: Cost Information System",
#'   "VANHOUCKE, Mario. Integrated project management and control:
#'     first comes the theory, then the practice. Gent: Springer, 2014, p. 6"
#' )
#'
schedule_from_data_frame <- function(activities, relations=NULL, title="", reference="") {

  schedule <- make_empty_schedule(title, reference)

  for(i in 1:nrow(activities)) {
    schedule = add_activity(
      schedule,
      activities$id[i],
      activities$name[i],
      activities$duration[i]
    )
  }

  if(!is.null(relations) && nrow(relations) > 0) {
    if(is.null(relations$type)) {
      relations$type <- "FS"
    }
    if(is.null(relations$lag)) {
      relations$lag <- 0
    }
    relations$critical <- FALSE
    relations$redundant <- FALSE
    for(i in 1:nrow(relations)) {
      schedule = add_relation(
        schedule,
        relations$from[i],
        relations$to[i],
        relations$type[i],
        relations$lag[i]
      )
    }
  }

  schedule
}
