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
#' @return A Schedule object with an activities and relations.
#'
#' @export
schedule_from_data_frame <- function(activities, relations=NULL, title="", reference="") {

  schedule <- Schedule$new(title, reference)

  for(i in 1:base::nrow(activities)) {
    schedule$add_activity(
      activities$id[i],
      activities$name[i],
      activities$duration[i]
    )
  }

  if(!base::is.null(relations) && base::nrow(relations) > 0) {
    if(base::is.null(relations$type)) {
      relations$type <- "FS"
    }
    if(base::is.null(relations$lag)) {
      relations$lag <- 0
    }
    relations$critical <- FALSE
    relations$redundant <- FALSE
    for(i in 1:base::nrow(relations)) {
      schedule$add_relation(
        relations$from[i],
        relations$to[i],
        relations$type[i],
        relations$lag[i]
      )
    }
  }

  schedule
}
