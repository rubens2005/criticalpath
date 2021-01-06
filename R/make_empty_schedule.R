#' Make an empty schedule
#'
#' Make a schedule with no activities and no relations.
#' The activities and relations will be add in sequence.
#'
#' @usage
#' make_empty_schedule(title="", reference="")
#'
#' @param title A project title for identification.
#'
#' @param reference A reference from project origin,
#' for example, a book, a paper, a corporation, or nothing.
#'
#' @return A Schedule object with no activities and no relations.
#'
#' @export
#'
#' @examples
#' schedule <- make_empty_schedule("P1", "From criticalpath package")
make_empty_schedule <- function(title="", reference="") {
  Schedule$new(title, reference)
}
