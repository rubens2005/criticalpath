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
#' @references
#'
#' Csardi, G. & Nepusz, T. (2005).
#' The Igraph Software Package for Complex Network Research.
#' *InterJournal*. Complex Systems. 1695.
#' [Article](https://www.researchgate.net/publication/221995787_The_Igraph_Software_Package_for_Complex_Network_Research)
#'  / [igraph](https://igraph.org/)
#'
#' Vanhoucke, M. (2009) *Measuring Time*:
#' Improving Project Performance Using Earned Value Management.
#' Springer-Verlag US.
#' doi: [10.1007/978-1-4419-1014-1](https://doi.org/10.1007/978-1-4419-1014-1).
#'
#' Vanhoucke, M. (2013) *Project Management with Dynamic Scheduling*:
#' Baseline Scheduling, Risk Analysis and Project Control.
#' Springer-Verlag Berlin Heidelberg.
#' doi: [10.1007/978-3-642-40438-2](https://doi.org/10.1007/978-3-642-40438-2)
#'
#' Vanhoucke, M. (2014) *Integrated Project Management and Control*:
#' First Comes the Theory, then the Practice.
#' Springer International Publishing Switzerland.
#' doi: [10.1007/978-3-319-04331-9](https://doi.org/10.1007/978-3-319-04331-9)
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
#' # Create an empty schedule
#' sch1 <- make_empty_schedule("P1", "From criticalpath package")
#' # Add activities to it
#' sch1 <- add_activity(sch1, 1, "A", 5)
#' sch1 <- add_activity(sch1, 2, "B", 4)
#' sch1 <- add_activity(sch1, 3, "C", 2)
#' sch1 <- add_activity(sch1, 4, "D", 3)
#' sch1 <- add_activity(sch1, 5, "E", 4)
#' sch1 <- add_activity(sch1, 6, "F", 3)
#' # Add relations between activities
#' sch1 <- add_relation(sch1, 1, 2)
#' sch1 <- add_relation(sch1, 1, 3)
#' sch1 <- add_relation(sch1, 2, 4)
#' sch1 <- add_relation(sch1, 3, 5)
#' sch1 <- add_relation(sch1, 4, 6)
#' sch1 <- add_relation(sch1, 5, 6)
#' # Get schedule info
#' # What is the project duration?
#' message(paste("Project duration:", sch1$info$duration))
#' # Which activities are critical?
#' sch1$activities$name[sch1$activities$critical]
#'
#' # Create an empty schedule
#' sch2 <- make_empty_schedule("P2", "From criticalpath package")
#' # Add activities to it and relations between activities in the same time
#' sch2 <- add_act_rel(sch2, 1, "A", 5, c(2,3))
#' sch2 <- add_act_rel(sch2, 2, "B", 4, c(4))
#' sch2 <- add_act_rel(sch2, 3, "C", 2, c(5))
#' sch2 <- add_act_rel(sch2, 4, "D", 3, c(6))
#' sch2 <- add_act_rel(sch2, 5, "E", 4, c(6))
#' sch2 <- add_act_rel(sch2, 6, "F", 3)
#' # What is the critical path?
#' sch2$relations[sch2$relations$critical, ]
#' # Critical activities from projects P1 and P2 are the same.
#' all.equal(sch1$activities$critical, sch2$activities$critical)
#'
make_empty_schedule <- function(title="", reference="") {
  schedule <- list(

    activities = data.frame(
      id = integer(),
      name = character(),
      duration = numeric(),
      milestone = logical(),
      critical = logical(),
      ES = numeric(),
      EF = numeric(),
      LS = numeric(),
      LF = numeric(),
      total_float = numeric(),
      free_float = numeric()
    ),

    relations = data.frame(
      from = numeric(),
      to   = numeric(),
      type = character(),
      lag = numeric(),
      critical = logical(),
      ord = numeric(),
      i_from = numeric(),
      i_to = numeric()
    ),

    info = list(
      title = title,
      reference = reference,

      nr_activities = 0,
      has_any_activity = FALSE,

      nr_relations = 0,
      has_any_relation = FALSE,

      duration = 0
    ),

    config = list()
  )
  class(schedule) <- "Schedule"
  schedule
}
