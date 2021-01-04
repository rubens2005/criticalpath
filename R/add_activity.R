#' Add an activity to a schedule
#'
#' Add an activity to a schedule that must exist.
#'
#' @usage
#' add_activity(schedule, id, name="", duration=0)
#'
#' @param schedule
#' Schedule in which the activity will be added.
#'
#' @param id
#' Activity id. The id will be used to make relation between activities.
#'
#' @param name
#' The name of activity. The default is a empty string.
#'
#' @param duration
#' A number that represents the activity's duration.
#' It must be equal or grater than zero. The default value is zero.
#'
#' @return A Schedule object with an activity added and critical path calculated.
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
#' sch <- make_empty_schedule("A Project", "From criticalpath package")
#' # Add activities to it
#' sch <- add_activity(sch, 1, "A", 5)
#' sch <- add_activity(sch, 2, "B", 4)
#' sch <- add_activity(sch, 3, "C", 2)
#' sch <- add_activity(sch, 4, "D", 3)
#' sch <- add_activity(sch, 5, "E", 5)
#' sch <- add_activity(sch, 6, "F", 3)
#' # Get schedule info
#' # What is the project duration?
#' message(paste("Project duration:", sch$info$duration))
#' # Which activities are critical?
#' sch$activities$name[sch$activities$critical]
#'
#'
 add_activity <- function(schedule, id, name="", duration=0) {
   assert_is_schedule(schedule)
   assert_activity_id_is_valid(id)
   assert_activity_id_does_not_exist(schedule, id)

   old_activities <- schedule$activities


  new_activity <- data.frame(
    id        = id,
    name      = ifelse(is.null(name), base::paste0("act", id), name),
    duration  = ifelse(is.null(duration), 0, duration),
    milestone = FALSE,
    critical = FALSE,
    ES =  -Inf,
    EF =  -Inf,
    LS =  +Inf,
    LF =  +Inf,
    total_float = 0,
    free_float = 0,
    progr_level = -Inf,
    regr_level = +Inf,
    topo_float = 0
  )

  schedule$activities <- rbind(old_activities, new_activity)

  schedule$info$nr_activities <- nrow(schedule$activities)
  schedule$info$has_any_activity <- TRUE

  ## Topological organization
  schedule <- topological_organization(schedule)

  ## Critical Path
  calculate_critical_path(schedule)
}
