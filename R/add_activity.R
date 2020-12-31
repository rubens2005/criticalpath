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

  old_activities <- data.frame(
    id        = schedule$activities$id,
    name      = schedule$activities$name,
    duration  = schedule$activities$duration
  )

  activities =   data.frame(
    id        = id,
    name      = ifelse(is.null(name), base::paste0("act", id), name),
    duration  = ifelse(is.null(duration), 0, duration)
  )
  schedule$activities <- rbind(old_activities, activities)

  schedule$info$nr_activities <- nrow(schedule$activities)
  schedule$info$has_any_activity <- TRUE

  ## Topological organization
  schedule <- topological_organization(schedule)

  ## Critical Path
  calculate_critical_path(schedule)
}
