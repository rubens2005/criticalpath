#' Add a relation to a schedule.
#'
#' Add a relation to a schedule.
#' If type is not defined, it is assumed to be FS.
#' If lag is not defined, it is assumed to be zero.
#'
#' @usage
#' add_relation(schedule, from_id, to_id, type, lag)
#'
#' @param schedule Schedule in which the relation will be added.
#'
#' @param from_id The id of predecessor activity.
#'
#' @param to_id The id of successor activity.
#'
#' @param type Specifies the type of relation between activities.
#' Its value may be: FS, FF, SS, SF, that means:
#'
#' FS: Finish-Start relation.
#' Activity to_id can only start after the finish of activity from_id.
#'
#' FF: Finish-Finish relation.
#' Activity to_id must finish together with activity from_id.
#'
#' SS: Start-Start relation.
#' Activity to_id must start together with activity from_id.
#'
#' SF: Start-Finish relation.
#' Activity to_id must finish when activity from_id starts.
#'
#' @param lag The time period between activities that the successor activity
#' must be advanced, or lated, after activity from_id.
#' The must be a integer, less than, equal or greater than zero.
#'
#' @return A Schedule object with a relation added and critical path calculated.
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
#' sch <- add_activity(sch, 5, "E", 4)
#' sch <- add_activity(sch, 6, "F", 3)
#' # Add relations between activities
#' sch <- add_relation(sch, 1, 2)
#' sch <- add_relation(sch, 1, 3)
#' sch <- add_relation(sch, 2, 4)
#' sch <- add_relation(sch, 3, 5)
#' sch <- add_relation(sch, 4, 6)
#' sch <- add_relation(sch, 5, 6)
#' # Get schedule info
#' # What is the project duration?
#' message(paste("Project duration:", sch$info$duration))
#' # Which activities are critical?
#' sch$activities$name[sch$activities$critical]
#'
add_relation <- function(schedule, from_id, to_id, type="FS", lag=0) {
  assert_is_schedule(schedule)

  old_relations <- schedule$relations

  new_relation = data.frame(
    from = from_id,
    to = to_id,
    type = type,
    lag = lag,
    critical = FALSE,
    redudant = FALSE,
    ord = nrow(old_relations) + 1,
    i_from = NA,
    i_to = NA
  )

  schedule$relations <- rbind(old_relations, new_relation)

  schedule$info$nr_relations <- nrow(schedule$relations)
  schedule$info$has_any_relation <- TRUE

  ## Topological organization
  schedule <- topological_organization(schedule)

  ## Critical Path
  calculate_critical_path(schedule)
}
