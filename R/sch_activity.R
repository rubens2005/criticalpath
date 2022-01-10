#' Add Activities
#'
#' Combine several vectors for activities and their attributes into a tibble,
#' which can be combined with other similarly-generated tibbles, resulting
#' in unique tibble to be added in a schedule. If the schedule already
#' contain some activities, the new activities will be added in the end.
#'
#' A activity tibble, or atb, has at least the following columns:
#'
#' - `id` (of type `integer`): Activity id.
#' It is an integer number that must be unique within a schedule.
#'
#' - `name` (of type `character`): Activity name. It may be empty string.
#'
#' - `duration` (of type `integer`): Activity duration.
#' It is integer number without unit time. It may be zero.
#'
#' An arbitrary number of additional columns containing data attributes
#' can be part of the atb, so long as they follow the aforementioned columns.
#'
#' @param sch A schedule object.
#' @param id Activity id that will be used to make
#' relation between activities. It must be unique.
#' @param name The name of activity.
#' @param duration A number that represents the activity's duration.
#' It must be equal or greater than zero.
#' @param ... One or more vectors for associated activity attributes.
#'
#' @return A schedule with a activity tibble (atb) added.
#'
#' @seealso [sch_reference()], [sch_add_relations()], [sch_add_activity()],
#' [sch_title()], [sch_nr_activities()], [sch_new()], [sch_plan()],
#' [sch_get_activity()], [sch_has_any_activity()],
#' [sch_change_activities_duration()].
#'
#' @examples
#' # Example #1
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch)
#' sch_activities(sch)
#'
#' # Example #2
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L),
#'     resource  = "Rubens",
#'     cost      = 123.45
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch)
#' atb <- sch_activities(sch)
#' atb$resource
#' atb$cost
#'
#' # Example #3
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L),
#'     resource  = c(
#'       "Rubens", "Jose", "Rosa", "Rodrigues", "Silva",
#'       "Rubens", "Jose", "Rosa", "Rodrigues", "Silva",
#'       "Rubens", "Jose", "Rosa", "Rodrigues", "Silva",
#'       "Rubens", "Jose"),
#'     cost      = c(
#'       123.45, 234.56, 345.56, 456.78, 567.89,
#'       123.45, 234.56, 345.56, 456.78, 567.89,
#'       123.45, 234.56, 345.56, 456.78, 567.89,
#'       123.45, 234.56)
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch)
#' atb <- sch_activities(sch)
#' atb$resource
#' atb$cost
#'
#' @export
sch_add_activities <- function(sch, id, name, duration, ...) {
  cpt_assert_schedule_object_valid(sch)

  # Validation: vector id must be an integer.
  if (!(inherits(id, "integer"))) {
    stop("The value supplied to `id` must be integer!")
  }
  nr_activities <- length(id)
  if (nr_activities == 0) {
    stop("There is no activity to be included!")
  }

  # Validation: vector duration must be an integer.
  if (!(inherits(duration, "integer"))) {
    stop("The value supplied to `duration` must be integer!")
  }

  # Name validation / treatment
  if (is.null(name)) {
    name <- as.character(id)
  } else if(length(name) != nr_activities) {
    stop("The length of `name` must be equal `id`'s length!")
  }

  # Duration validation
  if(length(duration) == 1L) {
    duration %<>% rep(nr_activities)
  } else if(length(duration) != nr_activities) {
    stop("The length of `duration` must be equal `id`'s length!")
  }

  activities_tib <- tibble::tibble(
    id = id,
    name = name,
    duration = duration,
    milestone = FALSE,
    critical = FALSE,
    early_start = NA_integer_,
    early_finish = NA_integer_,
    late_start = NA_integer_,
    late_finish = NA_integer_,
    total_float = NA_integer_,
    free_float = NA_integer_
  )

  extras <- list(...)
  extras <- cpt_collect_extra_vectors(nr_activities, extras)
  if (inherits(extras, "tbl_df")) {
    activities_tib %<>% dplyr::bind_cols(extras)
  }

  if(sch$info$nr_activities > 0) {
     activities_tib <- dplyr::bind_rows(sch$activities, activities_tib)
  }

  sch$info$status <- "SOME_ACTIVITY"
  sch$activities <- activities_tib
  sch$info$nr_activities <- nrow(activities_tib)
  sch$info$has_any_activity = TRUE

  return(sch)
}

#' Add Activity
#'
#' Add an activity and her relations to a schedule. The relations are optional.
#' They will be included if a set of activity id is informed,
#' after activity duration.
#'
#' @param sch A schedule object.
#' @param id Activity id that will be used to make
#' relation between activities. It must be unique.
#' @param name The name of activity.
#' @param duration A number that represents the activity's duration.
#' It must be equal or greater than zero.
#' @param ... A set of activity id relation such that will be linked with
#' activity id. It may be relations of successor or predecessors.
#' @param direction Direction of relations: It may be "succ" or "pred".
#' - `succ`: The relations_id will be the successor of the activity.
#' - `pred`: The relations_id will be the predecessor of the activity.
#'
#' @return A Schedule object with an activity added to it.
#' If relations id is present, it will be included to the schedule.
#'
#' @seealso [sch_change_activities_duration()], [sch_has_any_activity()],
#' [sch_new()], [sch_add_activities()], [sch_get_activity()], [sch_plan()],
#' [sch_nr_activities()], [sch_add_relation()].
#'
#' @examples
#' # Example #1: Only with activities
#' sch <- sch_new() %>%
#'   sch_add_activity(1L, "Task 1", 5L) %>%
#'   sch_add_activity(2L, "Task 2", 6L) %>%
#'   sch_add_activity(3L, "Task 3", 8L) %>%
#'   sch_add_activity(4L, "Task 4", 6L) %>%
#'   sch_add_activity(5L, "Task 5", 9L) %>%
#'   sch_add_activity(6L, "Task 6", 3L) %>%
#'   sch_add_activity(7L, "Task 7", 4L) %>%
#'   sch_plan()
#' sch_duration(sch)
#' sch_activities(sch)
#'
#' # Example #2: With activities and relations.
#' sch <- sch_new() %>%
#'   sch_add_activity(1L, "Task 1", 5L, 2L, 3L) %>%
#'   sch_add_activity(2L, "Task 2", 6L, 4L) %>%
#'   sch_add_activity(3L, "Task 3", 8L, 5L) %>%
#'   sch_add_activity(4L, "Task 4", 6L, 6L) %>%
#'   sch_add_activity(5L, "Task 5", 9L, 6L) %>%
#'   sch_add_activity(6L, "Task 6", 3L, 7L) %>%
#'   sch_add_activity(7L, "Task 7", 4L) %>%
#'   sch_plan()
#' sch_duration(sch)
#' sch_activities(sch)
#' sch_relations(sch)
#'
#'
#' @export
sch_add_activity <- function(sch, id, name, duration, ..., direction = "succ") {
  relations_id <- list(...)
  n <- length(relations_id)
  sch %<>%
    sch_add_activities(
      id        = id,
      name      = name,
      duration  = duration
    )

  if(n == 0) {
    return(sch)
  }

  if(direction != "succ" && direction != "pred") {
    msg <- base::paste(
      "Invalid '", direction, "' direction!",
      "It must be 'succ' or 'pred'!"
    )
    stop(msg)
  }

  relations_id %<>%
    unlist() %>%
    as.integer()

  if(direction == "succ") {
    return(
      sch %>% sch_add_relations(
        from = id,
        to   = relations_id
      )
    )
  }

  return(
    sch %>% sch_add_relations(
      from = relations_id,
      to   = id
    )
  )
}

#' Get Activity
#'
#' Gets an activity by id.
#'
#' @param sch A schedule object.
#' @param aid An activity id as defined by the user.
#'
#' @return A an activity information in a tibble with one line,
#' or an error if activity id doesn't exist.
#'
#' @seealso [sch_activities()], [sch_duration()], [sch_nr_activities()],
#' [sch_add_activities()], [sch_critical_activities()], [sch_has_any_activity()],
#' [sch_change_activities_duration()], [sch_add_activity()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
#'   ) %>%
#'   sch_plan()
#' sch_get_activity(sch, 7)
#'
#' @export
sch_get_activity <- function(sch, aid) {
  id <- NULL
  act <- sch_activities(sch) %>%
    dplyr::filter(id == aid)
  if(nrow(act) == 1) {
    return(act)
  }
  stop("Activity 'aid' must exist em activities list!")
}


#' Change Activities Duration
#'
#' Change activities duration and calculates critical path.
#' This way is faster than creating a new schedule with new durations.
#' The order of duration is the insertion order of activities.
#'
#' @param sch A schedule object.
#' @param new_durations A vector with new activities' duration.
#'
#' @return A schedule object with new durations.
#'
#' @seealso [sch_activities()], [sch_has_any_activity()], [sch_duration()],
#' [sch_nr_activities()], [sch_add_activity()], [sch_add_activities()],
#' [sch_get_activity()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 2: Patient Transport System") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 9") %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,1L,3L,2L, 2L,2L,2L,1L, 4L,5L,3L,3L, 4L,5L,1L,5L,2L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 3L, 3L, 4L, 5L, 6L, 7L, 8L,  8L,  8L,
#'              8L,  8L,  9L, 10L, 11L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
#'     to   = c(2L, 3L, 4L, 6L, 5L, 8L, 7L, 8L, 9L, 10L, 11L,
#'              12L, 13L, 14L, 14L, 14L, 14L, 14L, 15L, 16L, 17L, 16L, 17L)
#'   ) %>%
#'   sch_plan()
#'
#' # Project duration
#' sch_duration(sch) # 25
#' # Activities duration
#' atb <- sch_activities(sch)
#' atb$duration
#'
#' # Now, change activities duration
#' new_durations <- c(1L,2L,5L, 4L,3L, 2L,1L, 5L, 3L,5L,5L,3L,4L, 2L,1L, 2L,4L)
#' sch %<>%
#'   sch_change_activities_duration(new_durations)
#'
#' #Project duration
#' sch_duration(sch) # 31
#' # Activities duration
#' atb <- sch_activities(sch)
#' atb$duration
#'
#' @export
sch_change_activities_duration <- function(sch, new_durations) {
  cpt_assert_schedule_object_valid(sch)

  #    => Verificar se está no STATUS CREATED
  if(!cpt_is_schedule_created(sch)) {
    stop("To change duration, schedule must be created!")
  }

  # Validation: vector duration must be an integer.
  if (!(inherits(new_durations, "integer"))) {
    stop("The value supplied to `new_durations` must be integer!")
  }

  # Duration validation
  if(length(new_durations) != sch_nr_activities(sch)) {
    stop("The length of `new_durations` must be equal to `nr_activities`!")
  }

  if(any(is.null(new_durations) | is.na(new_durations))) {
    stop("Error: could not contain NULL or NA duration!")
  }

  sch$activities$duration <- new_durations
  return(cpt_calculate_critical_path(sch))
}


#' Has Any Activity
#'
#' A logical value that indicates if the schedule has any activity.
#' A TRUE value means that the schedule has any activity;
#' a FALSE, means that the schedule do not have any activity.
#'
#' @param sch A schedule object.
#'
#' @return A logical value:
#' - TRUE: The schedule has any activity;
#' - FALSE: The schedule do not have any activity.
#'
#' @seealso [sch_nr_activities()], [sch_critical_activities()],
#' [sch_add_activities()], [sch_change_activities_duration()],
#' [sch_activities()], [sch_nr_relations()], [sch_has_any_relation()],
#' [sch_add_activity()].
#'
#' @examples
#' sch <- sch_new()
#' sch_has_any_activity(sch) # FALSE
#'
#' sch <- sch_new() %>%
#'   sch_add_activity(1L, "Only one", 0L) %>%
#'   sch_plan()
#' sch_has_any_activity(sch) # TRUE
#'
#' @export
sch_has_any_activity <- function(sch) {
  return(sch$info$has_any_activity)
}

#' Nr. of Activities
#'
#' Number of activities in a schedule as an integer value.
#'
#' @param sch A schedule object.
#'
#' @return A integer value indicating the number of activities.
#'
#' @seealso [sch_add_activity()], [sch_nr_relations()], [sch_add_activities()],
#' [sch_activities()], [sch_change_activities_duration()],
#' [sch_critical_activities()], [sch_get_activity()], [sch_has_any_relation()].
#'
#' @examples
#' sch <- sch_new()
#' sch_nr_activities(sch) # 0
#'
#' sch <- sch_new() %>%
#'   sch_add_activity(1L, "Only one", 0L) %>%
#'   sch_plan()
#' sch_nr_activities(sch) # 1
#'
#' @export
sch_nr_activities <- function(sch) {
  return(sch$info$nr_activities)
}

#' Activities
#'
#' Return a tibble with all activities of a schedule in an insertion order.
#' These are the main information calculated by CPM.
#'
#' The tibble is formed by following structure:
#'    - **id:** Activity id.
#'    - **name:** The name of activity.
#'    - **duration:** A number that represents the activity's duration.
#'    - **milestone:** A milestone is an activity with zero duration.
#'    This property indicates if an activity is a milestone or not:
#'    \code{TRUE} indicates it is a milestone; \code{FALSE} indicates it is not.
#'    - **critical:** A critical activity is one with total float minor or equal
#'    to zero. This property indicates if an activity is critical:
#'    \code{TRUE} indicates it is critical;
#'    \code{FALSE} indicates it is not critical.
#'    - **early_start:** Is the earliest start period an activity can begin
#'    after its predecessors without violating precedence relation.
#'    - **early_finish:** Is the early start plus activity duration.
#'    - **late_start:** Is the late finish minus activity duration.
#'    - **late_finish:** Is the latest finish an activity can finish
#'    before their successors without violating precedence relation.
#'    - **total_float:** It is the amount of period an activity can be
#'    delayed without violating the project duration. Its formula is:
#'    late_start - early_start or late_finish - early_finish
#'    - **free_float:** It is the amount of period an activity can be
#'    delayed without violating the start time of the successors activities.
#'    - **progr_level:** It is the rank of activities counted from begin.
#'    The level of the activities that don't have predecessor is one;
#'    the level of the other activities, is one plus the maximal level of
#'    their predecessor.
#'    - **regr_level:** Regressive level is the rank of activities counted
#'    from the end. The level of the activities that don't have successor is the
#'    maximal progressive level; the level of the other activities,
#'    is one minus the minimal level of their successor.
#'    - **topo_float:** It is the difference between progressive level
#'     and regressive level.
#'
#' @param sch A schedule object.
#'
#' @return A tibble with activities.
#'
#' @seealso [sch_has_any_activity()], [sch_change_activities_duration()],
#' [sch_add_activity()], [sch_nr_activities()], [sch_critical_activities()],
#' [sch_add_activities()], [sch_get_activity()], [sch_duration()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8,11) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_activities(sch)
#'
#' @export
sch_activities <- function(sch) {
  return(sch$activities)
}

#' Critical Activities
#'
#' Return a tibble with all critical activities of a schedule in an insertion order.
#'
#' @param sch A schedule object.
#'
#' @return A tibble with critical activities.
#'
#' @seealso [sch_get_activity()], [sch_add_activities()], [sch_activities()],
#' [sch_add_activity()], [sch_nr_activities()], [sch_non_critical_activities()],
#' [sch_has_any_activity()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8,11) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_critical_activities(sch)
#'
#' @export
sch_critical_activities <- function(sch) {
  critical <- NULL
  return(
    sch %>%
      sch_activities() %>%
      dplyr::filter(critical)
  )
}

#' Non Critical Activities
#'
#' Return a tibble with all non critical activities of a schedule
#' in an insertion order.
#'
#' @param sch A schedule object.
#'
#' @return A tibble with non critical activities.
#'
#' @seealso [sch_get_activity()], [sch_add_activity()], [sch_activities()],
#' [sch_critical_activities()], [sch_has_any_activity()], [sch_nr_activities()],
#' [sch_add_activities()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity(  1L, "a1" , 0L, 2,3,4) %>%
#'   sch_add_activity(  2L, "a2" , 4L, 5) %>%
#'   sch_add_activity(  3L, "a3" , 9L, 10) %>%
#'   sch_add_activity(  4L, "a4" , 1L, 6) %>%
#'   sch_add_activity(  5L, "a5" , 4L, 9) %>%
#'   sch_add_activity(  6L, "a6" , 5L, 7) %>%
#'   sch_add_activity(  7L, "a7" , 1L, 8,11) %>%
#'   sch_add_activity(  8L, "a8" , 7L, 12) %>%
#'   sch_add_activity(  9L, "a9" , 8L, 12) %>%
#'   sch_add_activity( 10L, "a10", 3L, 12) %>%
#'   sch_add_activity( 11L, "a11", 3L, 12) %>%
#'   sch_add_activity( 12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_non_critical_activities(sch)
#'
#' @export
sch_non_critical_activities <- function(sch) {
  critical <- NULL
  return(
    sch %>%
      sch_activities() %>%
      dplyr::filter(!critical)
  )
}
