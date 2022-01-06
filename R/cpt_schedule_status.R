#Status:

## EMPTY: vazio
###### Transition: included activity ===> SOME_ACTIVITY
###### Transition: included relation ===> EMPTY

## SOME_ACTIVITY: with some activity
###### Transition: included activity ===> SOME_ACTIVITY
###### Transition: included relation ===> SOME_ACTIVITY

## CREATED: Schedule created, with CPM calculus
# Transition to this status: `create_schedule()`
###### Transition: included activity ===> SOME_ACTIVITY
###### Transition: included relation ===> SOME_ACTIVITY




#' Is the schedule empty?
#'
#' @param sch A schedule of class `KKKKKKKK`
#'
#' @return A logical value.
#'
#' @examples
#'
#' @export
cpt_is_schedule_empty <- function(sch) {
  # Get the name of the function

  # Validation: Schedule object is valid
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  # Determine if schedule graph is empty directed by getting the
  # value at `sch$info$status == EMPTY`
  return(sch$info$status == "EMPTY")
}


#' Is schedule with some activity?
#'
#' @param sch A schedule of class `KKKKKKKK`
#'
#' @return A logical value.
#'
#' @examples
#'
#' @export
cpt_is_schedule_with_some_activity <- function(sch) {
  # Validation: Schedule object is valid
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  return(sch$info$status == "SOME_ACTIVITY")
}


#' Is the schedule created?
#'
#' @param sch A schedule of class `KKKKKKKK`
#'
#' @return A logical value.
#'
#' @examples
#'
#' @export
cpt_is_schedule_created <- function(sch) {
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  return(sch$info$status == "CREATED")
}
