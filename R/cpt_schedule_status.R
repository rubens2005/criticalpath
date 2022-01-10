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




#' @noRd
cpt_is_schedule_empty <- function(sch) {
  # Validation: Schedule object is valid
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  return(sch$info$status == "EMPTY")
}


#' @noRd
cpt_is_schedule_with_some_activity <- function(sch) {
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  return(sch$info$status == "SOME_ACTIVITY")
}


#' @noRd
cpt_is_schedule_created <- function(sch) {
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }

  return(sch$info$status == "CREATED")
}
