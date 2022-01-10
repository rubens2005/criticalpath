###
# Schedule validation functions
###

#' Check whether a schedule object is valid
#'
#' From `DiagrammeR`
#'
#' @noRd
cpt_schedule_object_valid <- function(sch) {
  # Check for all component names to be present
  properties <- c("activities", "relations", "info", "config")
  if (!all(properties %in% names(sch))) {
    return(FALSE)
  }

  # Check for specific schedule classes
  class_vec <- c(
    !tibble::is_tibble(sch$activities),
    !tibble::is_tibble(sch$relations)
  )
  if(any(class_vec)) {
    return(FALSE)
  }

  return(TRUE)
}

#' Assert schedule object valid
#'
#' If is not valid, emit a stop and emit an error message.
#'
#' @noRd
cpt_assert_schedule_object_valid <- function(sch) {
  if(!cpt_schedule_object_valid(sch)) {
    stop("The schedule object is not valid!")
  }
}

#' Convert a number vector to a integer vector
#'
#' If there any NA or NULL, and `na_nul_to_zero == TRUE`,
#' convert them to ZERO.
#'
#' The vector values are truncated to the whole number.
#'
#' @noRd
cpt_convert_to_integer <- function(v, na_to_zero = TRUE) {
  if(is.null(v)) {
    stop("Vector 'v' cannot be NULL!")
  }
  if(length(v) == 0L) {
    stop("Vector 'v' cannot be empty!")
  }

  v <- as.integer(v)
  if(na_to_zero) {
    v[is.na(v)] <- 0L
  }
  return(v)
}

cpt_is_a_valid_integer <- function(i) {

}

#' Is a valid tibble?
#'
#' Verify if a table is a tibble.
#'
#' @noRd
cpt_is_valid_tibble <- function(obj) {
  if(base::any(base::is.null(obj))) {
    return(FALSE)
  }
  if(base::any(base::is.na(obj))) {
    return(FALSE)
  }
  if(!tibble::is_tibble(obj)) {
    return(FALSE)
  }

  return(TRUE)
}


#' Activity id exist?
#'
#' Verify if exist an activity with id.
#'
#' @noRd
sch_activity_id_exist <- function(sch, activity_id) {
  id <- NULL
  n <- sch$activities %>%
    dplyr::filter(id == activity_id) %>%
    dplyr::count()
  return(n == 1L)
}

#' assert_activity_id_exist
#'
#' @noRd
cpt_assert_activity_id_exist <- function(sch, activity_id) {
  if(!sch_activity_id_exist(sch, activity_id)){
    stop("Activity id must exist em activities list!")
  }
}

#' assert_activity_id_does_not_exist
#'
#' @noRd
cpt_assert_activity_id_does_not_exist <- function(sch, activity_id) {
  if(sch_activity_id_exist(sch, activity_id)){
    stop("Activity id must NOT EXIST em activities list!")
  }
}

cpt_assert_activity_id_is_valid <- function(activity_id) {
  if(any(base::is.null(activity_id))) {
    stop("Activity id cannot be NULL!")
  }
  if(any(base::is.na(activity_id))) {
    stop("Activity id cannot be NA!")
  }
  if(!is.integer(activity_id)) {
    stop("Activity id must be an integer!")
  }
}

#########

cpt_collect_extra_vectors <- function(nr_objects, extras) {
  n <- length(extras)
  if (n == 0) {
    return(NULL)
  }

  for (i in 1:n) {

    # Expand vectors with single values to fill to
    # the number of activities
    if (length(extras[[i]]) == 1) {
      extras[[i]] <- rep(extras[[i]], nr_objects)
    }

    # Expand vectors with `length` > `1` and
    # `length` < `length(nodes)`
    if (length(extras[[i]]) > 1 & length(extras[[i]]) < nr_objects) {
      extras[[i]] <- c(extras[[i]], rep("", (nr_objects - length(extras[[i]]))))
    }

    # Trim vectors with number of values exceeding
    # the number of nodes
    if (length(extras[[i]]) > nr_objects) {
      extras[[i]] <- extras[[i]][1:nr_objects]
    }
  }

  # Create a tibble from the `extras` list
  return(tibble::as_tibble(extras))
}

#' @noRd
cpt_assert_relation_exist <- function(sch, id_from, id_to) {
  rtb <- sch %>% sch_relations()
  u <- rtb$from == id_from
  v <- rtb$to == id_to
  if(sum(u & v) == 0L) {
    stop(paste("Relation", id_from, "->", id_to, " must exist!"))
  }
}

#' @noRd
assert_relation_does_not_exist <- function(sch, from, to) {
  from_exist <- sch$relations$from == from
  to_exist <- sch$relations$to == to
  u <- which(from_exist & to_exist)
  if(length(u) > 0L){
    stop(paste("Relations", from, "->", to, " must NOT exist!"))
  }
}

#' Verify is a gantt object is of Gantt matrix class
#' @noRd
cpt_is_gantt <- function (gantt)  {
  return("Gantt" %in% class(gantt))
}

#' @noRd
cpt_assert_is_gantt <- function(gantt) {
  if (!cpt_is_gantt(gantt)) {
    stop("Not a Gantt matrix object!")
  }
}
