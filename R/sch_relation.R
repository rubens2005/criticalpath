#' Add Relations
#'
#' Combine several vectors for relation and their attributes into a tibble
#' and add relations between activities to a schedule.
#'
#' An relation tibble, or rtb, has at least the following columns:
#'
#' - `from` (of type `integer`): The id of predecessor activity.
#' Must exist an activity with `from` id.
#' - `to` (of type `integer`): The id of successor activity.
#' Must exist an activity with `to` id.
#' - `type` (of type `character`)
#' Specifies the relation type between activities.
#' The default type is FS and its value may be: FS, FF, SS, SF, that means:
#'     - **FS:** Finish-Start relation.
#' Activity 'to' id can only start after the finish of activity 'from' id.
#'     - **FF:** Finish-Finish relation.
#' Activity 'to' id must finish together with activity 'from' id.
#'     - **SS:** Start-Start relation.
#' Activity 'to' id must start together with activity 'from' id.
#'     - **SF:** Start-Finish relation.
#' Activity 'to' id must finish when activity 'from' id starts.
#' - `lag` (of type `integer`): The time period between activities
#'  that the successor activity `to` must be advanced after activity `from`
#'  has been finished.
#'  The value may be negative, in such case, the activity 'to' will be
#'  anticipated 'lag' time periods.
#' It must be an integer, less than, equal or greater than zero.
#' If lag is not defined, it is assumed to be zero.
#'
#' An arbitrary number of additional columns containing data attributes can be
#' part of the rtb, so long as they follow the aforementioned columns.
#'
#' @param sch A schedule object.
#' @param from The id of predecessor activity.
#' @param to The id of successor activity.
#' @param type Specifies the relation type between activities.
#' The default type is FS and its value may be: FS, FF, SS, SF.
#' @param lag The time period between activities that the successor activity
#' `to` must be advanced after activity `from`  has been finished.
#' @param ... One or more vectors for associated relation attributes.
#'
#' @return A schedule with a relation tibble (rtb) added.
#'
#' @seealso [sch_title()], [sch_reference()], [sch_add_relation()],
#' [sch_nr_relations()], [sch_has_any_relation()], [sch_new()], [sch_plan()],
#' [sch_add_activities()], [sch_validate()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 1: Cost Information System") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'     first comes the theory, then the practice.Gent: Springer, 2014, p. 6"
#'   ) %>%
#'   sch_add_activities(
#'     id        = 1:17,
#'     name      = paste("a", as.character(1:17), sep=""),
#'     duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
#'   ) %>%
#'   sch_plan()
#' sch_has_any_relation(sch) # FALSE
#' sch_nr_relations(sch) # 0
#' sch_duration(sch) # 4
#'
#' sch %<>%
#'   sch_add_relations(
#'     from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
#'              7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
#'     to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
#'              12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
#'   ) %>%
#'   sch_plan()
#' sch_has_any_relation(sch) # TRUE
#' sch_nr_relations(sch) # 26
#' sch_duration(sch) # 11
#'
#' @export
sch_add_relations <- function(sch, from, to, type = "FS", lag = 0L, ...) {
  cpt_assert_schedule_object_valid(sch)

  nr_relations <- length(from)
  if (nr_relations == 0) {
    stop("There is no relation to be included!")
  }

  # Validation: vector from must be an integer.
  if (!(inherits(from, "integer"))) {
    stop("The value supplied to `from` id must be integer!")
  }

  # Validation: vector to must be an integer.
  if (!(inherits(to, "integer"))) {
    stop("The value supplied to `to` id must be integer!")
  }


  # Type validation
  if (!(inherits(type, "character"))) {
    stop("The value supplied to `type` must be a character!")
  }
  if(any(is.na(match(type, c("FS", "FF", "SS", "SF"))))) {
    stop("The value supplied to `type` must be 'FS', 'FF', 'SS' or 'SF'!")
  }
  if(length(type) == 1L) {
    type %<>% rep(nr_relations)
  } else if(length(type) != nr_relations) {
    stop("The length of `type` must be equal `from`'s length!")
  }

  # Lag validation
  if (!(inherits(lag, "integer"))) {
    stop("The value supplied to `lag` must be an integer!")
  }
  if(length(lag) == 1L) {
    lag %<>% rep(nr_relations)
  } else if(length(type) != nr_relations) {
    stop("The length of `type` must be equal `from`'s length!")
  }

  relations_tib <- tibble::tibble(
    from = from,
    to = to,
    type = type,
    lag = lag,
    critical = FALSE,
    ord = NA_integer_,
    i_from = NA_integer_,
    i_to = NA_integer_
  )

  extras <- list(...)
  extras <- cpt_collect_extra_vectors(nr_relations, extras)
  if (tibble::is_tibble(extras)) {
    relations_tib %<>% dplyr::bind_cols(extras)
  }

  if(sch$info$nr_relations > 0) {
    relations_tib <- dplyr::bind_rows(sch$relations, relations_tib)
  }

  relations_tib$ord <- 1:nrow(relations_tib)

  if(cpt_is_schedule_created(sch)) {
    sch$info$status <- "SOME_ACTIVITY"
  }
  sch$relations <- relations_tib
  sch$info$nr_relations <- nrow(relations_tib)
  sch$info$has_any_relation = TRUE

  return(sch)
}

#' Add Relation
#'
#' Add a relation to a schedule.
#'
#' @param sch A schedule object.
#' @param from
#' The id of predecessor activity.
#' Must exist an activity with from.
#' @param to
#' The id of successor activity.
#' Must exist an activity with to.
#' @param type
#' Specifies the type of relation between activities.
#' The default type is FS and its value may be: FS, FF, SS, SF, that means:
#' - **FS:** Finish-Start relation.
#' Activity 'to' id can only start after the finish of activity 'from' id.
#' - **FF:** Finish-Finish relation.
#' Activity 'to' id must finish together with activity 'from' id.
#' - **SS:** Start-Start relation.
#' Activity 'to' id must start together with activity 'from' id.
#' - **SF:** Start-Finish relation.
#' Activity 'to' id must finish when activity 'from' id starts.
#'
#' If type is not defined, it is assumed to be FS.
#' @param lag
#' The time period between activities that the successor activity
#'  'to' must be advanced after activity 'from' has been finished.
#'  The value may be negative, in such case, the activity 'to' will be
#'  anticipated 'lag' time periods.
#' It must be an integer, less than, equal or greater than zero.
#' If lag is not defined, it is assumed to be zero.
#'
#' @return A Schedule object with a relation added.
#'
#' @seealso [sch_has_any_relation()], [sch_nr_relations()],
#' [sch_add_relations()], [sch_plan()], [sch_validate()],
#' [sch_add_activities()], [sch_new()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 3: Old Carriage House Renovation") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 11") %>%
#'   sch_add_activity( 1L, "a1" , 2L) %>%
#'   sch_add_activity( 2L, "a2" , 2L) %>%
#'   sch_add_activity( 3L, "a3" , 4L) %>%
#'   sch_add_activity( 4L, "a4" , 3L) %>%
#'   sch_add_activity( 5L, "a5" , 4L) %>%
#'   sch_add_activity( 6L, "a6" , 1L) %>%
#'   sch_add_activity( 7L, "a7" , 1L) %>%
#'   sch_add_activity( 8L, "a8" , 1L) %>%
#'   sch_add_activity( 9L, "a9" , 1L) %>%
#'   sch_add_activity(10L, "a10", 1L) %>%
#'   sch_add_activity(11L, "a11", 3L) %>%
#'   sch_add_activity(12L, "a12", 2L) %>%
#'   sch_add_activity(13L, "a13", 1L) %>%
#'   sch_add_activity(14L, "a14", 1L) %>%
#'   sch_add_activity(15L, "a15", 2L) %>%
#'   sch_add_activity(16L, "a16", 1L) %>%
#'   sch_add_activity(17L, "a17", 1L) %>%
#'   sch_add_relation( 1L,  2L) %>%
#'   sch_add_relation( 2L,  3L) %>%
#'   sch_add_relation( 3L,  4L) %>%
#'   sch_add_relation( 4L,  5L) %>%
#'   sch_add_relation( 5L,  6L) %>%
#'   sch_add_relation( 6L,  7L) %>%
#'   sch_add_relation( 6L,  8L) %>%
#'   sch_add_relation( 6L,  9L) %>%
#'   sch_add_relation( 7L, 10L) %>%
#'   sch_add_relation( 8L, 10L) %>%
#'   sch_add_relation( 9L, 10L) %>%
#'   sch_add_relation(10L, 11L) %>%
#'   sch_add_relation(10L, 13L) %>%
#'   sch_add_relation(11L, 12L) %>%
#'   sch_add_relation(12L, 15L) %>%
#'   sch_add_relation(13L, 14L) %>%
#'   sch_add_relation(14L, 15L) %>%
#'   sch_add_relation(15L, 16L) %>%
#'   sch_add_relation(16L, 17L) %>%
#'   sch_plan()
#' sch_duration(sch)
#' sch_activities(sch)
#' sch_relations(sch)
#'
#' @export
sch_add_relation <- function(sch, from, to, type = "FS", lag = 0L) {
  return(
    sch %>%
      sch_add_relations(
        from = from,
        to = to,
        type = type,
        lag = lag
      )
  )
}


#' Add Relations Tibble
#'
#' Add relations tibble to a schedule.
#'
#' @param sch A schedule object.
#'
#' @param rtb A tibble or data frame with relations definitions.
#'
#' @return A Schedule object with a relation added.
#'
#' @examples
#' atb <- tibble::tibble(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
#' )
#' rtb <- data.frame(
#'   from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
#'            7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
#'   to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
#'            12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
#' )
#' sch <- sch_new() %>%
#'   sch_add_activities_tibble(atb) %>%
#'   sch_add_relations_tibble(rtb) %>%
#'   sch_plan()
#' sch_duration(sch) # 11
#'
#' @export
sch_add_relations_tibble <- function(sch, rtb) {
  columns <- names(rtb)
  if(any(columns == "type")) {
    type <- rtb$type
  } else {
    type = "FS"
  }
  if(any(columns == "lag")) {
    lag <- rtb$lag
  } else {
    lag = 0L
  }

  return(
    sch %>%
      sch_add_relations(
        from = rtb$from,
        to = rtb$to,
        type = type,
        lag = lag
      )
  )
}


#' Has Any Relation
#'
#' A logical value that indicates if the schedule has any relation.
#' A TRUE value means that the schedule has some relation;
#' a FALSE, means that the schedule do not have any relation.
#'
#' @param sch A schedule object.
#'
#' @return A logical value:
#' - TRUE: The schedule has any relation;
#' - FALSE: The schedule do not have any relation.
#'
#' @seealso [sch_topoi_la()], [sch_relations()], [sch_add_relations()],
#' [sch_topoi_sp()], [sch_has_any_activity()], [sch_all_predecessors()],
#' [sch_topoi_ad()], [sch_nr_relations()], [sch_all_successors()],
#' [sch_nr_activities()], [sch_topoi_tf()].
#'
#' @examples
#' sch <- sch_new()
#' sch_has_any_relation(sch) # FALSE
#'
#' sch <- sch_new() %>%
#'   sch_add_activity(1L, "A", 2L) %>%
#'   sch_add_activity(2L, "B", 5L, 1L, direction = "pred")
#' sch_has_any_activity(sch) # TRUE
#'
#' @export
sch_has_any_relation <- function(sch) {
  return(sch$info$has_any_relation)
}


#' Nr. of Relations
#'
#' Number of relations in a schedule as an integer value.
#'
#' @param sch A schedule object.
#'
#' @return A integer value indicating the number of relations.
#'
#' @seealso [sch_relations()], [sch_topoi_la()], [sch_topoi_tf()],
#' [sch_all_successors()], [sch_topoi_ad()], [sch_nr_activities()],
#' [sch_topoi_sp()], [sch_has_any_relation()], [sch_all_predecessors()],
#' [sch_add_relations()], [sch_has_any_activity()].
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
#' sch_nr_relations(sch) # 14
#'
#' @export
sch_nr_relations <- function(sch) {
  return(sch$info$nr_relations)
}

#' Relations
#'
#' Return a tibble with all relations of a schedule in topological order.
#' These are the main information calculated by CPM.
#'
#' The tibble is formed by following structure:
#'    - **from:** Predecessor activity id from a relation.
#'    - **to:** Successor activity id to a relation.
#'    - **type:** The type of relation between activities.
#'    Its value may be: FS, FF, SS, SF.
#'    - **lag:** The time period between activity predecessor and
#'    activity successor activity
#'    - **critical:** A critical relation is formed by two activity critical:
#'    predecessor and successor.
#'    \code{TRUE} indicates it is critical;
#'    \code{FALSE} indicates it is not critical.
#'    - **ord:** Indicates de order that the relation was added in the schedule.
#'    - **i_from:** It is the index of predecessor activity in the
#'    activities tibble.
#'    - **i_to:** It is the index of successor activity in the
#'    activities tibble.
#'
#'
#' @param sch A schedule object.
#' @param order Indicates the order of relations:
#'    - **`"topological"`:** The relations tibble is in topological order.
#'    - **`"insert"`:** The relations tibble is in insert order.
#'
#' @return A tibble with relations.
#'
#' @seealso [sch_add_activities()], [sch_has_any_relation()], [sch_topoi_tf()],
#' [sch_gantt_matrix()], [sch_activities()], [sch_add_relations()],
#' [sch_topoi_sp()], [sch_topoi_la()], [sch_non_critical_activities()],
#' [sch_topoi_ad()], [sch_nr_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 3: Old Carriage House Renovation") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 11") %>%
#'   sch_add_activity( 1L, "a1" , 2L) %>%
#'   sch_add_activity( 2L, "a2" , 2L) %>%
#'   sch_add_activity( 3L, "a3" , 4L) %>%
#'   sch_add_activity( 4L, "a4" , 3L) %>%
#'   sch_add_activity( 5L, "a5" , 4L) %>%
#'   sch_add_activity( 6L, "a6" , 1L) %>%
#'   sch_add_activity( 7L, "a7" , 1L) %>%
#'   sch_add_activity( 8L, "a8" , 1L) %>%
#'   sch_add_activity( 9L, "a9" , 1L) %>%
#'   sch_add_activity(10L, "a10", 1L) %>%
#'   sch_add_activity(11L, "a11", 3L) %>%
#'   sch_add_activity(12L, "a12", 2L) %>%
#'   sch_add_activity(13L, "a13", 1L) %>%
#'   sch_add_activity(14L, "a14", 1L) %>%
#'   sch_add_activity(15L, "a15", 2L) %>%
#'   sch_add_activity(16L, "a16", 1L) %>%
#'   sch_add_activity(17L, "a17", 1L) %>%
#'   sch_add_relation(14L, 15L) %>%
#'   sch_add_relation( 9L, 10L) %>%
#'   sch_add_relation( 2L,  3L) %>%
#'   sch_add_relation( 8L, 10L) %>%
#'   sch_add_relation(10L, 13L) %>%
#'   sch_add_relation( 5L,  6L) %>%
#'   sch_add_relation(11L, 12L) %>%
#'   sch_add_relation(15L, 16L) %>%
#'   sch_add_relation( 6L,  8L) %>%
#'   sch_add_relation( 3L,  4L) %>%
#'   sch_add_relation(16L, 17L) %>%
#'   sch_add_relation( 6L,  7L) %>%
#'   sch_add_relation(10L, 11L) %>%
#'   sch_add_relation(13L, 14L) %>%
#'   sch_add_relation( 4L,  5L) %>%
#'   sch_add_relation( 7L, 10L) %>%
#'   sch_add_relation(12L, 15L) %>%
#'   sch_add_relation( 6L,  9L) %>%
#'   sch_add_relation( 1L,  2L) %>%
#'   sch_plan()
#' # In "topological" order.
#' sch_relations(sch)
#' # In "insert" order.
#' sch_relations(sch, order = "insert")
#'
#' @export
sch_relations <- function(sch, order = "topological") {
  if(order == "topological") {
    return(sch$relations)
  }
  if(order == "insert"){
    return(sch$relations[order(sch$relations$ord), ])
  }
  stop("Error: Order must be 'topological' or 'insert'!")
}


#' Critical Relations
#'
#' Return a tibble with critical relations of a schedule in topological order.
#'
#' @param sch A schedule object.
#' @param order Indicates the order of relations:
#'    - **`"topological"`:** The relations tibble is in topological order.
#'    - **`"insert"`:** The relations tibble is in insert order.
#'
#' @return A tibble with critical relations.
#'
#' @seealso [sch_relations()], [sch_add_activities()], [sch_has_any_relation()],
#' [sch_topoi_tf()], [sch_gantt_matrix()], [sch_activities()], [sch_topoi_la()],
#' [sch_add_relations()], [sch_topoi_sp()], [sch_non_critical_activities()],
#' [sch_topoi_ad()], [sch_nr_relations()], [sch_non_critical_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 3: Old Carriage House Renovation") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 11") %>%
#'   sch_add_activity( 1L, "a1" , 2L) %>%
#'   sch_add_activity( 2L, "a2" , 2L) %>%
#'   sch_add_activity( 3L, "a3" , 4L) %>%
#'   sch_add_activity( 4L, "a4" , 3L) %>%
#'   sch_add_activity( 5L, "a5" , 4L) %>%
#'   sch_add_activity( 6L, "a6" , 1L) %>%
#'   sch_add_activity( 7L, "a7" , 1L) %>%
#'   sch_add_activity( 8L, "a8" , 1L) %>%
#'   sch_add_activity( 9L, "a9" , 1L) %>%
#'   sch_add_activity(10L, "a10", 1L) %>%
#'   sch_add_activity(11L, "a11", 3L) %>%
#'   sch_add_activity(12L, "a12", 2L) %>%
#'   sch_add_activity(13L, "a13", 1L) %>%
#'   sch_add_activity(14L, "a14", 1L) %>%
#'   sch_add_activity(15L, "a15", 2L) %>%
#'   sch_add_activity(16L, "a16", 1L) %>%
#'   sch_add_activity(17L, "a17", 1L) %>%
#'   sch_add_relation(14L, 15L) %>%
#'   sch_add_relation( 9L, 10L) %>%
#'   sch_add_relation( 2L,  3L) %>%
#'   sch_add_relation( 8L, 10L) %>%
#'   sch_add_relation(10L, 13L) %>%
#'   sch_add_relation( 5L,  6L) %>%
#'   sch_add_relation(11L, 12L) %>%
#'   sch_add_relation(15L, 16L) %>%
#'   sch_add_relation( 6L,  8L) %>%
#'   sch_add_relation( 3L,  4L) %>%
#'   sch_add_relation(16L, 17L) %>%
#'   sch_add_relation( 6L,  7L) %>%
#'   sch_add_relation(10L, 11L) %>%
#'   sch_add_relation(13L, 14L) %>%
#'   sch_add_relation( 4L,  5L) %>%
#'   sch_add_relation( 7L, 10L) %>%
#'   sch_add_relation(12L, 15L) %>%
#'   sch_add_relation( 6L,  9L) %>%
#'   sch_add_relation( 1L,  2L) %>%
#'   sch_plan()
#' # In "topological" order.
#' sch_critical_relations(sch)
#' # In "insert" order.
#' sch_critical_relations(sch, order = "insert")
#'
#' @export
sch_critical_relations <- function(sch, order = "topological") {
  critical <- NULL
  return(
    sch_relations(sch, order) %>%
      dplyr::filter(critical)
  )
}


#' Non Critical Relations
#'
#' Return a tibble with non critical relations of a schedule in topological order.
#'
#' @param sch A schedule object.
#' @param order Indicates the order of relations:
#' - **`"topological"`:** The relations tibble is in topological order.
#' - **`"insert"`:** The relations tibble is in insert order.
#'
#' @return A tibble with non critical relations.
#'
#' @seealso [sch_relations()], [sch_add_activities()], [sch_has_any_relation()],
#' [sch_topoi_tf()], [sch_gantt_matrix()], [sch_activities()], [sch_topoi_la()],
#' [sch_add_relations()], [sch_topoi_sp()], [sch_non_critical_activities()],
#' [sch_topoi_ad()], [sch_nr_relations()], [sch_critical_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Project 3: Old Carriage House Renovation") %>%
#'   sch_reference(
#'     "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 11") %>%
#'   sch_add_activity( 1L, "a1" , 2L) %>%
#'   sch_add_activity( 2L, "a2" , 2L) %>%
#'   sch_add_activity( 3L, "a3" , 4L) %>%
#'   sch_add_activity( 4L, "a4" , 3L) %>%
#'   sch_add_activity( 5L, "a5" , 4L) %>%
#'   sch_add_activity( 6L, "a6" , 1L) %>%
#'   sch_add_activity( 7L, "a7" , 1L) %>%
#'   sch_add_activity( 8L, "a8" , 1L) %>%
#'   sch_add_activity( 9L, "a9" , 1L) %>%
#'   sch_add_activity(10L, "a10", 1L) %>%
#'   sch_add_activity(11L, "a11", 3L) %>%
#'   sch_add_activity(12L, "a12", 2L) %>%
#'   sch_add_activity(13L, "a13", 1L) %>%
#'   sch_add_activity(14L, "a14", 1L) %>%
#'   sch_add_activity(15L, "a15", 2L) %>%
#'   sch_add_activity(16L, "a16", 1L) %>%
#'   sch_add_activity(17L, "a17", 1L) %>%
#'   sch_add_relation(14L, 15L) %>%
#'   sch_add_relation( 9L, 10L) %>%
#'   sch_add_relation( 2L,  3L) %>%
#'   sch_add_relation( 8L, 10L) %>%
#'   sch_add_relation(10L, 13L) %>%
#'   sch_add_relation( 5L,  6L) %>%
#'   sch_add_relation(11L, 12L) %>%
#'   sch_add_relation(15L, 16L) %>%
#'   sch_add_relation( 6L,  8L) %>%
#'   sch_add_relation( 3L,  4L) %>%
#'   sch_add_relation(16L, 17L) %>%
#'   sch_add_relation( 6L,  7L) %>%
#'   sch_add_relation(10L, 11L) %>%
#'   sch_add_relation(13L, 14L) %>%
#'   sch_add_relation( 4L,  5L) %>%
#'   sch_add_relation( 7L, 10L) %>%
#'   sch_add_relation(12L, 15L) %>%
#'   sch_add_relation( 6L,  9L) %>%
#'   sch_add_relation( 1L,  2L) %>%
#'   sch_plan()
#' # In "topological" order.
#' sch_non_critical_relations(sch)
#' # In "insert" order.
#' sch_non_critical_relations(sch, order = "insert")
#'
#' @export
sch_non_critical_relations <- function(sch, order = "topological") {
  critical <- NULL
  return(
    sch_relations(sch, order) %>%
      dplyr::filter(!critical)
  )
}


#' All Successors
#'
#' List all successors from an activity: direct and indirect successors.
#'
#' @param sch A schedule object.
#' @param id Activity id to be listed.
#' @param ign_to A relation to be ignored: id -> ign_to.
#' Activities from this relation will be ignored.
#'
#' @return A vector with all activities ids.
#'
#' @seealso [sch_predecessors()], [sch_activities()],
#' [sch_non_critical_activities()], [sch_successors()],
#' [sch_relations()], [sch_is_redundant()], [sch_all_predecessors()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity( 2L, "a2" , 4L,  5L, 12L) %>%
#'   sch_add_activity( 3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity( 4L, "a4" , 1L,  6L) %>%
#'   sch_add_activity( 5L, "a5" , 4L,  9L) %>%
#'   sch_add_activity( 6L, "a6" , 5L,  7L) %>%
#'   sch_add_activity( 7L, "a7" , 1L,  8L,11L) %>%
#'   sch_add_activity( 8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity( 9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity(10L, "a10", 3L, 12L) %>%
#'   sch_add_activity(11L, "a11", 3L, 12L) %>%
#'   sch_add_activity(12L, "a12", 0L) %>%
#'   sch_plan()
#'
#' sch_all_successors(sch, 2) # 5, 9, 12
#' sch_all_successors(sch, 7) # 8, 11, 12
#' sch_all_successors(sch, 10) # 12
#'
#' @export
sch_all_successors = function(sch, id, ign_to = NULL) {
  cpt_assert_activity_id_exist(sch, id)

  if(!base::is.null(ign_to)) {
    cpt_assert_activity_id_exist(sch, ign_to)
    cpt_assert_relation_exist(sch, id, ign_to)
  }

  temp_relation <- sch_relations(sch)
  if(!base::is.null(ign_to)) {
    u <- which(temp_relation$from == id & temp_relation$to == ign_to)
    temp_relation <- temp_relation[-u, ]
  }

  a_list <- c(id)
  for(i in 1:sch_nr_relations(sch)) {
    if(temp_relation$from[i] %in% a_list) {
      a_list <- c(a_list, temp_relation$to[i])
    }
  }
  base::rev(base::unique(base::rev(a_list[-1L])))
}

#' Successors
#'
#' List the direct successors from an activity.
#'
#' @param sch A schedule object.
#' @param id Activity id to be listed.
#'
#' @return A vector with all activities ids.
#'
#' @seealso [sch_relations()], [sch_all_predecessors()], [sch_activities()],
#' [sch_gantt_matrix()], [sch_predecessors()], [sch_is_redundant()],
#' [sch_all_successors()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity( 2L, "a2" , 4L,  5L, 12L) %>%
#'   sch_add_activity( 3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity( 4L, "a4" , 1L,  6L) %>%
#'   sch_add_activity( 5L, "a5" , 4L,  9L) %>%
#'   sch_add_activity( 6L, "a6" , 5L,  7L) %>%
#'   sch_add_activity( 7L, "a7" , 1L,  8L,11L) %>%
#'   sch_add_activity( 8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity( 9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity(10L, "a10", 3L, 12L) %>%
#'   sch_add_activity(11L, "a11", 3L, 12L) %>%
#'   sch_add_activity(12L, "a12", 0L) %>%
#'   sch_plan()
#'
#' sch_successors(sch, 2) # 5, 12
#' sch_successors(sch, 7) # 8, 11
#' sch_successors(sch, 10) # 12
#'
#' @export
sch_successors = function(sch, id) {
  from <- NULL
  cpt_assert_activity_id_exist(sch, id)

  temp_relation <- sch_relations(sch) %>%
    dplyr::filter(from == id)
  return(temp_relation$to)
}


#' All Predecessors
#'
#' List all predecessors from an activity: direct or indirect predecessors.
#'
#' @param sch A schedule object.
#' @param id Activity id to be listed.
#' @param ign_from A relation to be ignored: ign_from -> id.
#' Activities from this relation will be ignored.
#'
#' @return A vector with all activities ids.
#'
#' @seealso [sch_successors()], [sch_relations()], [sch_is_redundant()],
#' [sch_all_successors()], [sch_activities()], [sch_non_critical_activities()],
#' [sch_predecessors()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity( 2L, "a2" , 4L,  5L, 12L) %>%
#'   sch_add_activity( 3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity( 4L, "a4" , 1L,  6L) %>%
#'   sch_add_activity( 5L, "a5" , 4L,  9L) %>%
#'   sch_add_activity( 6L, "a6" , 5L,  7L) %>%
#'   sch_add_activity( 7L, "a7" , 1L,  8L,11L) %>%
#'   sch_add_activity( 8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity( 9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity(10L, "a10", 3L, 12L) %>%
#'   sch_add_activity(11L, "a11", 3L, 12L) %>%
#'   sch_add_activity(12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_all_predecessors(sch, 2) # nothing
#' sch_all_predecessors(sch, 7) # 6, 4
#' sch_all_predecessors(sch, 10) # 3
#'
#' @export
sch_all_predecessors <- function(sch, id, ign_from = NULL) {
  cpt_assert_activity_id_exist(sch, id)

  if(!base::is.null(ign_from)) {
    sch %>%
    cpt_assert_activity_id_exist(ign_from) %>%
    cpt_assert_relation_exist(ign_from, id)
  }

  temp_relation <- sch_relations(sch)
  if(!base::is.null(ign_from)) {
    u <- base::which(temp_relation$from == ign_from & temp_relation$to == id)
    temp_relation <- temp_relation[-u, ]
  }

  a_list <- c(id)
  for(i in sch_nr_relations(sch):1) {
    if(temp_relation$to[i] %in% a_list) {
      a_list <- c(a_list, temp_relation$from[i])
    }
  }
  rev(unique(rev(a_list[-1L])))
}

#' Predecessors
#'
#' List the direct predecessors of an activity.
#'
#' @param sch A schedule object.
#' @param id Activity id to be listed.
#'
#' @return A vector with all activities ids.
#'
#' @seealso [sch_gantt_matrix()], [sch_all_successors()], [sch_is_redundant()],
#' [sch_all_predecessors()], [sch_successors()], [sch_activities()],
#' [sch_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity( 2L, "a2" , 4L,  5L, 12L) %>%
#'   sch_add_activity( 3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity( 4L, "a4" , 1L,  6L) %>%
#'   sch_add_activity( 5L, "a5" , 4L,  9L) %>%
#'   sch_add_activity( 6L, "a6" , 5L,  7L) %>%
#'   sch_add_activity( 7L, "a7" , 1L,  8L,11L) %>%
#'   sch_add_activity( 8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity( 9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity(10L, "a10", 3L, 12L) %>%
#'   sch_add_activity(11L, "a11", 3L, 12L) %>%
#'   sch_add_activity(12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_predecessors(sch, 2) # nothing
#' sch_predecessors(sch, 7) # 6
#' sch_predecessors(sch, 10) # 3
#'
#' @export
sch_predecessors <- function(sch, id) {
  to <- NULL
  cpt_assert_activity_id_exist(sch, id)

  temp_relation <- sch_relations(sch) %>%
    dplyr::filter(to == id)
  return(temp_relation$from)
}

#' Is Redundant
#'
#' Verify if a relation between two activities is redundant.
#' A relation A->C is redundant if there are A->C, A->B, B->C relations.
#'
#' @param sch A schedule object.
#' @param id_from From activity id.
#' @param id_to To activity id.
#'
#' @return A logical \code{TRUE} if an arc is redundant;
#' \code{FALSE} if it is not.
#'
#' @seealso [sch_all_predecessors()], [sch_all_successors()],
#' [sch_gantt_matrix()], [sch_relations()], [sch_predecessors()],
#' [sch_successors()], [sch_activities()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_title("Fictitious Project Example") %>%
#'   sch_reference("VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18") %>%
#'   sch_add_activity( 2L, "a2" , 4L,  5L, 12L) %>%
#'   sch_add_activity( 3L, "a3" , 9L, 10L) %>%
#'   sch_add_activity( 4L, "a4" , 1L,  6L) %>%
#'   sch_add_activity( 5L, "a5" , 4L,  9L) %>%
#'   sch_add_activity( 6L, "a6" , 5L,  7L) %>%
#'   sch_add_activity( 7L, "a7" , 1L,  8L,11L) %>%
#'   sch_add_activity( 8L, "a8" , 7L, 12L) %>%
#'   sch_add_activity( 9L, "a9" , 8L, 12L) %>%
#'   sch_add_activity(10L, "a10", 3L, 12L) %>%
#'   sch_add_activity(11L, "a11", 3L, 12L) %>%
#'   sch_add_activity(12L, "a12", 0L) %>%
#'   sch_plan()
#' sch_is_redundant(sch, 2, 5) # FALSE
#' sch_is_redundant(sch, 2, 12) # TRUE
#'
#' @export
sch_is_redundant = function(sch, id_from, id_to) {
  cpt_assert_activity_id_exist(sch, id_from)
  cpt_assert_activity_id_exist(sch, id_to)
  cpt_assert_relation_exist(sch, id_from, id_to)

  succ <- sch_all_successors(sch, id_from, id_to)
  return(id_to %in% succ)
}


#' Evaluate Redundancy
#'
#' Evaluates redundancy of each relation and creates another column in
#' relation tibble. If the schedule does not have any relation, this function do
#' nothing.
#'
#' @param sch Object Schedule
#'
#' @return Object Schedule redundancy column added. Or the Schedule without any
#' modification, is trere is no relation in it.
#'
#' @examples
#' atb <- tibble::tibble(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1L,2L,2L,4L,3L,3L,3L,2L,1L,1L,2L,1L,1L,1L,1L,2L,1L)
#' )
#' rtb <- data.frame(
#'   from = c(1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,  3L,  4L,  5L,  6L,
#'            7L,  8L,  9L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L),
#'   to   = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 11L, 11L,
#'            12L, 13L, 14L, 15L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L, 16L, 17L)
#' )
#' sch <- sch_new() %>%
#'   sch_title("Project 1: Cost Information System") %>%
#'   sch_reference("VANHOUCKE, Mario.
#'       Integrated project management and control:
#'       first comes the theory, then the practice.
#'       Gent: Springer, 2014, p. 6") %>%
#'   sch_add_activities_tibble(atb) %>%
#'   sch_add_relations_tibble(rtb) %>%
#'   sch_plan() %>%
#'   sch_evaluate_redundancy()
#'
#' sch_duration(sch)  # 11L
#'
#' rtb <- sch_relations(sch)
#' sum(rtb$redundant) # 0
#'
#' sch1 <- sch %>%
#'   sch_add_relation(1L, 6L) %>%
#'   sch_add_relation(3L, 16L) %>%
#'   sch_add_relation(4L, 17L) %>%
#'   sch_plan() %>%
#'   sch_evaluate_redundancy()
#'
#' sch_duration(sch)  # 11L
#'
#' rtb <- sch_relations(sch1)
#' sum(rtb$redundant) # 3L
#'
#' @export
sch_evaluate_redundancy <- function(sch) {
  if(sch_nr_relations(sch) > 0) {
    rtb <- sch_relations(sch)
    rtb$redundant <- FALSE
    for(i in 1:sch_nr_relations(sch)) {
      rtb$redundant[i] <- sch_is_redundant(sch, rtb$from[i], rtb$to[i])
    }
    sch$relations <- rtb
  }
  return(sch)
}
