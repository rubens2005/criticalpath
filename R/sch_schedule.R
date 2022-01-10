#' New Schedule
#'
#' Create a new schedule without any information. The new schedule contains the
#' structure to include activities and relations.
#'
#' @return A list with schedule definition.
#'
#' @seealso [sch_reference()], [sch_add_activities()], [sch_duration()],
#' [sch_xy_gantt_matrix()], [sch_plan()], [sch_add_relations()],
#' [sch_validate()], [sch_non_critical_activities()], [sch_title()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id = c(1L, 2L, 3L, 4L),
#'     name = c("A", "B", "C", "D"),
#'     duration = c(3L, 4L, 9L, 1L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 2L),
#'     to   = c(2L, 3L, 4L)
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch) # 16
#'
#' @export
sch_new <- function() {
  sch <- list(
    activities = tibble::tibble(
      id = integer(),
      name = character(),
      duration = integer(),
      milestone = integer(),
      critical = integer(),
      early_start = integer(),
      early_finish = integer(),
      late_start = integer(),
      late_finish = integer(),
      total_float = integer(),
      free_float = integer()
    ),

    relations = tibble::tibble(
      from = integer(),
      to = integer(),
      type = character(),
      lag = integer(),
      critical = integer(),
      ord = integer(),
      i_from = integer(),
      i_to = integer()
    ),

    info = list(
      status = "EMPTY",

      title = "",
      reference = "",
      duration = 0L,

      nr_activities = 0L,
      has_any_activity = FALSE,

      nr_relations = 0L,
      has_any_relation = FALSE
    ),

    config = list()
  )
  return(sch)
}


#' Plan Schedule
#'
#' Perform schedule plan: execute topological sort and critical path calculation.
#' All information about critical path are calculated.
#'
#' @param sch A schedule object.
#'
#' @return A schedule with critical path calculated.
#'
#' @seealso [sch_gantt_matrix()], [sch_duration()], [sch_reference()],
#' [sch_add_activities()], [sch_has_any_activity()], [sch_title()], [sch_new()],
#' [sch_add_relations()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id = c(1L, 2L, 3L, 4L),
#'     name = c("A", "B", "C", "D"),
#'     duration = c(3L, 4L, 9L, 1L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 2L),
#'     to   = c(2L, 3L, 4L)
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch) # 16
#'
#' @export
sch_plan <- function(sch) {
  cpt_assert_schedule_object_valid(sch)

  if(cpt_is_schedule_empty(sch)) {
    stop("Error: There is no activity in the schedule!")
  }

  if(cpt_is_schedule_created(sch)) {
    stop("Error: Schedule already calculated! There is no necessity to be calculated!")
  }

  if(!cpt_is_schedule_with_some_activity(sch)) {
    stop(paste("Error: Status '", sch$info$status, "' is unknow!", sep = ""))
  }

  sch_valid <- sch_validate(sch)
  if(!sch_valid$is_valid) {
    stop("The schedule is invalid!!! To view the problem, execute:\n
         `validation <- sch_validate(sch)`")
  }

  sch$config$dag_igraph <- sch_valid$dag_igraph

  sch %<>% cpt_topological_organization() %>%
    cpt_calculate_critical_path()

  return(sch)
}

#' Title
#'
#' A title for project identification.
#' It depends on user of the class. It is used to set or get project's title.
#'
#' @param sch A schedule object.
#' @param new_value A new title.
#'
#' @return
#' - A schedule object with new title.
#' - A title.
#'
#' @seealso [sch_relations()], [sch_plan()], [sch_new()],
#' [sch_validate()], [sch_activities()], [sch_reference()],
#' [sch_duration()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id = c(1L, 2L, 3L, 4L),
#'     name = c("A", "B", "C", "D"),
#'     duration = c(3L, 4L, 9L, 1L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 2L),
#'     to   = c(2L, 3L, 4L)
#'   ) %>%
#'   sch_plan()
#'
#' sch_title(sch) # empty
#' sch %<>% sch_title("New title")
#' sch_title(sch)
#'
#' @export
sch_title <- function(sch, new_value) {
  if(missing(new_value)) {
    return(sch$info$title)
  }
  sch$info$title <- new_value
  return(sch)
}

#' Reference
#'
#' A reference from project origin, for example, a book, a paper, a corporation,
#' or nothing.
#'
#' @param sch A schedule object.
#' @param new_value A new reference.
#'
#' @return
#' - A schedule object with new reference.
#' - A reference.
#'
#' @seealso [sch_new()], [sch_activities()], [sch_relations()], [sch_title()],
#' [sch_plan()], [sch_duration()], [sch_validate()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id = c(1L, 2L, 3L, 4L),
#'     name = c("A", "B", "C", "D"),
#'     duration = c(3L, 4L, 9L, 1L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 2L),
#'     to   = c(2L, 3L, 4L)
#'   ) %>%
#'   sch_plan()
#'
#' sch_reference(sch) # empty
#' sch %<>% sch_reference("This schedule is from...")
#' sch_reference(sch)
#'
#' @export
sch_reference <- function(sch, new_value = NULL) {
  if(missing(new_value)) {
    return(sch$info$reference)
  }
  sch$info$reference <- new_value
  return(sch)
}

#' Duration
#'
#' An integer value that indicates the duration of a schedule.
#' **Atention:** the schedule must be planned with the function `sch_plan()`.
#'
#' @param sch A schedule object.
#'
#' @return The duration of the schedule.
#'
#' @seealso [sch_change_activities_duration()], [sch_validate()],
#' [sch_add_activities()], [sch_reference()], [sch_add_relations()],
#' [sch_title()], [sch_gantt_matrix()], [sch_plan()], [sch_new()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id = c(1L, 2L, 3L, 4L),
#'     name = c("A", "B", "C", "D"),
#'     duration = c(3L, 4L, 9L, 1L)
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 2L),
#'     to   = c(2L, 3L, 4L)
#'   ) %>%
#'   sch_plan()
#' sch_duration(sch) # 16
#'
#' @export
sch_duration <- function(sch) {
  if(!cpt_is_schedule_created(sch)) {
    stop("The schedule must be planned! Call function 'sch_plan(.)'!")
  }
  return(sch$info$duration)
}


#' Validate Schedule
#'
#' Validate your schedule in terms of structure: cannot have duplicated activity
#' id, all 'from' and 'to' relation id must exist in activities tibble
#' and cannot have duplicated relation. This function is called by
#' `sch_plan(plan)`. If there is an error, the  schedule cannot be calculated.
#'
#' There are two forms to use this function:
#' - The first is automatic, when you call `sch_plan(plan)`, the validation is
#' called for you.
#' - The second, you can call `sch_plan(plan)` with your schedule, before plan,
#' to see all error in your schedule.
#'
#' In both way, the calculation schedule is stopped, because there is some error.
#' To see the errors, you call the `sch_plan(plan)` function to find how to
#' correct the errors.
#'
#' The result of `sch_plan(plan)` is a lista with a lot of information about the
#' error. The structure is:
#' - `is_valid`: A logical value that indicates if the schedule structure is valid.
#'     - `TRUE`: The schedule structure is NOT valid.
#'     - `FALSE`: The schedule structure is valid.
#' - `is_error_with_activities`: A logical value that indicates if there is
#' any error  with activities.
#'     - `TRUE`: There is any error with activities.
#'     - `FALSE`: There is NOT any error with activities.
#' - `is_error_with_relations`:  A logical value that indicates if there is
#' any error  with relations.
#'     - `TRUE`: There is any error with relations.
#'     - `FALSE`: There is NOT any error with relations.
#' - `is_error_with_dag`: A logical value that indicates if there is any error
#' with `igraph` object tha support the schedule.
#'     - `TRUE`: There is any error with igraph.
#'     - `FALSE`: There is NOT any error with igraph.
#' - `activities_errors`: A tibble that list the activities errors:
#'     - `id`: activity's id of the error
#' 	   - `error`: the error.
#' 	   - `to_fix`: suggestion of how to fix the error.
#' - `relations_errors`:
#'     - `from`: Predecessor activity id 'from' of the error.
#' 	   - `to`: Successor activity id from a relation.
#' 	   - `error`: the error.
#' 	   - `to_fix`: suggestion of how to fix the error.
#' - `dag_errors`: Error identified by `igraph` object.
#' - `dag_igraph`: The `igraph` object that is totally validated.
#'
#'
#' **Attention:** You must identify and correct all errors
#' before call `sch_plan(plan)`!
#'
#' @param sch A schedule object.
#'
#' @return A list object with a description of all error.
#'
#' @seealso [sch_add_relation()], [sch_relations()], [sch_add_relations()],
#' [sch_add_activities()], [sch_add_activity()], [sch_activities()],
#' [sch_plan()].
#'
#' @export
sch_validate <- function(sch) {

  cpt_activity_duplicated <- function(ids) {
    id_dup <- unique(ids[duplicated(ids, fromLast = TRUE)])
    n <- length(id_dup)
    if(n == 0) {
      return (tibble::tibble())
    }

    dups <- character(n)
    for(ii in 1:n) {
      dups[ii] <- stringr::str_c(which(ids == id_dup[ii]), collapse = ", ")
    }
    return(tibble::tibble(
      id = id_dup,
      dup = dups,
      to_fix = "Change or remove duplicated id."
    ))
  }

  cpt_relation_id_exist <- function (aid, id_from, id_to) {
    a <- is.na(match(id_from, aid))
    b <- is.na(match(id_to, aid))
    error <- a | b
    if(!any(error)) {
      return (tibble::tibble())
    }

    from_errors <- tibble::tibble(
      from = id_from[a],
      to = id_to[a],
      error = "Id `from` does not exist.",
      to_fix = "Insert activity with 'from' id or exclude relation."
    )

    to_errors <- tibble::tibble(
      from = id_from[b],
      to = id_to[b],
      error = "Id `to` does not exist.",
      to_fix = "Insert activity with 'to' id or exclude relation."
    )


    return(dplyr::bind_rows(
      from_errors,
      to_errors
    ))

  }

  cpt_relation_duplicated <- function(id_from, id_to) {
    ids <- stringr::str_c(id_from, " -> ", id_to)
    id_dup <- unique(ids[duplicated(ids, fromLast = TRUE)])
    n <- length(id_dup)
    if(n == 0) {
      return (tibble::tibble())
    }

    errors <- tibble::tibble(
      from = integer(n),
      to = integer(n),
      error = "Relations are duplicated.",
      to_fix = "Exclude any duplicated relation."
    )

    sp <- stringr::str_split(id_dup, " -> ")
    for(ii in 1:n) {
      errors$from[ii] <- as.integer(sp[[ii]][1])
      errors$to[ii] <- as.integer(sp[[ii]][2])
    }

    return(errors)
  }

  ############
  cpt_is_dag <- function(atb, rtb) {
    dag_igraph <- try(
      igraph::graph_from_data_frame(
        rtb,
        TRUE,
        atb
      ),
      TRUE
    )

    if(!igraph::is_igraph(dag_igraph)) {
      return(list(
        valid = FALSE,
        error = dag_igraph[[1]],
        dag_igraph = NA
      ))
    }

    if(!igraph::is_dag(dag_igraph)) {
      return(list(
        valid = FALSE,
        error = "There is a cycle between relations.
        Example: A -> B, B -> C, C -> A",
        dag_igraph = NA
      ))
    }

    return(list(
      valid = TRUE,
      error = "",
      dag_igraph = dag_igraph
    ))

  }

  atb <- sch_activities(sch)
  rtb <- sch_relations(sch)

  # Activity duplicated
  dup_act <- cpt_activity_duplicated(atb$id)
  is_error_with_activities <- nrow(dup_act) > 0

  # Exist relation ids
  exist <-  cpt_relation_id_exist(atb$id, rtb$from, rtb$to)

  # Relation duplicated
  dup_rel <- cpt_relation_duplicated(rtb$from, rtb$to)
  is_error_with_relations <- nrow(exist) > 0 | nrow(dup_rel) > 0

  # Schedule is a DAG (directe acyclic graph)
  if(!is_error_with_activities & !is_error_with_relations) {
    dag <- cpt_is_dag(atb, rtb)
    is_error_with_dag <- !dag$valid
  } else {
    dag <- list(
      valid = TRUE,
      error = "",
      dag_igraph = NA
    )
    is_error_with_dag <- FALSE
  }


  is_valid <- !is_error_with_activities &
    !is_error_with_relations &
    !is_error_with_dag


  if(is_error_with_activities) {
    activities_errors = tibble::tibble(
      id = dup_act$id,
      error = stringr::str_c("Duplicated id at: ", dup_act$dup, "."),
      to_fix = dup_act$to_fix
    )
  } else {
    activities_errors = dup_act
  }

  if(nrow(exist) > 0) {
    relations_errors = tibble::tibble(
      from = exist$from,
      to = exist$to,
      error = exist$error,
      to_fix = exist$to_fix
    )
  } else {
    relations_errors = exist
  }

  if(nrow(dup_rel) > 0) {
    relations_errors %<>% dplyr::bind_rows(
      tibble::tibble(
        from = dup_rel$from,
        to = dup_rel$to,
        error = dup_rel$error,
        to_fix = dup_rel$to_fix
      )
    )
  }

  error_list <- list (
    is_valid = is_valid,
    is_error_with_activities = is_error_with_activities,
    is_error_with_relations = is_error_with_relations,
    is_error_with_dag = is_error_with_dag,
    activities_errors = activities_errors,
    relations_errors = relations_errors,
    dag_errors = dag$error,
    dag_igraph = dag$dag_igraph
  )

  return(error_list)
}

#' Gantt Matrix
#'
#' Create a matrix that represents a Gantt chart,
#' a matrix where "1" indicates that an activity is planned to be
#' in execution.
#' **Atention:** the schedule must be planned with the function `sch_plan()`.
#'
#' In this matrix, the rows represent activities,
#' whereas the columns represents the activity execution period.
#' So, the number of columns is equal to project duration. The cells is
#' an integer value that indicates the activity is in execution or not.
#'
#' @param sch A schedule object.
#'
#' @return A matrix where `1` indicates that an activity is in execution
#' and `0`, the activity is not executing.
#'
#' @seealso [sch_add_activities()], [sch_activities()], [sch_add_relations()],
#' [sch_add_relation()], [sch_relations()], [sch_plan()],
#' [sch_xy_gantt_matrix()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = c( 1L,  2L,  3L,  4L),
#'     name      = c("A", "B", "C", "D"),
#'     duration  = c( 2L,  3L,  1L, 2L )
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 4L, 4L),
#'     to   = c(3L, 3L, 1L, 2L)
#'   ) %>%
#'   sch_plan()
#'
#' sch_duration(sch)
#' gantt <- sch_gantt_matrix(sch)
#' gantt
#'
#' # What is the effort by time period?
#' colSums(gantt) # 1 1 2 2 1 1
#'
#' # What is the duration by activities?
#' rowSums(gantt) # 2 3 1 2
#'
#' # what is the S curve
#' cumsum(colSums(gantt))
#' plot(cumsum(colSums(gantt)), type="l", lwd=3)
#'
#' @export
sch_gantt_matrix <- function(sch) {
  if(sch_duration(sch) == 0L) {
    stop("There is no Gantt Matrix for a schedule with zero duration!")
  }
  atb <- sch_activities(sch)
  iii <- which(atb$duration > 0L)

  delta <- abs(min(atb$early_start))

  duration <- sch_duration(sch)
  nr_activities <- sch_nr_activities(sch)
  gantt <- base::matrix(base::integer(duration * nr_activities), nrow = nr_activities)
  for (i in iii) {
    start  <- atb$early_start[i] + delta + 1L
    finish <- atb$early_finish[i] + delta
    gantt[i, start:finish] <- 1L
  }
  class(gantt) <- base::unique(c("Gantt", class(gantt)))

  row.names(gantt) <- atb$id
  colnames(gantt) <- (min(atb$early_start) + 1L) : (max(atb$early_finish))

  return(gantt)
}


#' XY Gantt Matrix
#'
#' Transform a Gantt matrix into x, y coordinates and the weight one.
#' Each point greater than zero in a Gantt matrix becomes a x, y coordinate.
#' **Atention:** the schedule must be planned with the function `sch_plan()`.
#'
#' @param sch A schedule object.
#'
#' @param gantt A Gantt Matrix. If it is not informed, it will use
#' \code{gantt_matrix()} before this function.
#'
#' @return A matrix with three columns: x, y and weight.
#'
#' @seealso [sch_relations()], [sch_activities()], [sch_add_activities()],
#' [sch_add_relations()], [sch_add_relation()], [sch_plan()],
#' [sch_gantt_matrix()].
#'
#' @examples
#' sch <- sch_new() %>%
#'   sch_add_activities(
#'     id        = c( 1L,  2L,  3L,  4L),
#'     name      = c("A", "B", "C", "D"),
#'     duration  = c( 2L,  3L,  1L, 2L )
#'   ) %>%
#'   sch_add_relations(
#'     from = c(1L, 2L, 4L, 4L),
#'     to   = c(3L, 3L, 1L, 2L)
#'   ) %>%
#'   sch_plan()
#'
#' sch_duration(sch)
#'
#' xyw <- sch_xy_gantt_matrix(sch)
#' xyw
#' plot(xyw[, 1:2])
#'
#' @export
sch_xy_gantt_matrix <- function(sch, gantt = NULL) {
  if(base::is.null(gantt)) {
    gantt <- sch_gantt_matrix(sch)
  } else {
    cpt_assert_is_gantt(gantt)
  }
  qtdatvs <- base::nrow(gantt)
  pdur <- base::ncol(gantt)
  v <- as.integer(t(gantt))
  ii <- which(v > 0L) - 1L
  y <- base::floor(ii / pdur)
  x <- ii - y * pdur
  peso <- v[ii + 1L]
  base::matrix(c(x + 1L, y + 1L, peso), ncol = 3L)
}

