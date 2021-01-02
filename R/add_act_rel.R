#' Add an activity and her relations
#'
#' Add an activity and her relations to a schedule.
#'
#' @usage
#' add_act_rel(schedule, id, name, duration, relations_id=c(), dir="succ")
#'
#' @param schedule
#' Schedule in which the activity and the relations will be added.
#'
#' @param id
#' Activity id. The id will be used to make relation between activities.
#'
#' @param name
#' The name of activity.
#'
#' @param duration
#' A number that represents the activity's duration.
#' It must be equal or grater than zero.
#'
#' @param relations_id A vector of ids such that will be linked with activity id.
#' It may be a relations of successor or predecessors.
#'
#' @param dir Direction of relations_id: It may be "succ" or "pred".
#' If dir="succ" the relations_id will be the successor of tye activity.
#' If dir="pred" the relations_id will be the predecessor of the activity.
#'
#' @return schedule with an activity and relations added to it.
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
#' schedule <- make_empty_schedule(
#'   "Fictitious Project Example",
#'   "VANHOUCKE, Mario. Measuring time:
#'     improving project performance using earned value management.
#'     Gent: Springer, 2009, p. 18"
#' )
#' schedule <- add_act_rel(schedule,  1, "a1" , 0, c(2,3,4))
#' schedule <- add_act_rel(schedule,  2, "a2" , 4, c(5))
#' schedule <- add_act_rel(schedule,  3, "a3" , 9, c(10))
#' schedule <- add_act_rel(schedule,  4, "a4" , 1, c(6))
#' schedule <- add_act_rel(schedule,  5, "a5" , 4, c(9))
#' schedule <- add_act_rel(schedule,  6, "a6" , 5, c(7))
#' schedule <- add_act_rel(schedule,  7, "a7" , 1, c(8,11))
#' schedule <- add_act_rel(schedule,  8, "a8" , 7, c(12))
#' schedule <- add_act_rel(schedule,  9, "a9" , 8, c(12))
#' schedule <- add_act_rel(schedule, 10, "a10", 3, c(12))
#' schedule <- add_act_rel(schedule, 11, "a11", 3, c(12))
#' schedule <- add_act_rel(schedule, 12, "a12", 0)
#' message("Schedule info:")
#' schedule$info
#' message("Activities info:")
#' schedule$activities
#'
add_act_rel <- function(schedule, id, name, duration, relations_id=c(), dir="succ") {
  assert_is_schedule(schedule)
  assert_activity_id_is_valid(id)
  assert_activity_id_does_not_exist(schedule, id)

  schedule <- add_activity(schedule, id, name, duration)

  n <- length(relations_id)
  if(n > 0) {
    if(any(duplicated(relations_id))) {
      stop("Must NOT EXISTS duplicated id in relations_id!")
    }

    #1 Add temp relations
    if(is.null(schedule$config$temp_relations)) {
      schedule$config$temp_relations <- data.frame(
        from = numeric(),
        to   = numeric(),
        type = character(),
        lag = numeric(),
        critical = logical(),
        redundant = logical(),
        ord = numeric(),
        i_from = numeric(),
        i_to = numeric()
      )
    }
    if(dir == "succ") {
      for(i in 1:n) {
        schedule$config$temp_relations <- rbind(
          schedule$config$temp_relations,
          data.frame(
            from = id,
            to   = relations_id[i],
            type = "FS",
            lag = 0,
            critical = FALSE,
            redundant = FALSE,
            ord = NA,
            i_from = NA,
            i_to = NA
          )
        )
      }
    } else if(dir == "pred") {
      for(i in 1:n) {
        schedule$config$temp_relations <- rbind(
          schedule$config$temp_relations,
          data.frame(
            from = relations_id[i],
            to   = id,
            type = "FS",
            lag = 0,
            critical = FALSE,
            redundant = FALSE,
            ord = NA,
            i_from = NA,
            i_to = NA
          )
        )
      }
    } else {
      message(base::paste("Invalid '", dir, "' direction!"))
    }
  }

  #2 Add temp_relations to relations if activity id exist

  temp_n <- ifelse(
    is.null(schedule$config$temp_relations),
    0,
    nrow(schedule$config$temp_relations)
  )
  if(temp_n > 0) {
    schedule$config$temp_relations$keep <- TRUE
    for(i in 1:temp_n) {
      from_id <- schedule$config$temp_relations$from[i]
      to_id <- schedule$config$temp_relations$to[i]
      type <- schedule$config$temp_relations$type[i]
      lag <- schedule$config$temp_relations$lag[i]
      j <- base::intersect(schedule$activities$id, from_id)
      k <- base::intersect(schedule$activities$id, to_id)
      if(length(j) > 0 && length(k) > 0) {
        schedule <- add_relation(schedule, from_id, to_id, type, lag)
        schedule$config$temp_relations$keep[i] <- FALSE
      }
    }

    #2 Remove temp_relations add
    if(sum(schedule$config$temp_relations$keep) > 0) {
      schedule$config$temp_relations <- schedule$config$temp_relations[schedule$config$temp_relations$keep, ]
      schedule$config$temp_relations$keep <- NULL
    } else {
      schedule$config$temp_relations <- NULL
    }
  }

  schedule
}
