is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# Verify is a schedule object is of Schedule class
is_schedule <- function (schedule)  {
  "Schedule" %in% class(schedule)
}

assert_is_schedule <- function(schedule) {
  if (!is_schedule(schedule)) {
    stop("Not a Schedule object!")
  }
}

# Verify is a gantt object is of Gantt matrix class
is_gantt <- function (gantt)  {
  "Gantt" %in% class(gantt)
}

assert_is_gantt <- function(gantt) {
  if (!is_gantt(gantt)) {
    stop("Not a Gantt matrix object!")
  }
}

get_activity <- function(schedule, id) {
  assert_is_schedule(schedule)
  assert_activity_id_exist(schedule, id)
  schedule$activities[match(id, schedule$activities$id), ]
}

assert_activity_id_is_valid <- function(activity_id) {
  if(base::is.null(activity_id)) {
    stop("Activity id cannot be NULL!")
  }
  if(base::is.na(activity_id)) {
    stop("Activity id cannot be NA!")
  }
  if(!is_wholenumber(activity_id)) {
    stop("Activity id must be a whole number!")
  }
}

activity_id_exist <- function(schedule, id) {
  if(is.na(base::match(id, schedule$activities$id))) {
    return(FALSE)
  }
  TRUE
}

assert_activity_id_exist <- function(schedule, id) {
  if(!activity_id_exist(schedule, id)){
    stop("Activity id must exist em activities list!")
  }
}

assert_activity_id_does_not_exist <- function(schedule, id) {
  if(activity_id_exist(schedule, id)){
    stop("Activity id must NOT EXIST em activities list!")
  }
}

assert_relation_exist <- function(schedule, from_id, to_id) {
  from_exist <- schedule$relations$from == from_id
  to_exist <- schedule$relations$to == to_id
  u <- which(from_exist & to_exist)
  if(length(u) == 0){
    stop(paste("Relation", from_id, "->", to_id, " does NOT exist!"))
  }
}

assert_relation_does_not_exist <- function(schedule, from_id, to_id) {
  from_exist <- schedule$relations$from == from_id
  to_exist <- schedule$relations$to == to_id
  u <- which(from_exist & to_exist)
  if(length(u) > 0){
    stop(paste("Relations", from_id, "->", to_id, " must NOT exist!"))
  }
}
