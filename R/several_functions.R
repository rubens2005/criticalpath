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
