% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sch_schedule.R
\name{sch_plan}
\alias{sch_plan}
\title{Plan Schedule}
\usage{
sch_plan(sch)
}
\arguments{
\item{sch}{A schedule object.}
}
\value{
A schedule with critical path calculated.
}
\description{
Perform schedule plan: execute topological sort and critical path calculation.
All information about critical path are calculated.
}
\examples{
sch <- sch_new() \%>\%
  sch_add_activities(
    id = c(1L, 2L, 3L, 4L),
    name = c("A", "B", "C", "D"),
    duration = c(3L, 4L, 9L, 1L)
  ) \%>\%
  sch_add_relations(
    from = c(1L, 2L, 2L),
    to   = c(2L, 3L, 4L)
  ) \%>\%
  sch_plan()
sch_duration(sch) # 16

}
\seealso{
\code{\link[=sch_gantt_matrix]{sch_gantt_matrix()}}, \code{\link[=sch_duration]{sch_duration()}}, \code{\link[=sch_reference]{sch_reference()}},
\code{\link[=sch_add_activities]{sch_add_activities()}}, \code{\link[=sch_has_any_activity]{sch_has_any_activity()}}, \code{\link[=sch_title]{sch_title()}}, \code{\link[=sch_new]{sch_new()}},
\code{\link[=sch_add_relations]{sch_add_relations()}}.
}
