#' Topological Organization
#'
#' This function is for internal use! It calculates topological definitions
#' that is necessary before critical path method.
#'
#' @param schedule A Schecule object.
#'
#' @return schedule
#'
#' @references
#'
#' Csardi, G. & Nepusz, T. (2005).
#' The Igraph Software Package for Complex Network Research.
#' *InterJournal*. Complex Systems. 1695.
#' [Article](https://www.researchgate.net/publication/221995787_The_Igraph_Software_Package_for_Complex_Network_Research)
#'  / [igraph](https://igraph.org/)
#'
#' Vanhoucke, M. (2009) *Measuring Time*:
#' Improving Project Performance Using Earned Value Management.
#' Springer-Verlag US.
#' doi: [10.1007/978-1-4419-1014-1](https://doi.org/10.1007/978-1-4419-1014-1).
#'
#' Vanhoucke, M. (2013) *Project Management with Dynamic Scheduling*:
#' Baseline Scheduling, Risk Analysis and Project Control.
#' Springer-Verlag Berlin Heidelberg.
#' doi: [10.1007/978-3-642-40438-2](https://doi.org/10.1007/978-3-642-40438-2)
#'
#' Vanhoucke, M. (2014) *Integrated Project Management and Control*:
#' First Comes the Theory, then the Practice.
#' Springer International Publishing Switzerland.
#' doi: [10.1007/978-3-319-04331-9](https://doi.org/10.1007/978-3-319-04331-9)
#'
#'
topological_organization <- function(schedule) {
  assert_is_schedule(schedule)

  if(schedule$info$has_any_relation) {
    schedule$relations <- schedule$relations[order(schedule$relations$ord), ]
  }

  # 2) Find starters activities
  ids <- setdiff(schedule$activities$id, schedule$relations$to)
  schedule$config$starters <- match(ids, schedule$activities$id)

  # 3) Find ends activities
  ids <- setdiff(schedule$activities$id, schedule$relations$from)
  schedule$config$ends <- match(ids, schedule$activities$id)

  # 4) Topological sorting
  if(nrow(schedule$relations) > 0) {

    schedule$relations$i_from <- match(schedule$relations$from, schedule$activities$id)
    schedule$relations$i_to <- match(schedule$relations$to, schedule$activities$id)

    g <- igraph::graph_from_data_frame(
      schedule$relations,
      TRUE,
      schedule$activities
    )
    ts <- as.numeric(igraph::topo_sort(g, "out"))

    topo_order <- order(
      match(schedule$relations$from, ts),
      match(schedule$relations$to, ts)
    )
    schedule$relations <- schedule$relations[topo_order,]
    rownames(schedule$relations) <- 1:nrow(schedule$relations)
  }

  # 5) Calculate topological levels

  # Progressive Level
  schedule$info$max_level <- 1
  schedule$activities$progr_level <- schedule$info$max_level
  # Forward calculate
  if(nrow(schedule$relations) > 0) {
    for(i in 1:nrow(schedule$relations)) {
      from_id <- schedule$relations$i_from[i]
      to_id <- schedule$relations$i_to[i]
      next_level <- schedule$activities$progr_level[from_id] + 1
      if(next_level > schedule$activities$progr_level[to_id]) {
        schedule$activities$progr_level[to_id] <- next_level
        if(next_level > schedule$info$max_level) {
          schedule$info$max_level <- next_level
        }
      }
    }
  }

  # Backward calculate
  schedule$activities$regr_level <- schedule$info$max_level
  if(nrow(schedule$relations) > 0) {
    for(i in nrow(schedule$relations):1) {
      from_id <- schedule$relations$i_from[i]
      to_id <- schedule$relations$i_to[i]
      prev_level <- schedule$activities$regr_level[to_id] - 1
      if(prev_level < schedule$activities$regr_level[from_id]) {
        schedule$activities$regr_level[from_id] <- prev_level
      }
    }
  }

  # Topological Float is the diference between regressivel and progressive level
  schedule$activities$topo_float <- schedule$activities$regr_level - schedule$activities$progr_level

  schedule
}
