#' @noRd
cpt_topological_organization <- function(sch) {
  cpt_assert_schedule_object_valid(sch)

  # 1) Order relation to initial orders.
  if(sch$info$has_any_relation) {
    sch$relations <- sch$relations[order(sch$relations$ord), ]
  }

  # 2) Find starters activities
  ids <- setdiff(sch$activities$id, sch$relations$to)
  sch$config$starters <- match(ids, sch$activities$id)

  # 3) Find ends activities
  ids <- setdiff(sch$activities$id, sch$relations$from)
  sch$config$ends <- match(ids, sch$activities$id)

  # 4) Topological sorting
  if(sch$info$nr_relations > 0L) {

    sch$relations$i_from <- match(sch$relations$from, sch$activities$id)
    sch$relations$i_to <- match(sch$relations$to, sch$activities$id)

    g <- sch$config$dag_igraph
    ts <- as.integer(igraph::topo_sort(g, "out"))
    sorted_activity_id <- sch$activities$id[ts]
    topo_order <- order(
      match(sch$relations$from, sorted_activity_id),
      match(sch$relations$to, sorted_activity_id)
    )
    sch$relations <- sch$relations[topo_order, ]
  }

  # 5) Calculate topological levels

  # Progressive Level
  sch$info$max_level <- 1L
  sch$activities$progr_level <- sch$info$max_level
  # Forward calculate
  if(sch$info$has_any_relation) {
    for(i in 1:sch$info$nr_relations) {
      from <- sch$relations$i_from[i]
      to <- sch$relations$i_to[i]
      next_level <- sch$activities$progr_level[from] + 1L
      if(next_level > sch$activities$progr_level[to]) {
        sch$activities$progr_level[to] <- next_level
        if(next_level > sch$info$max_level) {
          sch$info$max_level <- next_level
        }
      }
    }
  }

  # Backward calculate
  sch$activities$regr_level <- sch$info$max_level
  if(sch$info$has_any_relation) {
    for(i in sch$info$nr_relations:1) {
      from <- sch$relations$i_from[i]
      to <- sch$relations$i_to[i]
      prev_level <- sch$activities$regr_level[to] - 1L
      if(prev_level < sch$activities$regr_level[from]) {
        sch$activities$regr_level[from] <- prev_level
      }
    }
  }

  # Topological Float is the difference between regressive and progressive level
  sch$activities$topo_float <- sch$activities$regr_level - sch$activities$progr_level
  sch$info$status <- "CREATED"

  return(sch)
}
