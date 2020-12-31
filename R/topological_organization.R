#' Topological Organization
#'
#' This function is for internal use! It calculates topological definitions
#' that is necessary before critical path method.
#'
#' @param schedule A Schecule object.
#'
#' @return schedule
#'
#' @references Mario Vanhoucke
#'
#' Rubens Jose Rosa
#'
#' PMBOK
#'
topological_organization <- function(schedule) {
  if(schedule$info$has_any_relation) {
    schedule$relations <- schedule$relations[order(schedule$relations$ord), ]

    # 1) Verify if type and lag is defined, if not, define it
    # TODO Por enquanto não está sendo feita essa verificação
    schedule$relations$type <- "FS"
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


  #TODO 6) Verify redundant relation (transitive)
  # Existe um algoritmo que considera a matriz de relacionamentos
  # Mas acredito que tem alguns erros. Desta forma, eu tenho certeza, apesar de lento.
  # Para cada relação
  #   guarda a relação corrente
  #   tira a relação corrente do conjunto de próximos do nó from_id
  #   Ou seja, cria um conjunto com todas as relações com from_id, menos a corrente
  #   descobre todos os sucessores, diretos ou não, do nó from_id, até o final da rede
  #   se o nó to_id da relação corrente aparecer no conjunto
  #       marca a relação corrente como redundante TRUE
  #   se não
  #       marca a relação corrente como NÃO redundante FALSE
  #   fim-se
  # fim-para

  schedule
}
