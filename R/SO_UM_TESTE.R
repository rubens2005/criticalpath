rm(list=ls())
library(igraph)

#set.seed(245179306)

ids <- sample(c(  10,   20,   30,   40,   50,   60))


activities <- data.frame(
  id        = match(c(  10,   20,   30,   40,   50,   60), ids),
  name      = c("Andre", "Beatriz", "Carlos", "Denise", "Efron", "Francine"),
  duration  = c(  5,   4,   2,   3,   4,   3)
)
activities <- activities[sample(1:nrow(activities)), ]
rownames(activities) <- 1:nrow(activities)

relations <- data.frame(
  from = match(c(10, 10, 20, 20, 30, 40, 50), ids),
  to   = match(c(20, 30, 40, 60, 50, 60, 60), ids),
  type = "FS",
  lag  = 0
)
relations <- relations[sample(1:nrow(relations)), ]
rownames(relations) <- 1:nrow(relations)

g <- graph_from_data_frame(
  relations,
  directed = TRUE,
  activities
)

lay <- layout.sugiyama(g)
plot(g, layout=lay$layout)


#Retorna o vértices de acordo com a ordenação topológica
ts <- as.numeric(igraph::topo_sort(g, "out"))
activities <- activities[ts, ]
rownames(activities) <- 1:nrow(activities)

topo_order <- order(
  match(relations$from, activities$id),
  match(relations$to, activities$id)
)
relations <- relations[topo_order,]
rownames(relations) <- 1:nrow(relations)

g <- graph_from_data_frame(
  relations,
  directed = TRUE,
  activities
)
lay <- layout.sugiyama(g)
plot(g, layout=lay$layout, vertex.label=activities$id)

V(g)[[]]
E(g)[[]]

g <- set_vertex_attr(g, "ES", value=89)
vertex_attr(g)

g <- set_vertex_attr(g, "color", value = "red")



