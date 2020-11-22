library(curl)
library(igraph)

source("helpers.R")

g <- get_graph()

plot(g)
vs <- V(g)
vcount(g)
V(g)
graphs <- list()
for(i in 1:41){
  g <- get_graph(colony = 1, day = i)
  V(g)$name <- V(g)$id
  graphs[[i]] <- g
}

unlist(lapply(graphs, get.adjacency))
unlist(lapply(graphs, vcount))

G <- union(graphs[[1]], graphs[[2]], byname = TRUE)
for(i in 3:41){
  G <- union(G, graphs[[i]], byname = TRUE)
}
plot(G)
g1 <- graphs[[1]]
g2 <- graphs[[2]]
u <- union(g1, g2, byname = TRUE)
vertex.attributes(G)
edge.attributes(G)

G_s <- simplify(G, remove.loops = FALSE)
ecount(G_s)
ecount(G)
plot(degree.distribution(G))

vertex.attributes(G)
edge.attributes(g)
# Group corresponds to 1: days 1-10, 2: days 11-20, 3: days 21-30, 4: days 31-41
g <- graphs[[1]]
V(g)$"age(days)"
V(g)$group_period1 == "Q"
V(g)$color <- as.factor(V(g)$group_period1)
plot(g, layout.grid(g), vertex.lables = NULL)
edge.attributes(g)
vertex.attributes(g)

