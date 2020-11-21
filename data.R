library(curl)
library(igraph)

# Download the data files for colony 1:

for(i in 1:41){
  selection <- paste("col1_day", formatC(i, width = 2, flag = 0), sep = "")
  filename <- paste("https://raw.githubusercontent.com/bansallab/asnr/master/Networks/Insecta/ants_proximity_weighted/ant_mersch_", selection, "_attribute.graphml"
                    , sep = "")
  dest <- paste("Data/", selection, ".graphml", sep = "")
  curl_download(filename, dest)
}


g <- read_graph(dest, format = "graphml")
g
plot(g)
vs <- V(g)
vcount(g)
V(g)
graphs <- list()
for(i in 1:41){
  selection <- paste("col1_day", formatC(i, width = 2, flag = 0), sep = "")
  dest <- paste("Data/", selection, ".graphml", sep = "")
  g <- read_graph(dest, format = "graphml")
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

