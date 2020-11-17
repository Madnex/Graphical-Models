library(curl)
library(igraph)

selection <- "col4_day04"
filename <- paste("https://raw.githubusercontent.com/bansallab/asnr/master/Networks/Insecta/ants_proximity_weighted/ant_mersch_", selection, "_attribute.graphml"
                  , sep = "")
dest <- paste("Data/", selection, ".graphml", sep = "")
curl_download(filename, dest)

g <- read_graph(dest, format = "graphml")
g
plot(g)
vs <- V(g)
vcount(g)
V(g)
for(i in 1:6){
  selection <- paste("col", i, "_day04", sep="")
  dest <- paste("Data/", selection, ".graphml", sep = "")
  g <- read_graph(dest, format = "graphml")
  print(i)
  print(vcount(g))
  print(ecount(g))
}

vertex.attributes(g)
# Group corresponds to 1: days 1-10, 2: days 11-20, 3: days 21-30, 4: days 31-41
V(g)$"age(days)"
V(g)$group_period1 == "Q"
V(g)$color <- as.factor(V(g)$group_period1)
plot(g)
