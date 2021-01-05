# Functions

library(curl)
library(igraph)

# To load the data sets from the repository
get_data_from_repo <- function(colony=1, days=1:41){
  for(i in days){
    selection <- paste("col", colony , "_day", formatC(i, width = 2, flag = 0), sep = "")
    filename <- paste("https://raw.githubusercontent.com/bansallab/asnr/master/Networks/Insecta/ants_proximity_weighted/ant_mersch_", selection, "_attribute.graphml"
                      , sep = "")
    dest <- paste("Data/", selection, ".graphml", sep = "")
    curl_download(filename, dest)
  }
}

# Create a graph object from the input
get_graph <- function(colony=1, day=4){
  selection <- paste("col", colony, "_day", formatC(day, width = 2, flag = 0), sep = "")
  dest <- paste("Data/", selection, ".graphml", sep = "")
  g <- read_graph(dest, format = "graphml")
  # Enrich the graph with the day and the belonging grouping
  g$day <- day
  V(g)$group <- get_group(g)
  V(g)$name <- V(g)$id
  return(g)
}

# Assign the group fitting to the period
# Group corresponds to 1: days 1-10, 2: days 11-20, 3: days 21-30, 4: days 31-41
get_group <- function(g){
  day <- g$day
  if(day <= 11){
    return(V(g)$group_period1)
  }
  if(day <= 21){
    return(V(g)$group_period2)
  }
  if(day <= 31){
    return(V(g)$group_period3)
  }
  return(V(g)$group_period4)
}