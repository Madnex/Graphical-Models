# Functions

library(curl)
library(igraph)

get_data_from_repo <- function(colony=1, days=1:41){
  for(i in days){
    selection <- paste("col", colony , "_day", formatC(i, width = 2, flag = 0), sep = "")
    filename <- paste("https://raw.githubusercontent.com/bansallab/asnr/master/Networks/Insecta/ants_proximity_weighted/ant_mersch_", selection, "_attribute.graphml"
                      , sep = "")
    dest <- paste("Data/", selection, ".graphml", sep = "")
    curl_download(filename, dest)
  }
}

get_graph <- function(colony=1, day=4){
  selection <- paste("col", colony, "_day", formatC(day, width = 2, flag = 0), sep = "")
  dest <- paste("Data/", selection, ".graphml", sep = "")
  g <- read_graph(dest, format = "graphml")
  return(g)
}
