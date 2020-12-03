# 
source("helpers.R")


g <- get_graph(colony=1, day=4)

# Connected yes/no
is_connected(g)

# Edge density 
edge_density(g)
plot(degree_distribution(g))


########################################################
# Building subgraphs
#######################################################
foragers <- induced_subgraph(g, which(V(g)$group=="F"))
nurses <- induced_subgraph(g, which(V(g)$group=="N"))
cleaners <- induced_subgraph(g, which(V(g)$group=="C"))

groups <- list(foragers=foragers, nurses=nurses, cleaners=cleaners)

lapply(groups, edge_density)

lapply(groups, plot)

lapply(groups, is_connected)

lapply(groups, function(u) plot(degree_distribution(u)))

cl_n <- cliques(nurses)
membership(nurses)

betw <- betweenness(g)
betw <- betw[order(betw, decreasing = TRUE)]
library(ggplot2)


V(g)$group
betw <- data.frame(betw)
betw<-data.frame(name=rownames(betw),value=betw$betw)
ggplot(betw) + aes(y=value,x=name) + geom_col()
