# 
source("helpers.R")


g <- get_graph(colony=1, day=4)

# Connected yes/no
is_connected(g)

# Edge density 
edge_density(g)
degree_distribution(g)


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

