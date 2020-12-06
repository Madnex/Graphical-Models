# 
source("helpers.R")

# Fixed for colony 1 and day 17
g <- get_graph(colony=1, day=17)

# Connected yes/no
is_connected(g)

# Edge density 
edge_density(g)
plot(degree_distribution(g))

diameter(g)

average.path.length(g)

hist(degree(g))


########################################################
# Building subgraphs
#######################################################
foragers <- induced_subgraph(g, which(V(g)$group=="F"))
nurses <- induced_subgraph(g, which(V(g)$group=="N"))
cleaners <- induced_subgraph(g, which(V(g)$group=="C"))

groups <- list(Foragers=foragers, Nurses=nurses, Cleaners=cleaners)

lapply(groups, edge_density)

lapply(groups, plot)

lapply(groups, is_connected)

lapply(groups, function(u) plot(degree_distribution(u), xlab = "Degree", ylab = "Density"))

lapply(groups, average.path.length)

lapply(groups, diameter)

lapply(names(groups), function(u) hist(degree(groups[[u]]), freq = FALSE, 
                                       main = paste("Histogram of", u), xlab = "Degree"))



library(ggplot2)
betw <- betweenness(g)
betw <- betw[order(betw, decreasing = TRUE)]
betw <- data.frame(betw)
betw<-data.frame(name=rownames(betw),value=betw$betw)
betw$group <- as.factor(sapply(betw$name, function(u) V(g)$group[V(g)$name == u]))
index<-10000:nrow(betw)+10000
for (i in 1:nrow(betw)){
  betw$name[i]<-paste(index[i],betw$name[i])
}

ggplot(betw) + aes(y=value, x=name, reorder(name, value), fill=group) + geom_col()


library(scales)
G <- g
G_Grouped = G

## Add edges with high weight between all nodes in the same group
for(i in unique(V(G)$group)) {
  GroupV = which(V(G)$group == i)
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=10))
} 

## Now create a layout based on G_Grouped
set.seed(123)
LO = layout_with_fr(G_Grouped)

V(G)$group <- as.factor(V(G)$group)
degs <- rescale(degree(g_plot), c(0,1))
## Use the layout to plot the original graph
plot(G, vertex.label = NA, vertex.color=V(G)$group, layout=LO, vertex.size = degs * 13)
legend('topleft',legend=c("NA", "Cleaners", "Foragers", "Nurses", "Queen"), col='black', pch=21, pt.bg=categorical_pal(5))

plot(g_plot, vertex.label = NA, vertex.color=V(g_plot)$group, vertex.size = degs * 13)






bridges <- crossing(membership = V(g)$group, graph = network)

betw <- betweenness(g)
betw <- betw[order(betw, decreasing = TRUE)]
library(ggplot2)


V(g)$group
betw <- data.frame(betw)
betw<-data.frame(name=rownames(betw),value=betw$betw)
ggplot(betw) + aes(y=value,x=name) + geom_col()





cluster_infomap(g)
