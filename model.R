source("helpers.R")

# Fixed for colony 1 and day 17
g <- get_graph(colony=1, day=17)

set.seed(42)
e <- ecount(g)
n <- vcount(g)
p <- e/(n*(n-1)/2)
di.g <- diameter(g)
cl.g <- transitivity(g)
apl.g <- average.path.length(g)


er.graph=erdos.renyi.game(n,p)

plot(g)
plot(er.graph)

cl.p <- transitivity(er.graph)
apl.p <- average.path.length(er.graph)
di.p <- diameter(er.graph)

## Simulation
n_sim <- 1000
cl.sim <- numeric(n_sim)
apl.sim <- numeric(n_sim)
di.sim <- numeric(n_sim)
for (i in 1:n_sim){
  g.temp=erdos.renyi.game(n,p)
  cl.sim[i] <- transitivity(g.temp)
  apl.sim[i] <- average.path.length(g.temp)
  di.sim[i] <- diameter(g.temp)
}

mean(cl.sim > cl.g)
mean(apl.sim > apl.g)
mean(di.sim > di.g)

## Optimal p search
steps=seq(0,1,0.05)
len=length(steps)
cl=numeric(len)
apl=numeric(len)
di=numeric(len)
ntrials=100
for (i in 1:len){
  cltemp=numeric(ntrials)
  apltemp=numeric(ntrials)
  ditemp=numeric(ntrials)
  for (j in 1:ntrials)	{
    g_temp=erdos.renyi.game(n,steps[i])
    cltemp[j]=transitivity(g_temp)
    apltemp[j]=average.path.length(g_temp)
    ditemp[j]=diameter(g_temp)
  }
  cl[i]=mean(cltemp)
  apl[i]=mean(apltemp)
  di[i]=mean(ditemp)
}
cl
apl
di
best_cl <- steps[which.min(abs(cl-cl.g))]
best_apl <- steps[which.min(abs(apl-apl.g))]
best_di <- steps[which.min(abs(di-di.g))]

cl.opt <- cl[which.min(abs(cl-cl.g))]
apl.opt <- apl[which.min(abs(apl-apl.g))]
di.opt <- di[which.min(abs(di-di.g))]


## Watts-Strogatz
ws.graph=watts.strogatz.game(1,n,10,p)
plot(ws.graph)
transitivity(ws.graph)
average.path.length(ws.graph)


steps=seq(-4,-0.5,0.1)
len=length(steps)
cl=numeric(len)
apl=numeric(len)
ntrials=100
for (i in 1:len){
  cltemp=numeric(ntrials)
  apltemp=numeric(ntrials)
  for (j in 1:ntrials)	{
    g_temp=watts.strogatz.game(1,n,10,10^steps[i])
    cltemp[j]=transitivity(g_temp)
    apltemp[j]=average.path.length(g_temp)
  }
  cl[i]=mean(cltemp)
  apl[i]=mean(apltemp)
}
cl
apl


## Correlation Networks
vertex.attributes(g)
g.size <- V(g)$body_size
g.age <- V(g)$`age(days)`
g.forage <- V(g)$nb_foraging_events
g.brood <- V(g)$visits_to_brood
g.nest <- V(g)$visits_to_nest_entrance
g.rubbish <- V(g)$visits_to_rubbishpile
g.queen <- V(g)$nb_interaction_queen

g.attr <- data.frame("size" = g.size, "age" = g.age, "forage" = g.forage,
                     "brood" = g.brood, "nest" = g.nest, "rubbish" = g.rubbish,
                     "queen" = g.queen)
plot(g.attr)

mycorr <- cor(g.attr, use = "complete.obs")
z=0.5*log((1+mycorr)/(1-mycorr))

z.vec=z[upper.tri(z)]
n=dim(g.attr)[1]
corr.pvals=2*pnorm(abs(z.vec),0,sqrt(1/(n-3)),lower.tail=FALSE)
length(corr.pvals) #tests simoultenously
# multiple correction testing
corr.pvals.adj=p.adjust(corr.pvals,"BH")
#number of final edges
length(corr.pvals.adj[corr.pvals.adj<0.05])
#check if normality is satisfied by the data
qqnorm(as.matrix(g.attr))

apply(g.attr, 2, shapiro.test)


#partial correlation network
pcorr=function(mycorr)
{
  d1=dim(mycorr)[1]	
  d2=dim(mycorr)[2]
  for (i in seq(1,d1)){
    for (j in seq(1,d2)){
      rowi=mycorr[i,-c(i,j)]
      rowj=mycorr[j,-c(i,j)]
      pcorr.out=(mycorr[i,j]-rowi*rowj)/sqrt((1-rowi^2)*(1-rowj^2))
    }	
  }
  return(pcorr.out)	
}

tmp=pcorr(mycorr)
###############################
d1=dim(mycorr)[1]	
d2=dim(mycorr)[2]
pcorr.pvals=matrix(0,d1,d2)
pcorr.pvals=matrix(0,d1,d2)
for (i in seq(1,d1)){
  for (j in seq(1,d2)){
    rowi=mycorr[i,-c(i,j)]
    rowj=mycorr[j,-c(i,j)]
    tmp=(mycorr[i,j]-rowi*rowj)/sqrt((1-rowi^2)*(1-rowj^2))
    tmp.zvals=0.5*log((1+tmp)/(1-tmp))
    tmp.s.zvals=sqrt(n-4)*tmp.zvals
    tmp.pvals=2*pnorm(abs(tmp.s.zvals),0,1,lower.tail=FALSE)
    pcorr.pvals[i,j]=max(tmp.pvals)
  }	
}


#apply for multiple testing
pcorr.pvals.vec=pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj=p.adjust(pcorr.pvals.vec, "BH")
#apply significant level gamma=0.05
pcorr.edges=pcorr.pvals.adj<0.05
#25 edges have been detected
length(pcorr.pvals.adj[pcorr.edges])

#create the final network
pcorr.A=matrix(0,153,153)
pcorr.A[lower.tri(pcorr.A)]=as.numeric(pcorr.edges)
g.pcorr=graph.adjacency(pcorr.A,"undirected")
plot(g.pcorr,vertex.size=3,vertex.color="lightblue",vertex.label=NA)

#compare it with original network
graph.intersection(g, g.pcorr, byname=FALSE)


## Gaussian-Graphical Model
library(huge)
set.seed(4)

huge.out <- huge(as.matrix(g.attr), lambda = seq(0.1, 10, 0.5))

# choice of penalty parameter
huge.opt <- huge.select(huge.out, criterion="ric")
huge.opt$opt.lambda
sum(huge.opt$refit)
graph.adjacency(huge.opt$refit) #returns an empty graph


### ERGM models
library(network)
library(ergm)
library(intergraph)

g.network <- asNetwork(g)
edge.indep <- ergm(g.network ~ edges) 

summary(edge.indep)
edge.indep$coef[1]
plogis(coef(edge.indep))
# Nodes have a 68.9% chance of being connected


edge.group <- ergm(g.network ~ edges + nodematch("group")) 
summary(edge.group)

plogis(coef(edge.group)[1] + coef(edge.group)[2])
# Nodes of the same group have a 88% chance of being connected

gof.edge.group=gof(edge.group)
par(mfrow=c(2,2))
plot(gof.edge.group)

