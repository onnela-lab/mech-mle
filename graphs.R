#####################
### Load packages ###
#####################

library(igraph)
library(doParallel)
library(foreach)
library(combinat)
library(VGAM)

##################
### Parameters ###
##################

set.seed(0)
registerDoParallel(cores = 10)
nGraphs <- 1e2
nNodes <- 1e2

#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

# Parameters
sNGraphs <- sqrt(nGraphs)
qm <- rep(x = 1:sNGraphs / (sNGraphs + 1),
          each = sNGraphs)
qc <- rep(x = 1:sNGraphs / (sNGraphs + 1),
          times = sNGraphs)

# List of graphs
g <- list()
for(i in 1:nGraphs){
  g[[i]] <- dmc(n = nNodes,
                qm = qm[i],
                qc = qc[i])
}

durationTruth <- proc.time()
truth <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconstruct(g = g[[i]][[1]],
                                                            init = c(qm[i],
                                                                     qc[i]),
                                                            u = g[[i]][[2]])
durationTruth <- proc.time() - durationTruth

#############################
### Remove unwanted stuff ###
#############################

rm(i,
   nGraphs,
   nNodes,
   qc,
   qm)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("graphs.RData")




