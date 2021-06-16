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
nNodes <- 7

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
gA <- list()
for(i in 1:nGraphs){
  gA[[i]] <- dmc(n = nNodes,
                 qm = qm[i],
                 qc = qc[i])
}

durationTruthA <- proc.time()
truthA <- foreach(i = 1:length(gA),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gA[[i]][[1]],
                                                             init = c(qm[i],
                                                                      qc[i]),
                                                             u = gA[[i]][[2]])
durationTruthA <- proc.time() - durationTruthA

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

save.image("graphsA.RData")




