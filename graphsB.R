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
nNodes <- 200

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
gB <- list()
for(i in 1:nGraphs){
  gB[[i]] <- dmc(n = nNodes,
                 qm = qm[i],
                 qc = qc[i])
}

durationTruthB <- proc.time()
truthB <- foreach(i = 1:length(gB),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gB[[i]][[1]],
                                                             init = c(qm[i],
                                                                      qc[i]),
                                                             u = gB[[i]][[2]])
durationTruthB <- proc.time() - durationTruthB

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

save.image("graphsB.RData")




