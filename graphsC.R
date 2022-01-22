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
nNodes <- 200 # JP to change

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
gC <- list()
for(i in 1:nGraphs){
  gC[[i]] <- dmc(n = nNodes,
                 qm = qm[i],
                 qc = qc[i])
}

durationTruthC <- proc.time()
truthC <- foreach(i = 1:length(gC),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gC[[i]][[1]],
                                                             init = c(qm[i],
                                                                      qc[i]),
                                                             u = gC[[i]][[2]])
durationTruthC <- proc.time() - durationTruthC

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

save.image("graphsC.RData")




