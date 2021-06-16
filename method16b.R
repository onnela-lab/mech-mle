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

set.seed(1)
registerDoParallel(cores = 10)
load("graphsB.RData")


#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

durationMethod16b <- proc.time()
est16b <- foreach(i = 1:length(gB),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gB[[i]][[1]],
                                                             method = "minInt")
durationMethod16b <- proc.time() - durationMethod16b

#############################
### Remove unwanted stuff ###
#############################

rm(gB,
   truthB,
   durationTruthB)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("method16b.RData")




