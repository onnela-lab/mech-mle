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

durationMethod03b <- proc.time()
est03b <- foreach(i = 1:length(gB),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconSearch(g = gB[[i]][[1]],
                                                             method = "minUni")
durationMethod03b <- proc.time() - durationMethod03b

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

save.image("method03b.RData")




