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

durationMethod19b <- proc.time()
est19b <- foreach(i = 1:length(gB),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gB[[i]][[1]],
                                                             method = "maxInt")
durationMethod19b <- proc.time() - durationMethod19b

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

save.image("method19b.RData")




