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

durationMethod18b <- proc.time()
est18b <- foreach(i = 1:length(gB),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gB[[i]][[1]],
                                                             u = randAnc(n = gorder(gB[[i]][[1]])))
durationMethod18b <- proc.time() - durationMethod18b

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

save.image("method18b.RData")




