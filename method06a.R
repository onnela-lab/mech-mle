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
load("graphsA.RData")


#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

durationMethod06a <- proc.time()
est06a <- foreach(i = 1:length(gA),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gA[[i]][[1]],
                                                             method = "NLL",
                                                             init = truthA[i,1:2])
durationMethod06a <- proc.time() - durationMethod06a

#############################
### Remove unwanted stuff ###
#############################

rm(gA,
   truthA,
   durationTruthA)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("method06a.RData")




