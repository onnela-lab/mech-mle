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

durationMethod14a <- proc.time()
est14a <- foreach(i = 1:length(gA),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconSearch(g = gA[[i]][[1]],
                                                             method = "random",
                                                             iter = 1e2)
durationMethod14a <- proc.time() - durationMethod14a

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

save.image("method14a.RData")




