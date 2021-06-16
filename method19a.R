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

durationMethod19a <- proc.time()
est19a <- foreach(i = 1:length(gA),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gA[[i]][[1]],
                                                             method = "maxInt")
durationMethod19a <- proc.time() - durationMethod19a

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

save.image("method19a.RData")




