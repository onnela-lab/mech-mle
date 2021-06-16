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

durationMethod17a <- proc.time()
est17a <- foreach(i = 1:length(gA),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gA[[i]][[1]],
                                                             method = "minUniMinusInt")
durationMethod17a <- proc.time() - durationMethod17a

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

save.image("method17a.RData")




