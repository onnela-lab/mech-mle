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
load("graphsC.RData")


#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

durationMethod02c <- proc.time()
est02c <- foreach(i = 1:length(gC),
                  .combine = rbind,
                  .packages = c("igraph",
                                "combinat",
                                "VGAM")) %dopar% deconstruct(g = gC[[i]][[1]],
                                                             method = "minUni")
durationMethod02c <- proc.time() - durationMethod02c

#############################
### Remove unwanted stuff ###
#############################

rm(gC,
   truthC,
   durationTruthC)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("method02c.RData")




