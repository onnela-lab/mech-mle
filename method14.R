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
load("graphs.RData")


#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

durationMethod14 <- proc.time()
est14 <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconSearch(g = g[[i]][[1]],
                                                            method = "random",
                                                            iter = 1e2)
durationMethod14 <- proc.time() - durationMethod14

#############################
### Remove unwanted stuff ###
#############################

rm(g,
   truth,
   durationTruth)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("method14.RData")




