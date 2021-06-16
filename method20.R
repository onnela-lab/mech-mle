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

durationMethod20 <- proc.time()
est20 <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconstruct(g = g[[i]][[1]],
                                                            method = "minCon")
durationMethod20 <- proc.time() - durationMethod20

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

save.image("method20.RData")




