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

durationMethod21 <- proc.time()
est21 <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconstruct(g = g[[i]][[1]],
                                                            method = "maxCon")
durationMethod21 <- proc.time() - durationMethod21

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

save.image("method21.RData")




