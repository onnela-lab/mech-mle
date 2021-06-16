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

durationMethod19 <- proc.time()
est19 <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconstruct(g = g[[i]][[1]],
                                                            method = "maxInt")
durationMethod19 <- proc.time() - durationMethod19

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

save.image("method19.RData")




