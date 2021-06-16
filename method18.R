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

durationMethod18 <- proc.time()
est18 <- foreach(i = 1:length(g),
                 .combine = rbind,
                 .packages = c("igraph",
                               "combinat",
                               "VGAM")) %dopar% deconstruct(g = g[[i]][[1]],
                                                            u = randAnc(n = gorder(g[[i]][[1]])))
durationMethod18 <- proc.time() - durationMethod18

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

save.image("method18.RData")




