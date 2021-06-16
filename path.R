#####################
### Load packages ###
#####################

library(igraph)


library(combinat)
library(VGAM)
library(colorRamps)

##################
### Parameters ###
##################

set.seed(3)


nNodes <- 1e2

#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

# Parameters
qm <- 0.5
qc <- 0.5

# Generate graph
gPath <- dmc(n = nNodes,
             qm = 0.5,
             qc = 0.5)

# Get results
estPath <- list()
estPath[[1]] <- deconstruct(g = gPath[[1]],
                            init = c(qm,qc),
                            u = gPath[[2]],
                            trace = TRUE)
estPath[[2]] <- deconstruct(g = gPath[[1]],
                            method = "minUni",
                            trace = TRUE)
estPath[[3]] <- deconstruct(g = gPath[[1]],
                            method = "NLL",
                            init = c(qm,qc),
                            trace = TRUE)
estPath[[4]] <- deconstruct(g = gPath[[1]],
                            method = "NLL",
                            init = c(0.1,0.1),
                            trace = TRUE)
estPath[[5]] <- deconstruct(g = gPath[[1]],
                            u = randAnc(n = gorder(gPath[[1]])),
                            trace = TRUE)
estPath[[6]] <- deconstruct(g = gPath[[1]],
                            method = "random",
                            trace = TRUE)

#############################
### Remove unwanted stuff ###
#############################

rm(nNodes)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("path.RData")




