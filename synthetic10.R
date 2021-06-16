#####################
### Load packages ###
#####################

library(igraph)


library(combinat)
library(VGAM)

##################
### Parameters ###
##################

set.seed(1)




#################
### Functions ###
#################

source("functions.R")

######################
### Empirical Data ###
######################

# Load HuRI data
load("empirical10.RData")
gSynth10 <- dmc(n = gorder(gHuRI10),
                qm = resultHuRI10[1],
                qc = resultHuRI10[2])

# Run minUni
durationSynth10 <- proc.time()
resultSynth10 <- deconstruct(g = gSynth10[[1]],
                             method = "minUni",
                             returnUV = TRUE)
durationSynth10 <- proc.time() - durationSynth10

# STRING data
synthSTR <- list()
durationSynthSTR <- rep(x = NA,
                        times = length(gSTR))
resultSynthSTR <- list()
for(i in 1:length(gSTR)){
  synthSTR[[i]] <- dmc(n = gorder(gSTR[[i]]),
                       qm = resultSTR[[i]][1],
                       qc = resultSTR[[i]][2])
  durationSynthSTR[i] <- proc.time()[3]
  resultSynthSTR[[i]] <- deconstruct(g = synthSTR[[i]][[1]],
                                     method = "minUni",
                                     returnUV = TRUE)
  durationSynthSTR[i] <- proc.time()[3] - durationSynthSTR[i]
}

#############################
### Remove unwanted stuff ###
#############################

rm(list = lsf.str())

#################
### Save data ###
#################

save.image("synthetic10.RData")




