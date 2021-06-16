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


p <- 0.10

#################
### Functions ###
#################

source("functions.R")

######################
### Empirical Data ###
######################

# Load HuRI data
HuRI <- read.table(file = 'HuRI.tsv',
                   sep = '\t',
                   stringsAsFactors = FALSE)
gHuRI <- simplify(graph_from_edgelist(el = as.matrix(HuRI),
                                      directed = FALSE))
V(gHuRI)$name <- as.character(as.numeric(sub(pattern = "ENSG",
                                             replacement = "",
                                             x = V(gHuRI)$name)))
gHuRI10 <- induced_subgraph(graph = gHuRI,
                            vids = sample(x = V(gHuRI)$name,
                                          size = round(p * gorder(gHuRI))))

# Run minUni
durationHuRI10 <- proc.time()
resultHuRI10 <- deconstruct(g = gHuRI10,
                            method = "minUni",
                            returnUV = TRUE)
durationHuRI10 <- proc.time() - durationHuRI10

# Load STRING data and run minUni
name <- c("caena",
          "caenp",
          "drosa",
          "drosp",
          "escha",
          "eschp",
          "homoa",
          "homop",
          "musma",
          "musmp",
          "sacca",
          "saccp")
gSTR <- list()
resultSTR <- list()
durationSTR <- rep(x = NA,
                   times = length(name))
for(i in 1:length(name)){
  temp <- read.table(file = paste0(name[i],
                                   ".tsv"),
                     sep = '\t',
                     stringsAsFactors = FALSE)
  temp <- simplify(graph_from_edgelist(el = as.matrix(temp),
                                        directed = FALSE))
  gSTR[[i]] <- induced_subgraph(graph = temp,
                                vids = sample(x = V(temp)$name,
                                              size = round(p * gorder(temp))))
  V(gSTR[[i]])$name <- as.character(1:length(V(gSTR[[i]])$name))
  
  # Run minUni
  durationSTR[i] <- proc.time()[3]
  resultSTR[[i]] <- deconstruct(g = gSTR[[i]],
                                method = "minUni",
                                returnUV = TRUE)
  durationSTR[i] <- proc.time()[3] - durationSTR[i]
}

#############################
### Remove unwanted stuff ###
#############################

rm(gHuRI,
   HuRI,
   p,
   temp)
rm(list = lsf.str())

#################
### Save data ###
#################

save.image("empirical10.RData")




