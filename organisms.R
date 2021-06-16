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

root <- c("caen",
          "dros",
          "esch",
          "homo",
          "musm",
          "sacc")
for(i in 1:length(root)){
  for(suffix in c("a","p")){
    temp <- read.table(file = paste0(root[i],
                                     suffix,
                                     ".txt"),
                       sep = ' ',
                       stringsAsFactors = FALSE,
                       header = TRUE)
    temp <- temp[temp[ ,3] > 700,1:2]
    write.table(x = temp,
                file = paste0(root[i],
                              suffix,
                              ".tsv"),
                quote = FALSE,
                sep = "\t",
                row.names = FALSE,
                col.names = FALSE)
    temp <- simplify(graph_from_edgelist(el = as.matrix(temp),
                                         directed = FALSE))
    print(paste0(root[i],
                 suffix,
                 " has ",
                 gorder(temp),
                 " nodes and ",
                 gsize(temp),
                 " edges."))
  }
}

#############################
### Remove unwanted stuff ###
#############################

rm(list = lsf.str())

#################
### Save data ###
#################






