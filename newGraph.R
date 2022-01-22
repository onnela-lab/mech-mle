#####################
### Load packages ###
#####################

library(igraph)
library(colorRamps)

library(combinat)
library(VGAM)
library(fields)

##################
### Parameters ###
##################

set.seed(2)




#################
### Functions ###
#################

source("functions.R")

###########
### Key ###
###########

# True Path   Correct backward pathway
# 18          Known node age but random anchor nodes
# 06          NK with true initial values
# 15          Exhaustive search
# 05          NK until maximum reached
# 11          NK until maximum reached then one more iteration
# 02          Single minUni
# 03          minUni followed by NK until maximum reached
# 01          Single random path
# 14          100 random paths
# U           Uniform random variable

# 16          Minimize shared neighbors
# 17          Minimize unique neighbors
# 19          Maximize shared neighbors
# 20          Choose unconnected anchor and new nodes
# 21          Choose connected anchor and new nodes

# Max         Maximum likelihood
# Ave         Average
# EM          Expectation Maximization Algorithm

# 100 nodes
# 7 nodes (followed by "a")
# 200 nodes (followed by "b")
# nodes (followed by "c") # JP to change

#################
### Load data ###
#################

root <- "~/Harvard/Research/3 Mechanistic Network Models/methods/round09/" # JP to change
load(paste0(root,
            "graphsC.RData"))
load(paste0(root,
            "method02c.RData"))

################
### Analysis ###
################



#############
### Plots ###
#############

# Error as a function of true qm and qc
x <- sort(unique(truthC[ ,1]))
y <- sort(unique(truthC[ ,2]))
z <- list()
z[[1]] <- matrix(data = NA,
                 nrow = length(x),
                 ncol = length(y))
z[[2]] <- matrix(data = NA,
                 nrow = length(x),
                 ncol = length(y))
pdf(file = "draft09error.pdf",
    width = 8.5,
    height = 11)
old.par <- par()
par(mfrow = c(2,1))
for(k in 1:2){
  error <- abs(est02c[ ,k] - truthC[ ,k])
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      z[[k]][i,j] <- error[truthC[ ,1] == x[i] & truthC[ ,2] == y[j]]
    }
  }
  image.plot(x = x,
             y = y,
             z = z[[k]],
             xlab = expression(q[m]),
             ylab = expression(q[c]))
}
par(old.par)
dev.off()
rm(i,
   j,
   k)



