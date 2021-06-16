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

nGraphs <- 4
nNodes <- 1e2

#################
### Functions ###
#################

source("functions.R")

##################
### Simulation ###
##################

# Parameters
sNGraphs <- sqrt(nGraphs)
qm <- rep(x = 1:sNGraphs / (sNGraphs + 1),
          each = sNGraphs)
qc <- rep(x = 1:sNGraphs / (sNGraphs + 1),
          times = sNGraphs)

mFitAll <- list()

for(i in 1:length(qm)){
  # Generate graph
  gNLL <- dmc(n = nNodes,
              qm = qm[i],
              qc = qc[i])
  
  # Run true path
  mTruth <- deconstruct(g = gNLL[[1]],
                        init = c(qm[i],
                                 qc[i]),
                        u = gNLL[[2]],
                        returnUV = TRUE)
  
  # Known node age but random anchor nodes
  m18 <- deconstruct(g = gNLL[[1]],
                     u = randAnc(n = gorder(gNLL[[1]])),
                     returnUV = TRUE)
  
  # NK with true initial values
  m06 <- deconstruct(g = gNLL[[1]],
                     method = "NLL",
                     init = c(qm[i],
                              qc[i]),
                     returnUV = TRUE)
  
  # NK until maximum reached
  m05 <- deconSearch(g = gNLL[[1]],
                     method = "NLL",
                     numPerSide = 10,
                     returnAll = TRUE)
  
  # Single minUni
  m02 <- deconstruct(g = gNLL[[1]],
                     method = "minUni",
                     returnUV = TRUE)
  
  # Minimize shared neighbors
  m16 <- deconstruct(g = gNLL[[1]],
                     method = "minInt",
                     returnUV = TRUE)
  
  # Minimize unique neighbors
  m17 <- deconstruct(g = gNLL[[1]],
                     method = "minUniMinusInt",
                     returnUV = TRUE)
  
  # minUni followed by NK until maximum reached
  m03 <- deconSearch(g = gNLL[[1]],
                     method = "minUni",
                     returnAll = TRUE)
  
  # 100 random paths
  m14 <- deconSearch(g = gNLL[[1]],
                     method = "random",
                     iter = 1e2,
                     returnAll = TRUE)
  
  # Make plot
  mFit <- rbind(mTruth[c(6:8,11:12)],
                m18[c(1:3,6:7)],
                m06[c(6:8,11:12)],
                refineRaw(m = m05,
                          n = nNodes),
                m02[c(1:3,6:7)],
                m16[c(1:3,6:7)],
                m17[c(1:3,6:7)],
                refineRaw(m = m03,
                          n = nNodes),
                refineRaw(m = m14,
                          n = nNodes))
  mFit <- mFit[!duplicated(mFit), ]
  mFit <- mFit[order(mFit[ ,3]), ]
  colorFit <- rev(matlab.like(nrow(mFit)))
  pdf(file = paste0("nll",
                    i,
                    ".pdf"),
      width = 8.5,
      height = 5.25)
  plot(x = mFit[ ,1],
       y = mFit[ ,2],
       xlab = "Fitted qm",
       ylab = "Fitted qc",
       pch = 16,
       col = colorFit,
       cex = 3)
  abline(v = qm[i],
         h = qc[i])
  legend(x = "topright",
         legend = c("Highest Likelihood",
                    "Lowest Likelihood"),
         fill = c(colorFit[1],
                  colorFit[length(colorFit)]))
  dev.off()
  
  pdf(file = paste0("kt",
                    i,
                    ".pdf"),
      width = 8.5,
      height = 5.25)
  plot(x = mFit[ ,5],
       y = mFit[ ,3],
       xlab = "Kendall's Tau",
       ylab = "Negative Log-Likelihood")
  dev.off()
  
  mFitAll[[i]] <- mFit
}

#############################
### Remove unwanted stuff ###
#############################

rm(list = lsf.str())

#################
### Save data ###
#################

save.image("nll.RData")




