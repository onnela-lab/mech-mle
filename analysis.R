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

#################
### Load data ###
#################

root <- "~/Harvard/Research/3 Mechanistic Network Models/methods/round09/"
load(paste0(root,
            "graphs.RData"))
load(paste0(root,
            "graphsA.RData"))
load(paste0(root,
            "graphsB.RData"))
load(paste0(root,
            "method01.RData"))
load(paste0(root,
            "method01a.RData"))
load(paste0(root,
            "method01b.RData"))
load(paste0(root,
            "method02.RData"))
load(paste0(root,
            "method02a.RData"))
load(paste0(root,
            "method02b.RData"))
load(paste0(root,
            "method03.RData"))
load(paste0(root,
            "method03a.RData"))
load(paste0(root,
            "method03b.RData"))
load(paste0(root,
            "method05.RData"))
load(paste0(root,
            "method05a.RData"))
load(paste0(root,
            "method06.RData"))
load(paste0(root,
            "method06a.RData"))
load(paste0(root,
            "method06b.RData"))
load(paste0(root,
            "method11.RData"))
load(paste0(root,
            "method11a.RData"))
load(paste0(root,
            "method14.RData"))
load(paste0(root,
            "method14a.RData"))
load(paste0(root,
            "method14b.RData"))
load(paste0(root,
            "method15a.RData"))
load(paste0(root,
            "method18.RData"))
load(paste0(root,
            "method18a.RData"))
load(paste0(root,
            "method18b.RData"))
load(paste0(root,
            "nll.RData"))
# load(paste0(root,
#             "path.RData"))
# load(paste0(root,
#             "empirical05.RData"))
# load(paste0(root,
#             "empirical10.RData"))
# load(paste0(root,
#             "empirical15.RData"))
# load(paste0(root,
#             "empirical20.RData"))
# load(paste0(root,
#             "empirical25.RData"))
# load(paste0(root,
#             "empirical30.RData"))
# load(paste0(root,
#             "empirical35.RData"))
# load(paste0(root,
#             "empirical40.RData"))
# load(paste0(root,
#             "empirical45.RData"))
# load(paste0(root,
#             "empirical50.RData"))
# load(paste0(root,
#             "empirical55.RData"))
# load(paste0(root,
#             "empirical60.RData"))
# load(paste0(root,
#             "empirical65.RData"))
# load(paste0(root,
#             "empirical70.RData"))
# load(paste0(root,
#             "empirical75.RData"))
load(paste0(root,
            "synthetic10.RData"))

################
### Analysis ###
################

# Create uniform random variable
estUU <- matrix(data = runif(n = 600),
                nrow = 100,
                ncol = 6)

# Create matrix of estimates
filler <- matrix(data = NA,
                 nrow = 1e2,
                 ncol = 2)
est <- cbind(truthA[ ,6:7],
             truth[ ,6:7],
             truthB[ ,6:7],
             est18a[ ,1:2],
             est18[ ,1:2],
             est18b[ ,1:2],
             est06a[ ,6:7],
             est06[ ,6:7],
             est06b[ ,6:7],
             est15a[ ,6:7],
             filler,
             filler,
             est05a[ ,6:7],
             est05[ ,6:7],
             filler,
             est11a[ ,6:7],
             est11[ ,6:7],
             filler,
             est02a[ ,1:2],
             est02[ ,1:2],
             est02b[ ,1:2],
             est03a[ ,6:7],
             est03[ ,6:7],
             est03b[ ,6:7],
             est01a[ ,1:2],
             est01[ ,1:2],
             est01b[ ,1:2],
             est14a[ ,6:7],
             est14[ ,6:7],
             est14b[ ,6:7],
             estUU)
estEM <- cbind(est15a[ ,6:7],
               filler,
               filler,
               est15a[ ,13:14],
               filler,
               filler,
               est15a[ ,11:12],
               filler,
               filler,
               est05a[ ,6:7],
               est05[ ,6:7],
               filler,
               est05a[ ,13:14],
               est05[ ,13:14],
               filler,
               est05a[ ,11:12],
               est05[ ,11:12],
               filler,
               est11a[ ,6:7],
               est11[ ,6:7],
               filler,
               est11a[ ,13:14],
               est11[ ,13:14],
               filler,
               est11a[ ,11:12],
               est11[ ,11:12],
               filler,
               est03a[ ,6:7],
               est03[ ,6:7],
               est03b[ ,6:7],
               est03a[ ,13:14],
               est03[ ,13:14],
               est03b[ ,13:14],
               est03a[ ,11:12],
               est03[ ,11:12],
               est03b[ ,11:12],
               est14a[ ,6:7],
               est14[ ,6:7],
               est14b[ ,6:7],
               est14a[ ,13:14],
               est14[ ,13:14],
               est14b[ ,13:14],
               est14a[ ,11:12],
               est14[ ,11:12],
               est14b[ ,11:12])
tau <- cbind(truthA[ ,12:11],
             truth[ ,12:11],
             truthB[ ,12:11],
             est18a[ ,7:6],
             est18[ ,7:6],
             est18b[ ,7:6],
             est06a[ ,12:11],
             est06[ ,12:11],
             est06b[ ,12:11],
             est15a[ ,16:15],
             filler,
             filler,
             est05a[ ,16:15],
             est05[ ,16:15],
             filler,
             est11a[ ,16:15],
             est11[ ,16:15],
             filler,
             est02a[ ,7:6],
             est02[ ,7:6],
             est02b[ ,7:6],
             est03a[ ,16:15],
             est03[ ,16:15],
             est03b[ ,16:15],
             est01a[ ,7:6],
             est01[ ,7:6],
             est01b[ ,7:6],
             est14a[ ,16:15],
             est14[ ,16:15],
             est14b[ ,16:15])

# RMSE
rmse <- matrix(data = sqrt(colMeans(x = (est - matrix(data = truth[ ,1:2],
                                                      nrow = nrow(truth),
                                                      ncol = ncol(est)))^2,
                                    na.rm = TRUE)),
               nrow = 11,
               ncol = 6,
               byrow = TRUE)
rownames(rmse) <- c("Known Theta",
                    "Known New, Random Anchor",
                    "NK True Initial",
                    "Exhaustive",
                    "NK",
                    "NK+1",
                    "Minimize Y(u,v)",
                    "Minimize Y(u,v), then NK",
                    "1 Random",
                    "100 Random",
                    "Uniform RV")
colnames(rmse) <- c("QM 7 nodes",
                    "QC 7 nodes",
                    "QM 100 nodes",
                    "QC 100 nodes",
                    "QM 200 nodes",
                    "QC 200 nodes")
rmseEM <- matrix(data = sqrt(colMeans(x = (estEM - matrix(data = truth[ ,1:2],
                                                          nrow = nrow(truth),
                                                          ncol = ncol(estEM)))^2,
                                      na.rm = TRUE)),
                 nrow = 15,
                 ncol = 6,
                 byrow = TRUE)
rownames(rmseEM) <- c("Exhaustive Max",
                      "Exhaustive EM",
                      "Exhaustive Ave",
                      "NK Max",
                      "NK EM",
                      "NK Ave",
                      "NK + 1 Max",
                      "NK + 1 EM",
                      "NK + 1 Ave",
                      "Minimize Y(u,v), then NK Max",
                      "Minimize Y(u,v), then NK EM",
                      "Minimize Y(u,v), then NK Ave",
                      "100 Random Sequences Max",
                      "100 Random Sequences EM",
                      "100 Random Sequences Ave")
colnames(rmseEM) <- c("QM 7 nodes",
                      "QC 7 nodes",
                      "QM 100 nodes",
                      "QC 100 nodes",
                      "QM 200 nodes",
                      "QC 200 nodes")
worstPossible <- rep(x = 0,
                     times = length(truth[ ,1]))
worstPossible[truth[ ,1] <= 0.5] <- 1
sqrt(mean((worstPossible - truth[ ,1])^2))

# Kendall's Tau
tauAve <- matrix(data = colMeans(x = tau,
                                 na.rm = TRUE),
                 nrow = 10,
                 ncol = 6,
                 byrow = TRUE)
rownames(tauAve) <- c("Known Theta",
                      "Known New, Random Anchor",
                      "NK True Initial",
                      "Exhaustive",
                      "NK",
                      "NK+1",
                      "Minimize Y(u,v)",
                      "Minimize Y(u,v), then NK",
                      "1 Random",
                      "100 Random")
colnames(tauAve) <- c("Strict Tau 7 nodes",
                      "Lenient Tau 7 nodes",
                      "Strict Tau 100 nodes",
                      "Lenient Tau 100 nodes",
                      "Strict Tau 200 nodes",
                      "Lenient Tau 200 nodes")

# Number of times qm-hat is NA
na <- est[ ,seq(from = 1,
                to = ncol(est) - 1,
                by = 2)]
na <- matrix(data = colSums(is.na(na)),
             nrow = 11,
             ncol = 3,
             byrow = TRUE)
rownames(na) <- c("Known Theta",
                  "Known New, Random Anchor",
                  "NK True Initial",
                  "Exhaustive",
                  "NK",
                  "NK+1",
                  "Minimize Y(u,v)",
                  "Minimize Y(u,v), then NK",
                  "1 Random",
                  "100 Random",
                  "Uniform RV")
colnames(na) <- c("7 nodes",
                  "100 nodes",
                  "200 nodes")
naEM <- estEM[ ,seq(from = 1,
                    to = ncol(estEM) - 1,
                    by = 2)]
naEM <- matrix(data = colSums(is.na(naEM)),
               nrow = 15,
               ncol = 3,
               byrow = TRUE)
rownames(naEM) <- c("Exhaustive Max",
                    "Exhaustive EM",
                    "Exhaustive Ave",
                    "NK Max",
                    "NK EM",
                    "NK Ave",
                    "NK + 1 Max",
                    "NK + 1 EM",
                    "NK + 1 Ave",
                    "Minimize Y(u,v), then NK Max",
                    "Minimize Y(u,v), then NK EM",
                    "Minimize Y(u,v), then NK Ave",
                    "100 Random Sequences Max",
                    "100 Random Sequences EM",
                    "100 Random Sequences Ave")
colnames(naEM) <- c("7 nodes",
                    "100 nodes",
                    "200 nodes")

# Coverage
coverage <- est[ ,1:60]
sds <- cbind(truthA[ ,9:10],
             truth[ ,9:10],
             truthB[ ,9:10],
             est18a[ ,4:5],
             est18[ ,4:5],
             est18b[ ,4:5],
             est06a[ ,9:10],
             est06[ ,9:10],
             est06b[ ,9:10],
             est15a[ ,9:10],
             filler,
             filler,
             est05a[ ,9:10],
             est05[ ,9:10],
             filler,
             est11a[ ,9:10],
             est11[ ,9:10],
             filler,
             est02a[ ,4:5],
             est02[ ,4:5],
             est02b[ ,4:5],
             est03a[ ,9:10],
             est03[ ,9:10],
             est03b[ ,9:10],
             est01a[ ,4:5],
             est01[ ,4:5],
             est01b[ ,4:5],
             est14a[ ,9:10],
             est14[ ,9:10],
             est14b[ ,9:10])
test1 <- matrix(data = truth[ ,1:2],
                nrow = nrow(truth),
                ncol = ncol(coverage)) <= coverage + qnorm(p = 0.975) * sds
test2 <- matrix(data = truth[ ,1:2],
                nrow = nrow(truth),
                ncol = ncol(coverage)) >= coverage - qnorm(p = 0.975) * sds
coverage <- matrix(data = colMeans(x = test1 & test2,
                                   na.rm = TRUE),
                   nrow = 10,
                   ncol = 6,
                   byrow = TRUE)
rownames(coverage) <- c("Known Theta",
                        "Known New, Random Anchor",
                        "NK True Initial",
                        "Exhaustive",
                        "NK",
                        "NK+1",
                        "Minimize Y(u,v)",
                        "Minimize Y(u,v), then NK",
                        "1 Random",
                        "100 Random")
colnames(coverage) <- c("QM 7 nodes",
                        "QC 7 nodes",
                        "QM 100 nodes",
                        "QC 100 nodes",
                        "QM 200 nodes",
                        "QC 200 nodes")

# Duration
duration <- matrix(data = c(durationTruthA[3],
                            durationTruth[3],
                            durationTruthB[3],
                            durationMethod18a[3],
                            durationMethod18[3],
                            durationMethod18b[3],
                            durationMethod06a[3],
                            durationMethod06[3],
                            durationMethod06b[3],
                            durationMethod15a[3],
                            NA,
                            NA,
                            durationMethod05a[3],
                            durationMethod05[3],
                            NA,
                            durationMethod11a[3],
                            durationMethod11[3],
                            NA,
                            durationMethod02a[3],
                            durationMethod02[3],
                            durationMethod02b[3],
                            durationMethod03a[3],
                            durationMethod03[3],
                            durationMethod03b[3],
                            durationMethod01a[3],
                            durationMethod01[3],
                            durationMethod01b[3],
                            durationMethod14a[3],
                            durationMethod14[3],
                            durationMethod14b[3]),
                   nrow = 10,
                   ncol = 3,
                   byrow = TRUE) / 60 / 60
rownames(duration) <- c("Known Theta",
                        "Known New, Random Anchor",
                        "NK True Initial",
                        "Exhaustive",
                        "NK",
                        "NK+1",
                        "Minimize Y(u,v)",
                        "Minimize Y(u,v), then NK",
                        "1 Random",
                        "100 Random")
colnames(duration) <- c("7 nodes",
                        "100 nodes",
                        "200 nodes")

# Table of missingness, 0s and 1s, and likelihood bias
overfit <- cbind(na[1:(nrow(na) - 1),1],
                 c(sum(truthA[ ,6] %in% 0:1),
                   sum(est18a[ ,1] %in% 0:1),
                   sum(est06a[ ,6] %in% 0:1),
                   sum(est15a[ ,6] %in% 0:1),
                   sum(est05a[ ,6] %in% 0:1),
                   sum(est11a[ ,6] %in% 0:1),
                   sum(est02a[ ,1] %in% 0:1),
                   sum(est03a[ ,6] %in% 0:1),
                   sum(est01a[ ,1] %in% 0:1),
                   sum(est14a[ ,6] %in% 0:1)),
                 c(sum(truthA[ ,7] %in% 0:1),
                   sum(est18a[ ,2] %in% 0:1),
                   sum(est06a[ ,7] %in% 0:1),
                   sum(est15a[ ,7] %in% 0:1),
                   sum(est05a[ ,7] %in% 0:1),
                   sum(est11a[ ,7] %in% 0:1),
                   sum(est02a[ ,2] %in% 0:1),
                   sum(est03a[ ,7] %in% 0:1),
                   sum(est01a[ ,2] %in% 0:1),
                   sum(est14a[ ,7] %in% 0:1)),
                 colMeans(cbind(truthA[ ,8],
                                est18a[ ,3],
                                est06a[ ,8],
                                est15a[ ,8],
                                est05a[ ,8],
                                est11a[ ,8],
                                est02a[ ,3],
                                est03a[ ,8],
                                est01a[ ,3],
                                est14a[ ,8]) - truthA[ ,3]))
colnames(overfit) <- c("Missing",
                       "qmHat 0 or 1",
                       "qcHat 0 or 1",
                       "Likelihood Bias")

# # Empirical graph
# empTable <- matrix(data = c(gorder(gHuRI05),
#                             gsize(gHuRI05),
#                             resultHuRI05[1:2],
#                             durationHuRI05[3] / 60 / 60,
#                             gorder(gHuRI10),
#                             gsize(gHuRI10),
#                             resultHuRI10[1:2],
#                             durationHuRI10[3] / 60 / 60,
#                             gorder(gHuRI15),
#                             gsize(gHuRI15),
#                             resultHuRI15[1:2],
#                             durationHuRI15[3] / 60 / 60,
#                             gorder(gHuRI20),
#                             gsize(gHuRI20),
#                             resultHuRI20[1:2],
#                             durationHuRI20[3] / 60 / 60,
#                             gorder(gHuRI25),
#                             gsize(gHuRI25),
#                             resultHuRI25[1:2],
#                             durationHuRI25[3] / 60 / 60,
#                             gorder(gHuRI30),
#                             gsize(gHuRI30),
#                             resultHuRI30[1:2],
#                             durationHuRI30[3] / 60 / 60,
#                             gorder(gHuRI35),
#                             gsize(gHuRI35),
#                             resultHuRI35[1:2],
#                             durationHuRI35[3] / 60 / 60,
#                             gorder(gHuRI40),
#                             gsize(gHuRI40),
#                             resultHuRI40[1:2],
#                             durationHuRI40[3] / 60 / 60,
#                             gorder(gHuRI45),
#                             gsize(gHuRI45),
#                             resultHuRI45[1:2],
#                             durationHuRI45[3] / 60 / 60,
#                             gorder(gHuRI50),
#                             gsize(gHuRI50),
#                             resultHuRI50[1:2],
#                             durationHuRI50[3] / 60 / 60,
#                             gorder(gHuRI55),
#                             gsize(gHuRI55),
#                             resultHuRI55[1:2],
#                             durationHuRI55[3] / 60 / 60,
#                             gorder(gHuRI60),
#                             gsize(gHuRI60),
#                             resultHuRI60[1:2],
#                             durationHuRI60[3] / 60 / 60,
#                             gorder(gHuRI65),
#                             gsize(gHuRI65),
#                             resultHuRI65[1:2],
#                             durationHuRI65[3] / 60 / 60,
#                             gorder(gHuRI70),
#                             gsize(gHuRI70),
#                             resultHuRI70[1:2],
#                             durationHuRI70[3] / 60 / 60,
#                             gorder(gHuRI75),
#                             gsize(gHuRI75),
#                             resultHuRI75[1:2],
#                             durationHuRI75[3] / 60 / 60),
#                    nrow = 15,
#                    ncol = 5,
#                    byrow = TRUE)
# empTable <- cbind(seq(from = 0.05,
#                       to = 0.75,
#                       by = 0.05),
#                   empTable)
# colnames(empTable) <- c("p",
#                         "# Nodes",
#                         "# Edges",
#                         "q_m Hat",
#                         "q_c Hat",
#                         "Hours")
# round(x = empTable,
#       digits = 3)

# # Get info for Python stuff
# suffixes <- c("05",
#               "10",
#               "15",
#               "20",
#               "25",
#               "30",
#               "35",
#               "40",
#               "45",
#               "50",
#               "55",
#               "60",
#               "65",
#               "70",
#               "75",
#               "80",
#               "85",
#               "90")
# gsizes <- rep(x = NA,
#               times = length(suffixes))
# for(i in 1:length(suffixes)){
#   temp <- read.table(file = paste0("python",
#                                    suffixes[i],
#                                    ".tsv"),
#                      sep = '\t',
#                      stringsAsFactors = FALSE)
#   class(temp$V1) <- "character"
#   class(temp$V2) <- "character"
#   gTemp <- simplify(graph_from_edgelist(el = as.matrix(temp),
#                                         directed = FALSE))
#   gsizes[i] <- gsize(gTemp)
# }
# gsizes

#############
### Plots ###
#############

# # Empirical graph duration as a function of proportion of nodes sampled
# plot(x = empTable[ ,1],
#      y = empTable[ ,6],
#      type = "l",
#      xlab = "p",
#      ylab = "duration")
# plot(x = empTable[ ,1],
#      y = empTable[ ,6],
#      type = "l",
#      xlab = "p",
#      ylab = "duration",
#      log = "xy")
# mod <- lm(formula = log(empTable[ ,6]) ~ log(empTable[ ,1]))
# exp(mod$coefficients[1]) / 24
# rm(mod)

# Likelihood as a function of estimates
qm <- c(1 / 3,
        1 / 3,
        2 / 3,
        2 / 3)
qc <- c(1 / 3,
        2 / 3,
        1 / 3,
        2 / 3)
placement <- c("topright",
               "bottomright",
               "topleft",
               "bottomleft")
pdf(file = "nll.pdf",
    width = 8.5,
    height = 5.25)
old.par <- par()
par(mfrow = c(2,2),
    mar = c(4.5,
            5,
            0.5,
            0.5))
for(i in c(2,4,1,3)){
  mFitAll[[i]] <- as.data.frame(mFitAll[[i]])
  mFitAll[[i]] <- mFitAll[[i]][!duplicated(mFitAll[[i]]), ]
  mFitAll[[i]] <- mFitAll[[i]][order(mFitAll[[i]]$negLL), ]
  uNegLL <- unique(mFitAll[[i]]$negLL)
  temp <- data.frame(negLL = uNegLL,
                     color = rev(matlab.like(length(uNegLL))),
                     stringsAsFactors = FALSE)
  mFitAll[[i]] <- merge(x = mFitAll[[i]],
                        y = temp)
  these <- c(1,
             round(c(0.25,
                     0.5,
                     0.75,
                     1) * nrow(mFitAll[[i]])))
  plot(x = mFitAll[[i]]$qmHat,
       y = mFitAll[[i]]$qcHat,
       xlim = c(0.25,1),
       ylim = c(0,1),
       pch = 16,
       col = mFitAll[[i]]$color,
       cex = 1,
       xlab = expression(hat(q)[m]),
       ylab = expression(hat(q)[c]))
  legend(x = placement[i],
         legend = -round(mFitAll[[i]]$negLL[these]),
         fill = mFitAll[[i]]$color[these],
         bty = "n")
  abline(h = qc[i],
         v = qm[i])
}
par(old.par)
dev.off()
rm(qm,
   qc,
   placement,
   i,
   uNegLL,
   temp)

# Error as a function of true qm and qc
x <- sort(unique(truthB[ ,1]))
y <- sort(unique(truthB[ ,2]))
z <- list()
z[[1]] <- matrix(data = NA,
                 nrow = length(x),
                 ncol = length(y))
z[[2]] <- matrix(data = NA,
                 nrow = length(x),
                 ncol = length(y))
imageTitle <- c(expression(paste("|",
                                 hat(q)[m] - q[m],
                                 "|")),
                expression(paste("|",
                                 hat(q)[c] - q[c],
                                 "|")))
pdf(file = "error.pdf",
    width = 8.5,
    height = 11)
old.par <- par()
par(mfrow = c(2,1))
for(k in 1:2){
  error <- abs(est02b[ ,k] - truthB[ ,k])
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      z[[k]][i,j] <- error[truthB[ ,1] == x[i] & truthB[ ,2] == y[j]]
    }
  }
  image.plot(x = x,
             y = y,
             z = z[[k]],
             xlab = expression(q[m]),
             ylab = expression(q[c]))
#             main = imageTitle[k])
}
par(old.par)
dev.off()
rm(i,
   j,
   k)

# Empirical and synthetic graphs
n <- gorder(gSynth10[[1]])
estOrder <- rev(swapUV(u = resultHuRI10[8:(n + 7)],
                       v = resultHuRI10[(n + 8):(2 * n + 7)]))
estOrder[estOrder == 1] <- V(gHuRI10)$name[!(V(gHuRI10)$name %in% estOrder)]
whichColor <- rep(x = NA,
                  times = n)
for(i in 1:n){
  whichColor[i] <- which(estOrder == as.numeric(V(gHuRI10)$name[i]))
}
cor.test(x = whichColor,
         y = degree(gHuRI10))
cor.test(x = 1:n,
         y = degree(gSynth10[[1]]))
coords <- layout_(graph = gSynth10[[1]],
                  layout = nicely())
pdf(file = "graphs.pdf",
    width = 8.5,
    height = 8.5)
old.par <- par()
par(mfrow = c(2,2),
    mar = c(4.5,
            4.1,
            0.5,
            0.5))
plot(gHuRI10,
     vertex.size = 2,
     vertex.label = NA,
     vertex.color = matlab.like(n)[whichColor])
legend(x = "bottom",
       fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
       legend = c("1",
                  "n/4",
                  "n/2",
                  "3n/4",
                  "n"),
       bty = "n",
       horiz = TRUE,
       inset = c(0,-0.1))
text(x = -1,
     y = 1,
     labels = "A",
     cex = 2)
plot(gSynth10[[1]],
     layout = coords,
     vertex.size = 2,
     vertex.label = NA,
     vertex.color = matlab.like(n))
legend(x = "bottom",
       fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
       legend = c("1",
                  "n/4",
                  "n/2",
                  "3n/4",
                  "n"),
       bty = "n",
       horiz = TRUE,
       inset = c(0,-0.1))
text(x = -1,
     y = 1,
     labels = "B",
     cex = 2)
ddHuRI10 <- 1 - cumsum(degree_distribution(gHuRI10))
ddSynth10 <- 1 - cumsum(degree_distribution(gSynth10[[1]]))
ddSynth10 <- c(ddSynth10,
               rep(x = 0,
                   times = length(ddHuRI10) - length(ddSynth10)))
ddHuRI10[ddHuRI10 == 0] <- NA
ddSynth10[ddSynth10 == 0] <- NA
par(mar = c(4.5,
            4.1,
            2,
            0.5))
plot(x = 1:length(ddHuRI10),
     y = ddHuRI10,
     log = "xy",
     xlab = "k",
     ylab = "Proportion of Nodes with Degree >= k",
     pch = 1,
     ylim = c(min(c(ddHuRI10,
                    ddSynth10),
                  na.rm = TRUE),
              max(c(ddHuRI10,
                    ddSynth10),
                  na.rm = TRUE)))
points(x = 1:length(ddSynth10),
       y = ddSynth10,
       pch = 2)
legend(x = "topright",
       pch = 1:2,
       legend = c("Observed Graph",
                  "Synthetic Graph"))
text(x = 1,
     y = 1,
     labels = "C",
     cex = 2)
estOrder <- rev(swapUV(u = resultSynth10[8:(n + 7)],
                       v = resultSynth10[(n + 8):(2 * n + 7)]))
par(mar = c(4.5,
            4.1,
            0.5,
            0.5))
plot(gSynth10[[1]],
     layout = coords,
     vertex.size = 2,
     vertex.label = NA,
     vertex.color = matlab.like(n)[order(estOrder)])
legend(x = "bottom",
       fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
       legend = c("1",
                  "n/4",
                  "n/2",
                  "3n/4",
                  "n"),
       bty = "n",
       horiz = TRUE,
       inset = c(0,-0.1))
text(x = -1,
     y = 1,
     labels = "D",
     cex = 2)
par(old.par)
dev.off()
cor.test(x = order(estOrder),
         y = degree(gSynth10[[1]]))

# Additional organism plots
ddSTR <- list()
ddSynthSTR <- list()
for(j in 1:length(synthSTR)){
  # Empirical and synthetic graphs
  n <- gorder(synthSTR[[j]][[1]])
  estOrder <- rev(swapUV(u = resultSTR[[j]][8:(n + 7)],
                         v = resultSTR[[j]][(n + 8):(2 * n + 7)]))
  whichColor <- rep(x = NA,
                    times = n)
  for(i in 1:n){
    whichColor[i] <- which(estOrder == as.numeric(V(gSTR[[j]])$name[i]))
  }
  cor.test(x = whichColor,
           y = degree(gSTR[[j]]))
  cor.test(x = 1:n,
           y = degree(synthSTR[[j]][[1]]))
  coords <- layout_(graph = synthSTR[[j]][[1]],
                    layout = nicely())
  pdf(file = paste0("graphs",
                    j,
                    ".pdf"),
      width = 8.5,
      height = 8.5)
  old.par <- par()
  par(mfrow = c(2,2),
      mar = c(4.5,
              4.1,
              0.5,
              0.5))
  plot(gSTR[[j]],
       vertex.size = 2,
       vertex.label = NA,
       vertex.color = matlab.like(n)[whichColor])
  legend(x = "bottom",
         fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
         legend = c("1",
                    "n/4",
                    "n/2",
                    "3n/4",
                    "n"),
         bty = "n",
         horiz = TRUE,
         inset = c(0,-0.1))
  text(x = -1,
       y = 1,
       labels = "A",
       cex = 2)
  plot(synthSTR[[j]][[1]],
       layout = coords,
       vertex.size = 2,
       vertex.label = NA,
       vertex.color = matlab.like(n))
  legend(x = "bottom",
         fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
         legend = c("1",
                    "n/4",
                    "n/2",
                    "3n/4",
                    "n"),
         bty = "n",
         horiz = TRUE,
         inset = c(0,-0.1))
  text(x = -1,
       y = 1,
       labels = "B",
       cex = 2)
  ddSTR[[j]] <- 1 - cumsum(degree_distribution(gSTR[[j]]))
  ddSynthSTR[[j]] <- 1 - cumsum(degree_distribution(synthSTR[[j]][[1]]))
  a <- length(ddSTR[[j]])
  b <- length(ddSynthSTR[[j]])
  if(a > b){
    ddSynthSTR[[j]] <- c(ddSynthSTR[[j]],
                         rep(x = 0,
                             times = a - b))
  } else if(a < b){
    ddSTR[[j]] <- c(ddSTR[[j]],
                    rep(x = 0,
                        times = b - a))
  }
  rm(a,b)
  ddSTR[[j]][ddSTR[[j]] <= .Machine$double.eps] <- NA
  ddSynthSTR[[j]][ddSynthSTR[[j]] == 0] <- NA
  par(mar = c(4.5,
              4.1,
              2,
              0.5))
  plot(x = 1:length(ddSTR[[j]]),
       y = ddSTR[[j]],
       log = "xy",
       xlab = "k",
       ylab = "Proportion of Nodes with Degree >= k",
       pch = 1,
       ylim = c(0.0009,1))
  points(x = 1:length(ddSynthSTR[[j]]),
         y = ddSynthSTR[[j]],
         pch = 2)
  legend(x = "bottomleft",
         pch = 1:2,
         legend = c("Observed Graph",
                    "Synthetic Graph"))
  text(x = 1,
       y = 2,
       labels = "C",
       cex = 2)
  estOrder <- rev(swapUV(u = resultSynthSTR[[j]][8:(n + 7)],
                         v = resultSynthSTR[[j]][(n + 8):(2 * n + 7)]))
  par(mar = c(4.5,
              4.1,
              0.5,
              0.5))
  plot(synthSTR[[j]][[1]],
       layout = coords,
       vertex.size = 2,
       vertex.label = NA,
       vertex.color = matlab.like(n)[order(estOrder)])
  legend(x = "bottom",
         fill = matlab.like(n)[c(1,round(c(0.25,0.5,0.75,1) * n))],
         legend = c("1",
                    "n/4",
                    "n/2",
                    "3n/4",
                    "n"),
         bty = "n",
         horiz = TRUE,
         inset = c(0,-0.1))
  text(x = -1,
       y = 1,
       labels = "D",
       cex = 2)
  par(old.par)
  dev.off()
  cor.test(x = order(estOrder),
           y = degree(synthSTR[[j]][[1]]))
}

