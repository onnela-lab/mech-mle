#####################
### Load packages ###
#####################

library(igraph)


library(combinat)
library(VGAM)

#################
### Functions ###
#################

dmc <- function(n,
                qm,
                qc,
                seed = NULL){
  if(is.null(seed)){
    g <- make_empty_graph(n = n,
                          directed = FALSE)
    startHere <- 2
    V(g)$name <- as.character(1:n)
    
    # u is the anchor node
    # v is the new node
    u <- rep(x = as.character(NA),
             times = n)
  } else {
    g <- seed[[1]]
    currentN <- gorder(g)
    startHere <- currentN + 1
    g <- add_vertices(graph = g,
                      nv = n - currentN,
                      name = as.character(startHere:n))
    u <- c(seed[[2]],
           rep(x = as.character(NA),
               times = n - length(seed[[2]])))
  }
  
  if(n >= startHere){
    for(v in startHere:n){
      u[v] <- sample(x = V(g)$name[1:(v - 1)],
                     size = 1)
      neigh <- names(neighbors(graph = g,
                               v = u[v]))
      if(length(neigh) > 0){
        for(w in neigh){
          g <- add_edges(graph = g,
                         edges = c(as.character(v),
                                   w))
          if(runif(1) <= qm){
            r <- sample(x = c(u[v],
                              as.character(v)),
                        size = 1)
            g <- delete_edges(graph = g,
                              edges = paste0(r,
                                             "|",
                                             w))
          }
        }
      }
      if(runif(1) <= qc){
        g <- add_edges(graph = g,
                       edges = c(as.character(v),
                                 u[v]))
      }
    }
  }
  return(list(g,u))
}

getNegLL <- function(con,
                     int,
                     uni,
                     n,
                     init = NULL){
  if(is.null(init)){
    qmEst <- 1 - int / uni
    qcEst <- con / (n - 1)
  } else if(class(init) == "matrix"){
    qmEst <- init[ ,1]
    qcEst <- init[ ,2]
  } else if(class(init) == "numeric"){
    qmEst <- rep(x = init[1],
                 times = length(int))
    qcEst <- rep(x = init[2],
                 times = length(con))
  }
  term1 <- rep(x = 0,
               times = length(con))
  term2 <- rep(x = 0,
               times = length(con))
  term3 <- rep(x = 0,
               times = length(int))
  term4 <- rep(x = 0,
               times = length(int))
  
  useThese <- con != 0
  term1[useThese] <- con[useThese] * log(qcEst[useThese])
  useThese <- con != n - 1
  term2[useThese] <- (n - 1 - con[useThese]) * log(1 - qcEst[useThese])
  
  useThese <- int != 0
  term3[useThese] <- int[useThese] * log(1 - qmEst[useThese])
  useThese <- int != uni
  term4[useThese] <- (uni[useThese] - int[useThese]) * log(qmEst[useThese])
  
  logLik <- term1 + term2 + term3 + term4
  
  return(-logLik)
}

getValue <- function(uv,
                     g,
                     method,
                     init = NULL){
  neighV <- names(neighbors(graph = g,
                            v = uv[1]))
  neighU <- names(neighbors(graph = g,
                            v = uv[2]))
  if(uv[1] %in% neighU){
    con <- 1
  } else {
    con <- 0
  }
  int <- length(intersect(neighV,
                          neighU))
  uni <- length(union(neighV,
                      neighU)) - 2 * con
  if(method == "minUni"){
    value <- uni
  } else if(method == "NLL") {
    value <- getNegLL(con = con,
                      int = int,
                      uni = uni,
                      n = 2,
                      init = init)
  } else if(method == "minInt"){
    value <- int
  } else if(method == "maxInt"){
    value <- -int
  } else if(method == "minUniMinusInt"){
    value <- uni - int
  } else if(method == "minCon"){
    value <- con
  } else if(method == "maxCon"){
    value <- -con
  }
  
  return(value)
}

sortChar <- function(x){
  x <- as.character(sort(as.numeric(x)))
  return(x)
}

randMin <- function(x){
  indices <- which(x == min(x))
  if(length(indices) == 1){
    return(indices)
  } else {
    return(sample(x = indices,
                  size = 1))
  }
}

swapUV <- function(u,v){
  u <- as.numeric(u)
  v <- as.numeric(v)
  n <- length(v)
  
  for(i in 1:(n - 1)){
    if(runif(1) <= 0.5){
      oldV <- v[i]
      oldU <- u[i]
      
      v[i:n][v[i:n] == oldV] <- -1
      v[i:n][v[i:n] == oldU] <- oldV
      v[i:n][v[i:n] == -1] <- oldU
      
      u[i:n][u[i:n] == oldV] <- -1
      u[i:n][u[i:n] == oldU] <- oldV
      u[i:n][u[i:n] == -1] <- oldU
    }
  }
  return(v)
}

deconstruct <- function(g,
                        method = NULL,
                        init = NULL,
                        u = NULL,
                        trace = FALSE,
                        returnUV = FALSE,
                        v = NULL,
                        printProgress = FALSE,
                        returnRaw = FALSE){
  uNULL <- is.null(u)
  vNULL <- is.null(v)
  n <- gorder(g)
  con <- rep(x = 0,
             times = n - 1)
  int <- rep(x = 0,
             times = n - 1)
  uni <- rep(x = 0,
             times = n - 1)
  
  if(uNULL){
    v <- rep(x = as.character(NA),
             times = n)
    u <- rep(x = as.character(NA),
             times = n)
    if(method == "random"){
      uv <- sample(x = names(V(g)),
                   size = 2)
      uv <- sortChar(uv)
    } else {
      obj <- as.data.frame(x = t(combn(x = names(V(g)),
                                       m = 2)),
                           stringsAsFactors = FALSE)
      obj$value <- as.numeric(NA)
      obj$value <- apply(X = obj[ ,1:2],
                         MARGIN = 1,
                         FUN = getValue,
                         g = g,
                         method = method,
                         init = init)
      uv <- as.character(obj[randMin(obj$value),1:2])
      uv <- sortChar(uv)
    }
    u[1] <- uv[1]
    v[1] <- uv[2]
    
  } else {
    u <- rev(as.character(u))
    if(vNULL){
      v <- as.character(n:1)
    } else {
      v <- rev(as.character(v))
    }
  }
  
  for(i in 1:(n - 1)){
    if(printProgress){
      print(paste("Starting",i))
    }
    if(u[i] %in% names(neighbors(graph = g,
                                 v = v[i]))){
      con[i] <- 1
      g <- delete_edges(graph = g,
                        edges = paste0(u[i],
                                       "|",
                                       v[i]))
    }
    neighV <- names(neighbors(graph = g,
                              v = v[i]))
    neighU <- names(neighbors(graph = g,
                              v = u[i]))
    int[i] <- length(intersect(neighV,
                               neighU))
    uni[i] <- length(union(neighV,
                           neighU))
    justV <- neighV[!(neighV %in% neighU)]
    if(length(justV) > 0){
      for(j in justV){
        g <- add_edges(graph = g,
                       edges = c(u[i],
                                 j))
      }
    }
    g <- delete_vertices(graph = g,
                         v = v[i])
    if(is.na(v[i + 1]) & i < (n - 1)){
      if(method == "random"){
        uv <- sample(x = names(V(g)),
                     size = 2)
        uv <- sortChar(uv)
      } else {
        obj <- obj[!(obj$V1 == v[i] | obj$V2 == v[i]), ]
        theseChanged <- c(u[i],
                          names(neighbors(graph = g,
                                          v = u[i])))
        changeThese <- (obj$V1 %in% theseChanged) | (obj$V2 %in% theseChanged)
        if(sum(changeThese) != 0){
          obj$value[changeThese] <- apply(X = obj[changeThese,1:2],
                                          MARGIN = 1,
                                          FUN = getValue,
                                          g = g,
                                          method = method,
                                          init = init)
        }
        uv <- as.character(obj[randMin(obj$value),1:2])
        uv <- sortChar(uv)
      }
      u[i + 1] <- uv[1]
      v[i + 1] <- uv[2]
      
    }
  }
  
  if(is.na(v[n])){
    v[n] <- as.character(1:n)[!(as.character(1:n) %in% v)]
  }
  
  if(trace){
    kt1 <- kendall.tau(x = n:1,
                       y = as.numeric(v),
                       exact = TRUE,
                       max.n = n)
    kt2 <- kendall.tau(x = n:1,
                       y = swapUV(u = u,
                                  v = v),
                       exact = TRUE,
                       max.n = n)
    qmHat <- (cumsum(uni) - cumsum(int)) / cumsum(uni)
    qcHat <- cumsum(con) / (1:(n - 1))
    qmSD <- sqrt(qmHat * (1 - qmHat) / cumsum(uni))
    qcSD <- sqrt(qcHat * (1 - qcHat) / (1:(n - 1)))
    return(list(qmHat,
                qcHat,
                qmSD,
                qcSD,
                kt1,
                kt2))
  } else if(returnRaw){
    if(is.null(init)){
      init <- c(NA,NA)
    }
    s <- c(init,
           sum(con),
           sum(int),
           sum(uni))
    if(returnUV){
      return(c(s,
               as.numeric(u),
               as.numeric(v)))
    } else {
      return(s)
    }
  } else {
    kt1 <- kendall.tau(x = n:1,
                       y = as.numeric(v),
                       exact = TRUE,
                       max.n = n)
    kt2 <- kendall.tau(x = n:1,
                       y = swapUV(u = u,
                                  v = v),
                       exact = TRUE,
                       max.n = n)
    qmHat <- (sum(uni) - sum(int)) / sum(uni)
    qcHat <- sum(con) / (n - 1)
    negLogLik <- getNegLL(con = sum(con),
                          int = sum(int),
                          uni = sum(uni),
                          n = n)
    qmSD <- sqrt(qmHat * (1 - qmHat) / sum(uni))
    qcSD <- sqrt(qcHat * (1 - qcHat) / (n - 1))
    if(is.null(init)){
      s <- c(qmHat,
             qcHat,
             negLogLik,
             qmSD,
             qcSD,
             kt1,
             kt2)
    } else {
      s <- c(init,
             getNegLL(con = sum(con),
                      int = sum(int),
                      uni = sum(uni),
                      n = n,
                      init = init),
             sqrt(init[1] * (1 - init[1]) / sum(uni)),
             sqrt(init[2] * (1 - init[2]) / (n - 1)),
             qmHat,
             qcHat,
             negLogLik,
             qmSD,
             qcSD,
             kt1,
             kt2)
    }
    if(returnUV){
      return(c(s,
               as.numeric(u),
               as.numeric(v)))
    } else {
      return(s)
    }
  }
}

getCoords <- function(center,
                      gap,
                      numPerSide){
  vec1 <- seq(from = center[1] - gap * (numPerSide - 1) / 2,
              to = center[1] + gap * (numPerSide - 1) / 2,
              length.out = numPerSide)
  vec1[vec1 < 0] <- 0
  vec1[vec1 > 1] <- 1
  vec2 <- seq(from = center[2] - gap * (numPerSide - 1) / 2,
              to = center[2] + gap * (numPerSide - 1) / 2,
              length.out = numPerSide)
  vec2[vec2 < 0] <- 0
  vec2[vec2 > 1] <- 1
  m <- matrix(data = NA,
              nrow = numPerSide^2,
              ncol = 2)
  m[ ,1] <- rep(x = vec1,
                each = numPerSide)
  m[ ,2] <- rep(x = vec2,
                times = numPerSide)
  return(m)
}

getAll <- function(vec){
  n <- length(vec)
  m <- matrix(data = NA,
              nrow = factorial(n) * factorial(n - 1),
              ncol = 2 * n)
  m[ ,(n + 1):(2 * n)] <- matrix(data = unlist(rep(x = permn(vec),
                                                   each = factorial(n - 1))),
                                 nrow = nrow(m),
                                 ncol = n,
                                 byrow = TRUE)
  k <- factorial(n - 1)
  for(i in 1:factorial(n)){
    j <- (i - 1) * k
    temp <- list()
    for(l in 1:(n - 1)){
      temp[[l]] <- m[j + 1,(n + 1):(n + l)]
    }
    m[(j + 1):(j + k),2:n] <- as.matrix(expand.grid(temp))
  }
  keep <- rep(x = NA,
              times = nrow(m))
  for(i in 1:nrow(m)){
    keep[i] <- sum(m[i,2:n] < m[i,(n + 2):(2 * n)]) == n - 1
  }
  m <- m[keep, ]
  return(m)
}

getNPaths <- function(x){
  factorial(x) * factorial(x - 1) / 2^(x - 1)
}

randAnc <- function(n){
  u <- rep(x = NA,
           times = n)
  for(v in 2:n){
    u[v] <- sample(x = 1:(v - 1),
                   size = 1)
  }
  u <- as.character(u)
  return(u)
}

deconSearch <- function(g,
                        method,
                        numPerSide = NULL,
                        returnAll = FALSE,
                        useInit = FALSE,
                        iter = NULL){
  n <- gorder(g)
  if(method == "minUni"){
    m <- deconstruct(g = g,
                     method = method,
                     returnUV = TRUE,
                     returnRaw = TRUE)
    m <- matrix(data = m,
                nrow = 1,
                ncol = length(m))
    if(m[1,5] != 0){
      continue <- TRUE
      while(continue){
        mRow <- nrow(m)
        newCenter <- c(1 - m[mRow,4] / m[mRow,5],
                       m[mRow,3] / (n - 1))
        if(is.nan(newCenter[1]) | useInit){
          newCenter[1] <- m[mRow,1]
        }
        if(useInit){
          newCenter[2] <- m[mRow,2]
        }
        temp <- deconstruct(g = g,
                            method = "NLL",
                            init = newCenter,
                            returnUV = TRUE,
                            returnRaw = TRUE)
        continue <- getNegLL(con = temp[3],
                             int = temp[4],
                             uni = temp[5],
                             n = n) < getNegLL(con = m[mRow,3],
                                               int = m[mRow,4],
                                               uni = m[mRow,5],
                                               n = n)
        m <- rbind(m,temp)
      }
    }
  } else if(method == "NLL"){
    m <- matrix(data = NA,
                nrow = numPerSide^2,
                ncol = 5 + 2 * n)
    m[1:numPerSide^2,1:2] <- getCoords(center = c(0.5,0.5),
                                       gap = 1 / (numPerSide + 1),
                                       numPerSide = numPerSide)
    for(i in 1:nrow(m)){
      m[i, ] <- deconstruct(g = g,
                            method = method,
                            init = m[i,1:2],
                            returnUV = TRUE,
                            returnRaw = TRUE)
    }
    temp <- matrix(data = NA,
                   nrow = numPerSide^2,
                   ncol = 5 + 2 * n)
    counter <- 1
    continue <- TRUE
    while(continue){
      useThis <- randMin(getNegLL(con = m[ ,3],
                                  int = m[ ,4],
                                  uni = m[ ,5],
                                  n = n))
      newCenter <- c(1 - m[useThis,4] / m[useThis,5],
                     m[useThis,3] / (n - 1))
      if(is.nan(newCenter[1]) | useInit){
        newCenter[1] <- m[useThis,1]
      }
      if(useInit){
        newCenter[2] <- m[useThis,2]
      }
      counter <- counter + 1
      temp[1:numPerSide^2,1:2] <- getCoords(center = newCenter,
                                            gap = 1 / (numPerSide + 1)^counter,
                                            numPerSide = numPerSide)
      for(i in 1:nrow(temp)){
        temp[i, ] <- deconstruct(g = g,
                                 method = "NLL",
                                 init = temp[i,1:2],
                                 returnUV = TRUE,
                                 returnRaw = TRUE)
      }
      continue <- min(getNegLL(con = temp[ ,3],
                               int = temp[ ,4],
                               uni = temp[ ,5],
                               n = n)) < min(getNegLL(con = m[ ,3],
                                                      int = m[ ,4],
                                                      uni = m[ ,5],
                                                      n = n))
      m <- rbind(m,temp)
    }
    if(!is.null(iter)){
      for(times in 1:iter){
        useThis <- randMin(getNegLL(con = m[ ,3],
                                    int = m[ ,4],
                                    uni = m[ ,5],
                                    n = n))
        newCenter <- c(1 - m[useThis,4] / m[useThis,5],
                       m[useThis,3] / (n - 1))
        if(is.nan(newCenter[1]) | useInit){
          newCenter[1] <- m[useThis,1]
        }
        if(useInit){
          newCenter[2] <- m[useThis,2]
        }
        counter <- counter + 1
        temp[1:numPerSide^2,1:2] <- getCoords(center = newCenter,
                                              gap = 1 / (numPerSide + 1)^counter,
                                              numPerSide = numPerSide)
        for(i in 1:nrow(temp)){
          temp[i, ] <- deconstruct(g = g,
                                   method = "NLL",
                                   init = temp[i,1:2],
                                   returnUV = TRUE,
                                   returnRaw = TRUE)
        }
        m <- rbind(m,temp)
      }
    }
  } else if(method == "random"){
    m <- t(replicate(n = iter,
                     expr = deconstruct(g = g,
                                        method = method,
                                        returnUV = TRUE,
                                        returnRaw = TRUE)))
  } else if(method == "all"){
    m <- matrix(data = NA,
                nrow = getNPaths(n),
                ncol = 5 + 2 * n)
    allPaths <- getAll(V(g)$name)
    for(i in 1:nrow(allPaths)){
      m[i, ] <- deconstruct(g = g,
                            method = method,
                            u = allPaths[i,1:n],
                            v = allPaths[i,(n + 1):(2 * n)],
                            returnUV = TRUE,
                            returnRaw = TRUE)
    }
  }
  
  if(returnAll){
    return(m)
  } else {
    result <- rep(x = NA,
                  times = 16)
    
    # Remove duplicates
    if(nrow(m) > 1){
      m <- m[!duplicated(m[ ,6:NCOL(m)]), ]
      if(class(m) == "numeric"){
        m <- matrix(data = m,
                    nrow = 1,
                    ncol = length(m))
      }
    }
    
    # Get raw data
    con <- m[ ,3]
    int <- m[ ,4]
    uni <- m[ ,5]
    
    # First is maximum likelihood estimate
    negLL <- getNegLL(con = con,
                      int = int,
                      uni = uni,
                      n = n)
    useThis <- randMin(negLL)
    result[1:2] <- m[useThis,1:2]
    result[3] <- getNegLL(con = m[useThis,3],
                          int = m[useThis,4],
                          uni = m[useThis,5],
                          n = n,
                          init = m[useThis,1:2])
    result[4] <- sqrt(result[1] * (1 - result[1]) / m[useThis,5])
    result[5] <- sqrt(result[2] * (1 - result[2]) / (n - 1))
    result[6] <- 1 - m[useThis,4] / m[useThis,5]
    result[7] <- m[useThis,3] / (n - 1)
    result[8] <- negLL[useThis]
    result[9] <- sqrt(result[6] * (1 - result[6]) / m[useThis,5])
    result[10] <- sqrt(result[7] * (1 - result[7]) / (n - 1))
    result[15] <- kendall.tau(x = n:1,
                              y = m[useThis,(n + 6):ncol(m)],
                              exact = TRUE,
                              max.n = n)
    result[16] <- kendall.tau(x = n:1,
                              y = swapUV(u = m[useThis,6:(n + 5)],
                                         v = m[useThis,(n + 6):ncol(m)]),
                              exact = TRUE,
                              max.n = n)
    
    # Get average
    p <- exp(min(negLL) - negLL)
    result[11] <- 1 - weighted.mean(x = int,
                                    w = p,
                                    na.rm = TRUE) / weighted.mean(x = uni,
                                                                  w = p,
                                                                  na.rm = TRUE)
    result[12] <- weighted.mean(x = con,
                                w = p,
                                na.rm = TRUE) / (n - 1)
    
    # Get EM
    qm <- result[6]
    qc <- result[7]
    continue <- TRUE
    reltol <- sqrt(.Machine$double.eps)
    while(continue){
      p <- getNegLL(con = con,
                    int = int,
                    uni = uni,
                    n = n,
                    init = c(qm,qc))
      p <- exp(min(p) - p)
      conAve <- weighted.mean(x = con,
                              w = p,
                              na.rm = TRUE)
      uniAve <- weighted.mean(x = uni,
                              w = p,
                              na.rm = TRUE)
      intAve <- weighted.mean(x = int,
                              w = p,
                              na.rm = TRUE)
      qmNew <- (uniAve - intAve) / uniAve
      qcNew <- conAve / (n - 1)
      relChange <- abs(c(qmNew,qcNew) - c(qm,qc))
      relChange <- relChange / (c(qm,qc) + reltol)
      if(sum(relChange < reltol,
             na.rm = TRUE) == sum(!is.na(relChange))){
        continue <- FALSE
      } else {
        qm <- qmNew
        qc <- qcNew
      }
    }
    result[13:14] <- c(qm,qc)
    return(result)
  }
  
}

refineRaw <- function(m,n){
  qmHat <- 1 - m[ ,4] / m[ ,5]
  qcHat <- m[ ,3] / (n - 1)
  negLL <- getNegLL(con = m[ ,3],
                    int = m[ ,4],
                    uni = m[ ,5],
                    n = n)
  kt1 <- rep(x = NA,
             times = nrow(m))
  kt2 <- rep(x = NA,
             times = nrow(m))
  for(i in 1:nrow(m)){
    kt1[i] <- kendall.tau(x = n:1,
                          y = m[i,(n + 6):ncol(m)],
                          exact = TRUE,
                          max.n = n)
    kt2[i] <- kendall.tau(x = n:1,
                          y = swapUV(u = m[i,6:(n + 5)],
                                     v = m[i,(n + 6):ncol(m)]),
                          exact = TRUE,
                          max.n = n)
  }
  return(cbind(qmHat,
               qcHat,
               negLL,
               kt1,
               kt2))
}




