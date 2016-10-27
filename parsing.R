f <- function(query) {
  
  query1 <- query[[1]]
  query2 <- query[[2]]
  
  d <- D[eval(parse(text = query1)), c(query2), drop = FALSE]
  
  gmax <- 0
  gind <- 0
  
  for (i in 1:ncol(d)) {
    max <- 0
    r <- d[, i]
    t <- table(r)
    
    for (j in 1:length(t)) {
      if (t[j] > max)
        max = t[j]
    }
    
    if (max > gmax) {
      gmax = max
      gind = i
    }
    
  }
  
  result <- list()
  
  colname <- colnames(d)[gind]
  result[[1]] <- paste0(query1, " & D$", colname, " == ", names(gmax))
  result[[2]] <- query2[query2 != colname]
  
  aresult <- list()
  aresult[[1]] <- paste0(query1, " & D$", colname, " != ", names(gmax))
  aresult[[2]] <- query2
  
  return(result)
}

# > query1
# [1] "TRUE"
# > query2
# [1] "A1 A2 A3 A4 A5"
# query = list()
# query[[1]] <- query1
# query[[2]] <- query2

# > query <- list()
# > query[[1]] <- "TRUE"
# > query[[2]] <- colnames(D)
# > root <- Node$new("ROOT")

# input D, support, confidence
D;S <- 0.4;C <- 0.3
library(data.tree)
MASP.TREE <- function(query, root, block) {
  
  query1 <- query[[1]]
  query2 <- query[[2]]
  
  d <- D[eval(parse(text = query1)), c(query2), drop = FALSE]
  
  if(nrow(d) == 0 || ncol(d) == 0) {
    return()
  }
  
  print(d)
  gmax <- 0
  gind <- 0
  
  for (i in 1:ncol(d)) {
    max <- 0
    r <- d[, i, drop = FALSE]
    t <- table(r)
    
    for (j in 1:length(t)) {
      if (t[j] > max)
        max = t[j]
    }
    
    if (max > gmax) {
      gmax = max
      gind = i
    }
    
  }
  
  colname <- colnames(d)[gind]
  
  result <- list()
  result[[1]] <- paste0(query1, " & D$", colname, " == ", names(gmax))
  result[[2]] <- query2[query2 != colname]
  
  aresult <- list()
  aresult[[1]] <- paste0(query1, " & D$", colname, " != ", names(gmax))
  aresult[[2]] <- query2
  
  if(gmax/nrow(D) >= S) {
    if(gmax/nrow(d) >= C) {
      lchild <- root$AddChild(names(gmax))
      MASP.TREE(result, lchild, TRUE)
      if((nrow(d)-gmax)/nrow(D) >= S) {
        if((nrow(d)-gmax)/nrow(d) >= C) {
          rchild <- root$AddChild(paste0("~", names(gmax)))
          MASP.TREE(aresult, rchild, FALSE)
        } else {
          rchild <- root$AddChild(paste0("~", names(gmax)))
          rchild$tmined <- TRUE
        }
      } else {
        rchild <- root$AddChild(paste0("~", names(gmax)))
        rchild$tmined <- FALSE
      }
    } else {
      lchild <- root$AddChild(names(gmax))
      lchild$tmined <- TRUE
    }
  } else if(block == TRUE) {
    root$tmined <- TRUE
  } else {
    root$tmined <- FALSE
  }
}

d <- data.frame(A1 = c(1,1,1,1,1,1,2,2,2,2))
d$A2 <- c(1,1,2,1,2,2,2,2,3,3)
d$A3 <- c(1,2,1,2,2,2,1,1,1,2)
d$A4 = c(1,1,2,2,3,3,4,4,5,6)
d$A5 = c(1,3,2,2,2,2,1,1,1,1)
D=d
query <- list()
query[[1]] <- "TRUE"
query[[2]] <- colnames(D)
root <- Node$new("ROOT")
MASP.TREE(query, root, TRUE)
acme=root
SetGraphStyle(acme, rankdir = "TB")
SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(acme, style = "filled,rounded", shape = "egg", fillcolor = "pink", 
             fontname = "helvetica", tooltip = GetDefaultTooltip)
plot(root)
