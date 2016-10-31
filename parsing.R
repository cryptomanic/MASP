library(data.tree)

# generate masp tree
# query: query to get masp block
# root: root of the sub tree
# block: if true implies block else counterblock
MASP.TREE <- function(query, root, block) {
  
  query1 <- query[[1]]
  query2 <- query[[2]]
  
  d <- D[eval(parse(text = query1)), c(query2), drop = FALSE]
  
  if(nrow(d) == 0 || ncol(d) == 0) {
    return()
  }
  
  root$query1 <- query1
  root$query2 <- query2
  
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
  
  # nrow.d <- nrow(d)
  # rm(d)
  
  if(gmax/nrow(D) >= S) {
    if(gmax/nrow(d) >= C) {
      lchild <- root$AddChild(paste0(colname, "=", names(gmax)))
      MASP.TREE(result, lchild, TRUE)
      if((nrow(d)-gmax)/nrow(D) >= S) {
        if((nrow(d)-gmax)/nrow(d) >= C) {
          rchild <- root$AddChild(paste0("~", paste0(colname, "=", names(gmax))))
          MASP.TREE(aresult, rchild, FALSE)
        } else {
          rchild <- root$AddChild(paste0("~", paste0(colname, "=", names(gmax))))
          rchild$tmined <- TRUE
        }
      } else {
        rchild <- root$AddChild(paste0("~", paste0(colname, "=", names(gmax))))
        rchild$tmined <- FALSE
      }
    } else {
      lchild <- root$AddChild(paste0(colname, "=", names(gmax)))
      lchild$tmined <- TRUE
    }
  } else if(block == TRUE) {
    root$tmined <- TRUE
  } else {
    root$tmined <- FALSE
  }
}

D = NULL;S = NULL; C = NULL;

# wrapper function
# D: data table
# S: support
# C: confidence
generateMaspTree <- function(data, support = 0.2, confidence = 0.3) {
  if(!(support >= 0 && support <= 1)) {
    stop("support value must belongs to [0, 1]")
  }
  
  if(!(confidence >= 0 && confidence <= 1)) {
    stop("confidence value must belongs to [0, 1]")
  }
    
  if(! is.data.frame(data)) {
    stop("data must be a data frame")
  }
  
  D <<- data
  S <<- support
  C <<- confidence
  
  query <- list()
  query[[1]] <- "TRUE"
  query[[2]] <- colnames(D)
  root <- Node$new("ROOT")
  MASP.TREE(query, root, TRUE)
  root
}

# GET DATA TABLE
subdata <- function(query1, query2) {
  D[eval(parse(text = query1)), c(query2), drop = FALSE]
}

# TESTING 1
d <- data.frame(A1 = c(1,1,1,1,1,1,2,2,2,2))
d$A2 <- c(1,1,2,1,2,2,2,2,3,3)
d$A3 <- c(1,2,1,2,2,2,1,1,1,2)
d$A4 = c(1,1,2,2,3,3,4,4,5,6)
d$A5 = c(1,3,2,2,2,2,1,1,1,1)

root <- generateMaspTree(data = d, support = 0.2, confidence = 0.3)
plot(root)

# leaf nodes
nodes <- Traverse(root, filterFun = isLeaf)
# path from root to all leaf
pinroot <- lapply(nodes, FUN = function(node){node$path})
# remove root node
pexroot <- lapply(pinroot, FUN = function(v){v[-1]})

# information associated with every nodes nodes
all.nodes <- Traverse(root)

for(i in 1:length(all.nodes)) {
  base <- all.nodes[[i]]
  cat("<-----------------------------------------\n")
  cat("name\n", base$name, "\n")
  cat("query1\n", base$query1, "\n")
  cat("query2\n", base$query2, "\n")
  if(base$isLeaf)
    cat("tmined\n", base$tmined, "\n")
  cat("----------------------------------------->\n")
  cat("\n")
}

# TESTING 2
d <- read.table("pumsb.dat")
root <- generateMaspTree(data = d, support = 0.1, confidence = 0.5)
plot(root)

# > system.time(root <- generateMaspTree(data = d, support = 0.001, confidence = 0.25))
# user  system elapsed 
# 58.076   0.024  58.065

# TESTING 3
d <- read.table("connect.dat")
root <- generateMaspTree(data = d, support = 0.001, confidence = 0.25)
plot(root)

longestRuleSize <- function(root) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  max(sapply(pexroot, function(x) length(x)))
}

generateMaspRules <- function(masp) {
   l <- length(masp)-1
   
   if(l == 0) {
     return(list())
   }
   
   rules <- list()
   for(i in 1:l) {
     rule <- paste0("(")
     for(j in 1:i) {
       if(j == i)
         rule <- paste0(rule, masp[j], ")")
       else
         rule <- paste0(rule, masp[j], ", ")
     }
     rule <- paste0(rule, " -> (", masp[i+1], ")")
     rules[[i]] <- rule
   }
   rules
}

generateAllMaspRules <- function(root) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  allRules <- list()
  for(i in 1:length(pexroot)) {
    rules <- generateMaspRules(pexroot[[i]])
    allRules <- append(allRules, rules)
  }
  allRules
}
