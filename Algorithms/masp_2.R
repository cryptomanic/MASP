library(data.tree)
MASP.TREE.2 <- function(data, root, block) {
  
  if(nrow(data) == 0 || ncol(data) == 0) {
    return()
  }
  
  # list of
  #   1. frequent item
  #   2. block
  #   3. counter block
  l <- fitem_matrs(data)
  
  fitem <- l[[1]]
  tblock <- l[[2]]
  tcblock <- l[[3]]
  
  if(nrow(tblock)/nrow(D) >= S) {
    if(nrow(tblock)/nrow(data) >= C) {
      lchild <- root$AddChild(fitem)
      MASP.TREE.2(tblock, lchild, TRUE)
      if(nrow(tcblock)/nrow(D) >= S) {
        if(nrow(tcblock)/nrow(data) >= C) {
          rchild <- root$AddChild(paste0("~", fitem))
          MASP.TREE.2(tcblock, rchild, FALSE)
        }
      }
    }
  } 
}

D <- S <- C <- NULL

generateMaspTree2 <- function(data, support = 0.2, confidence = 0.3) {
  if(!(support >= 0 && support <= 1)) {
    stop("support value must belongs to [0, 1]")
  }
  
  if(!(confidence >= 0 && confidence <= 1)) {
    stop("confidence value must belongs to [0, 1]")
  }
  
  if(! is.matrix(data)) {
    stop("data must be a matrix")
  }
  
  D <<- data
  S <<- support
  C <<- confidence
  
  root <- Node$new("ROOT")
  MASP.TREE.2(data, root, TRUE)
  root
}

# example
mat <- matrix(nrow = 4, ncol = 4)
mat[1, ] <- c(1, 2, 3, 4, 5)
mat[2, ] <- c(2, 3, 4, 1)
mat[3, ] <- c(3, 4, 1, 2)
mat[4, ] <- c(4, 1, 2, 3)

# old approach
plot(root1 <- generateMaspTree(as.data.frame(mat)))

dtdt <- tribble(~C1, ~C2, ~C3, ~C4, ~C5,
               '1', '12', '3', '4', '5',
               '1', '5', '6', '4', '12',
               '8', '6', '9', '12', '5',
               '9', '2', '3', '6', '7',
               '6', '9', '10', '8', '7',
               '1', '8', '3', '2', '7')
dfdt <- as.data.frame(dtdt)
mtdt <- as.matrix(dfdt)
mtdt[1, 1] <- as.character(mtdt[1, 1])
plot(root2 <- generateMaspTree2(mtdt[5:6,], .2, .3))
# new approach
mat[1, 1] <- as.character(mat[1, 1])
plot(root2 <- generateMaspTree2(mat, .2, .3))
