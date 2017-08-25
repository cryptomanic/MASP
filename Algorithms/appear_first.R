longestRuleSizeMod <- function(root, item) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  max(sapply(pexroot, function(x) ifelse(item %in% x, length(x), 0)))
}

generateMaspRulesMod <- function(masp, items) {
  l <- length(masp)-1
  
  if(l == 0 || !any(items %in% masp)) {
    return(list())
  }
  
  where_occur <- length(masp)
  
  for(i in 1:length(items)) {
    where_occur <- min(which(masp == items[i]), where_occur)
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
    
    if(where_occur <= i+1)
      rules <- append(rules, rule)
  }
  rules
}

generateAllMaspRulesMod <- function(root, items) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  allRules <- list()
  for(i in 1:length(pexroot)) {
    rules <- generateMaspRulesMod(pexroot[[i]], items)
    allRules <- append(allRules, rules)
  }
  allRules
}

  root_collection <- list()
  new_model <- function(data, support = 0.3, confidence = 0.2) {
    # itemsMinRow is a list
    # first element is the row number after that which items first appear in this row
    itemsMinRow <- uniqueItems(data)
    print(itemsMinRow)
    
    row <- nrow(data)
    col <- ncol(data)
    
    root_collection <<- list()
    allrules <- list()
    for(i in 1:length(itemsMinRow)) {
      help <- itemsMinRow[[i]]
      root <- generateMaspTree2(data[(as.numeric(help[1])):row, , drop = FALSE], support, confidence)
      lrs <- generateAllMaspRulesMod(root, help[-1])
      root_collection <<- append(root_collection, root)
      allrules <- append(allrules, lrs)
    }
    unique(allrules)
  }
  
max_rule_size <- function(rules) {
  max(0, sapply(rules, function(rule) length(strsplit(rule, ",")[[1]]) + 1))
}  

# testing

support <- .03; confidence <-  .001;
library(tidyverse) # support .60 confidence .30
tdt <- tribble(~C1, ~C2, ~C3, ~C4, ~C5,
                '1', '2', '3', '4', '5',
                '1', '5', '3', '4', '2',
                '8', '1', '9', '2', '3',
                '9', '2', '3', '1', '7',
                '1', '9', '3', '8', '7',
                '1', '8', '3', '2', '7')

# for old algo
dfdt <- as.data.frame(tdt)
for(i in 1:5) dfdt[[i]] <- as.integer(dfdt[[i]])

# for new algo
mtdt <- as.matrix(dfdt)
mtdt[1, 1] <- as.character(mtdt[1, 1])

# old approach
root1 <- generateMaspTree(dfdt, support, confidence)
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)

# new approach
new_rules <- new_model(mtdt, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)

# mid approach
root2 <- generateMaspTree2(mtdt, support, confidence)
mid_rules <- generateAllMaspRules(root2)
length(mid_rules)
max_rule_size(mid_rules)

#lowram #stuck #testcase
generate.data.mod <- function(nrow, ncol, rng) {
  data <- matrix(nrow = nrow, ncol = ncol) 
  for(i in 1:nrow) {
    data[i, ] <- sample(1:rng, ncol)
  }
  old_data <- as.data.frame(data)
  new_data <- data
  new_data[1, 1] <- as.character(new_data[1, 1])
  
  list(old_data = old_data, new_data = new_data)
}

# dataset B
set.seed(10001)
cdata <- generate.data.mod(20, 20, 25)
root1 <- generateMaspTree(cdata$old_data, support, confidence)
root1
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)
new_rules <- new_model(cdata$new_data, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)
# mid approach
root2 <- generateMaspTree2(cdata$new_data, support, confidence)
mid_rules <- NULL
mid_rules <- generateAllMaspRules(root2)
length(mid_rules)
max_rule_size(mid_rules)

# dataset C
set.seed(1010)
cdata <- generate.data.mod(40, 40, 47)
root1 <- generateMaspTree(cdata$old_data, support, confidence)
root1
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)
new_rules <- new_model(cdata$new_data, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)
# mid approach
root2 <- generateMaspTree2(cdata$new_data, support, confidence)
mid_rules <- NULL
mid_rules <- generateAllMaspRules(root2)
length(mid_rules)
max_rule_size(mid_rules)

# Dataset D  slow speed
set.seed(1010)
cdata <- generate.data.mod(100, 100, 105)
root1 <- generateMaspTree(cdata$old_data, support, confidence)
root1
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)
new_rules <- new_model(cdata$new_data, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)
# mid approach
root2 <- generateMaspTree2(cdata$new_data, support, confidence)
mid_rules <- NULL
mid_rules <- generateAllMaspRules(root2)
length(mid_rules)
max_rule_size(mid_rules)

# Dataset E when new approch became worse
set.seed(1010)
cdata <- generate.data.mod(1000, 1000, 1020)
root1 <- generateMaspTree(cdata$old_data, support, confidence)
root1
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)
new_rules <- new_model(cdata$new_data, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)
# mid approach
root2 <- generateMaspTree2(cdata$new_data, support, confidence)
mid_rules <- NULL
mid_rules <- generateAllMaspRules(root2)
length(mid_rules)
max_rule_size(mid_rules)

# connect dataset on first 3000 transactions
data <- read.table("connect.dat")[1:3000, ]
mdata <- as.matrix(data)
mdata[1, 1] <- as.character(mdata[1, 1])

old_rules <- NULL
mid_rules <- NULL
new_rules <- NULL
support <- 0.6;confidence <- 0.3;
root1 <- generateMaspTree(data, support, confidence)
root2 <- generateMaspTree2(mdata, support, confidence)
new_rules <- new_model(mdata, support, confidence)

old_rules <- generateAllMaspRules(root1)
mid_rules <- generateAllMaspRules(root2)

length(old_rules)
max_rule_size(old_rules)
length(new_rules)
max_rule_size(new_rules)
length(mid_rules)
max_rule_size(mid_rules)

# ------------------------------------------------------------

# mushroom dataset
data <- read.table("mushroom.dat")
mdata <- as.matrix(data)
mdata[1, 1] <- as.character(mdata[1, 1])

old_rules <- NULL
mid_rules <- NULL
new_rules <- NULL
support <- 0.6;confidence <- 0.3;
root1 <- generateMaspTree(data, support, confidence)
root2 <- generateMaspTree2(mdata, support, confidence)
new_rules <- new_model(mdata, support, confidence)

old_rules <- generateAllMaspRules(root1)
mid_rules <- generateAllMaspRules(root2)

length(old_rules)
max_rule_size(old_rules)
length(new_rules)
max_rule_size(new_rules)
length(mid_rules)
max_rule_size(mid_rules)
# ------------------------------------------------------------

# chess dataset
data <- read.table("chess.dat")
mdata <- as.matrix(data)
mdata[1, 1] <- as.character(mdata[1, 1])

old_rules <- NULL
mid_rules <- NULL
new_rules <- NULL
support <- 0.6;confidence <- 0.3;
root1 <- generateMaspTree(data, support, confidence)
root2 <- generateMaspTree2(mdata, support, confidence)
new_rules <- new_model(mdata, support, confidence)

old_rules <- generateAllMaspRules(root1)
mid_rules <- generateAllMaspRules(root2)

length(old_rules)
max_rule_size(old_rules)
length(new_rules)
max_rule_size(new_rules)
length(mid_rules)
max_rule_size(mid_rules)

# ------------------------------------------------------------

mat <- matrix(nrow = 4, ncol = 4)
mat[1, ] <- c(1, 2, 3, 4)
mat[2, ] <- c(2, 3, 4, 1)
mat[3, ] <- c(3, 4, 1, 2)
mat[4, ] <- c(4, 1, 2, 3)
mat[1, 1] <- as.character(mat[1, 1])
new_rules <- new_model(mat, .3, .01)


# TEST -----/-------
tdt <- tribble(~C1, ~C2, ~C3,
               '1', '2', '3',
               '3', '1', '2',
               '2', '3', '1')

# for old algo
dfdt <- as.data.frame(tdt)
for(i in 1:3) dfdt[[i]] <- as.integer(dfdt[[i]])

# for new algo
mtdt <- as.matrix(dfdt)
mtdt[1, 1] <- as.character(mtdt[1, 1])

# old approach
root1 <- generateMaspTree(dfdt, .001, .001)
old_rules <- NULL
new_rules <- NULL
old_rules <- generateAllMaspRules(root1)

# new approach
new_rules <- new_model(mtdt, support, confidence)
length(old_rules)
length(new_rules)
max_rule_size(old_rules)
max_rule_size(new_rules)

# generate dataset information
info_set <- function(dst) {
  cat("rows:", nrow(dst), "\n")
  cat("cols:", ncol(dst), "\n")
  unitems <- sort(as.integer(unique(unlist(dst))))
  cat("UniqueItems: ", length(unitems), "\n")
  cat(unitems)
}

# microbenchmark is used for calculating running time