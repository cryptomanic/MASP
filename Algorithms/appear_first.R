longestRuleSizeMod <- function(root, item) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  max(sapply(pexroot, function(x) ifelse(item %in% x, length(x), 0)))
}

generateMaspRulesMod <- function(masp, item) {
  l <- length(masp)-1
  
  if(l == 0 || !(item %in% masp)) {
    return(list())
  }
  
  where_occur <- which(masp == item)
  
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

generateAllMaspRulesMod <- function(root, item) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  allRules <- list()
  for(i in 1:length(pexroot)) {
    rules <- generateMaspRulesMod(pexroot[[i]], item)
    allRules <- append(allRules, rules)
  }
  allRules
}

  new_model <- function(data, support = 0.3, confidence = 0.2) {
    itemsMinRow <- uniqueItems(data)
    print(itemsMinRow)
    items <- itemsMinRow[[1]]
    rowno <- itemsMinRow[[2]]
    
    row <- nrow(data)
    col <- ncol(data)
    
    allrules <- list()
    for(i in 1:length(items)) {
      root <- generateMaspTree2(data[rowno[i]:row, , drop = FALSE], support, confidence)
      lrs <- generateAllMaspRulesMod(root, items[i])
      allrules <- append(allrules, lrs)
    }
    unique(allrules)
  }

# testing
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
root1 <- generateMaspTree(dfdt, .60, .3)
generateAllMaspRules(root1)

# new approach
new_model(mtdt, .6, .3)