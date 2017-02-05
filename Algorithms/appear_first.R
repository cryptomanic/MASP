longestRuleSizeMod <- function(root, item) {
  nodes <- Traverse(root, filterFun = isLeaf)
  pinroot <- lapply(nodes, FUN = function(node){node$path})
  pexroot <- lapply(pinroot, FUN = function(v){v[-1]})
  max(sapply(pexroot, function(x) ifelse(item %in% x, length(x), 0)))
}

new_model <- function(data, support = 0.3, confidence = 0.2) {
  itemsMinRow <- uniqueItems(data)
  print(itemsMinRow)
  items <- itemsMinRow[[1]]
  rowno <- itemsMinRow[[2]]
  
  row <- nrow(data)
  col <- ncol(data)
  
  globalmax <- 0
  for(i in 1:length(items)) {
    root <- generateMaspTree2(data[rowno[i]:row, , drop = FALSE], support, confidence)
    lrs <- longestRuleSizeMod(root, items[i])
    print(lrs)
    if(lrs > globalmax) globalmax <- lrs
  }
  
  globalmax
}

# testing
