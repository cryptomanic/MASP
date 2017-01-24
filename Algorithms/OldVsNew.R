data <- read.table("connect.dat")
set.seed(1001)
data <- t(apply(data, 1, sample)) # matrix

data_old <-  as.data.frame(data) # data for old algorithm

data_new <- data # data for new algorithm
data_new[1, 1] <- as.character(data_new[1, 1])

root_old <- generateMaspTree(data = data_old, support = 0.001, confidence = 0.25)
root_new <- generateMaspTree2(data = data_new, support = 0.001, confidence = 0.25)

plot(root_old)
plot(root_new)

longestRuleSize(root_old)
longestRuleSize(root_new) # new method is better!

# Mon Jan 23 22:00:33 2017 ------------------------------
data <- read.table("pumsb.dat")
data <- t(apply(data, 1, sample)) # matrix

data_old <-  as.data.frame(data) # data for old algorithm

data_new <- data # data for new algorithm
data_new[1, 1] <- as.character(data_new[1, 1])

root_old <- generateMaspTree(data = data_old, support = 0.001, confidence = 0.25)
root_new <- generateMaspTree2(data = data_new, support = 0.001, confidence = 0.25)

plot(root_old)
plot(root_new)

longestRuleSize(root_old)
longestRuleSize(root_new) # again better!

# Mon Jan 23 22:14:12 2017 ------------------------------
setwd("blogdata/")
# same item present more than once within a row
data <- read.csv("blogData_train.csv", header = FALSE) 

# Tue Jan 24 11:18:03 2017 ------------------------------
# let's generate a data set(unbiased)

generate.data <- function(nrow, ncol, rng) {
  data <- matrix(nrow = nrow, ncol = ncol) 
  for(i in 1:nrow) {
    if(i%%2 == 0)
      data[i, ] <- ncol:1
    else
      data[i, ] <- 1:ncol
  }
  old_data <- as.data.frame(data)
  new_data <- data
  new_data[1, 1] <- as.character(new_data[1, 1])
  
  list(old_data = old_data, new_data = new_data)
}

compareOandN <- function(nrow, ncol, rng, support, confidence) {
  ldata <- generate.data(nrow, ncol, rng)
  old_root <- generateMaspTree(data = ldata$old_data, support = support, confidence)
  new_root <- generateMaspTree2(data = ldata$new_data, support = support, confidence)
  list(old_root, new_root)
}

# reproducible comparison
set.seed(10001)
lroot <- compareOandN(10000, 100, 5, .001, .25)
plot(lroot[[1]])
plot(lroot[[2]])
longestRuleSize(lroot[[1]])
longestRuleSize(lroot[[2]])