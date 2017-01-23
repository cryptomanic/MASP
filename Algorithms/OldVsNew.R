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

