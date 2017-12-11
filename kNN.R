#########################################################################
# Script for using kNN classification to determine MIDI file keysignature
#
# Ben Adams, Christ Diglio
#########################################################################

library(class)

# Read in data
m_cvdata <- read.csv("C:/Users/Ben/Desktop/CVData.txt", sep = ",")
m_data <- read.csv("C:/Users/Ben/Desktop/Data.txt", sep = ",")
m_test <- read.csv("C:/Users/Ben/Desktop/Test.txt", sep = ",")

#Extract key signatures of training data
m_cvkeys <- m_cvdata[,1]
m_keys <- m_data[,1]

#Remove key signatures from training data data frame
m_cvdata <- m_cvdata[,-1]
m_data <- m_data[,-1]

test_keys <- m_test[,1]

#Remove key signatures from testing data data frame
m_test <- m_test[,-1]

#Scales data to between 0-1
normalize <- function(x) {
  return ( (x - min(x)) / (max(x) - min(x)))
}

# Apply normalize function to training data
n_data <- data.frame(apply(m_data, 1, normalize))

n_cvdata <- data.frame(apply(m_cvdata, 1, normalize))

# Apply normalize function to test data
n_test <- data.frame(apply(m_test, 1, normalize))

# Pass normalized training and testing data to kNN function, with k value of 5
k1 <- knn(train = m_data, test = m_test, cl = m_keys, k = 1, prob = TRUE)

# Show comparison of kNN key classifications to actual testing key signatures
ftest_keys <- factor(test_keys, levels = c(0:23))
tab1 <- table(ftest_keys, k1)
acc1 <- sum(diag(tab1)) / sum(tab1)
acc1

# KNN Cross-validation
k2 <- knn.cv(n_cvdata, m_cvkeys, 1, prob = TRUE)
tab2 <- table(m_cvkeys, k2)
acc2 <- sum(diag(tab2)) / sum(tab2)
acc2

