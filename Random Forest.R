#########################################################################
# Script for using Random Forest classification to determine MIDI file keysignature
#
# Ben Adams, Christ Diglio
#########################################################################

library(randomForest)
set.seed(511)

#Read in data
m_data <- read.csv("C:/Users/Ben/Desktop/Data.txt", sep = ",")

m_test <- read.csv("C:/Users/Ben/Desktop/Test.txt", sep = ",")

m_data <-  data.frame(m_data)
m_test <- data.frame(m_test)

#Extract test key signatures
keys <- m_test$key

key.rf <- randomForest(as.factor(key) ~ ., data = m_data, ntree = 2000)

# Calculate accuracy
pred <- predict(key.rf, m_test)
keys <- factor(keys, levels = c(0:23))
tab1 <- table(pred, keys)
acc1 <- sum(diag(tab1)) / sum(tab1)

# Accuracy
acc1

# Cross-validation
rfcv <- rfcv(m_cvdata, as.factor(m_cvkeys), cv.fold = 10, step = 2)

# Entry labeled 12 is the last iteration, and has the lowest error rate.
rfcv$error.cv


