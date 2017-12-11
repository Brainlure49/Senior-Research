#########################################################################
# Script for using Naive Bayes classification to determine MIDI file keysignature
#
# Ben Adams, Christ Diglio
#########################################################################

library(e1071)
library(rminer)

# Read in data
m_data <- read.csv("C:/Users/Ben/Desktop/Data.txt", sep = ",")
m_test <- read.csv("C:/Users/Ben/Desktop/Test.txt", sep = ",")

m_data <-  data.frame(m_data)
m_test <- data.frame(m_test)

colnames(m_data) <- c("key", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")

# Generate Naive Bayes model for training data
nb_model <- naiveBayes(as.factor(key) ~ ., data = m_data)
nb_model

colnames(m_test) <- c("key", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")

# Generate predictions on test data from model
predict <- predict(nb_model, m_test)
predict

# Calculate accuracy
ftest_keys <- factor(m_test$key, levels = c(0:23))
tab1 <- table(predict, ftest_keys)
acc1 <- sum(diag(tab1)) / sum(tab1)

# Accuracy
acc1

