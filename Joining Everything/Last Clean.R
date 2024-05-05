setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")
# include libraries
library(dplyr)

train <- read.csv("Processed Data Set/Processed/processed_train.csv", TRUE, stringsAsFactors = FALSE)
test <- read.csv("Processed Data Set/Processed/processed_test.csv", TRUE, stringsAsFactors = FALSE)

train <- subset(train, status != 0)
test$status <- NULL

train$frequency <- as.factor(train$frequency)
levels(train$frequency) <- 1:length(levels(train$frequency))
train$frequency <- as.numeric(train$frequency)
train$gender <- as.factor(train$gender)
levels(train$gender) <- 1:length(levels(train$gender))
train$gender<- as.numeric(train$gender)
train$type <- as.factor(train$type)
levels(train$type) <- 1:length(levels(train$type))
train$type<- as.numeric(train$type)

test$frequency <- as.factor(test$frequency)
levels(test$frequency) <- 1:length(levels(test$frequency))
test$frequency <- as.numeric(test$frequency)
test$gender <- as.factor(test$gender)
levels(test$gender) <- 1:length(levels(test$gender))
test$gender<- as.numeric(test$gender)
test$type <- as.factor(test$type)
levels(test$type) <- 1:length(levels(test$type))
test$type<- as.numeric(test$type)

write.csv(train, file = "Processed Data Set/Processed/train.csv", row.names = FALSE)
write.csv(test, file = "Processed Data Set/Processed/test.csv", row.names = FALSE)