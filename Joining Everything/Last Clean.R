setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")
# include libraries
library(dplyr)

train <- read.csv("Processed Data Set/Processed/processed_train.csv", TRUE, stringsAsFactors = FALSE)
test <- read.csv("Processed Data Set/Processed/processed_test.csv", TRUE, stringsAsFactors = FALSE)

train <- subset(train, status != 0)
test$status <- NULL

write.csv(train, file = "Processed Data Set/Processed/train.csv", row.names = FALSE)
write.csv(test, file = "Processed Data Set/Processed/test.csv", row.names = FALSE)