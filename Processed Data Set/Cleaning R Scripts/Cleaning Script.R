# set working directory
setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")
# include libraries
library(dplyr)

# -----------------------------------------------------------------
# loading data region
# loads data from csv
dispositions <- read.csv2("Processed Data Set/disp.csv", TRUE, stringsAsFactors = FALSE)
clients <- read.csv2("Processed Data Set/client.csv", TRUE, stringsAsFactors = FALSE)
accounts <- read.csv2("Processed Data Set/account.csv", TRUE, stringsAsFactors = FALSE)
districts <- read.csv2("Processed Data Set/district.csv", TRUE, stringsAsFactors = FALSE)
transactions <- read.csv2("Processed Data Set/trans_train.csv", TRUE, stringsAsFactors = FALSE)

# -----------------------------------------------------------------
# dispositions data cleaning

# shows dispositions data structure
str(dispositions)

# -----------------------------------------------------------------
# clients data cleaning

# shows clients data structure
str(clients)

# using subset function to filter clients by gender
females <- subset(clients,((clients$birth_number / 100) %% 100) > 50, 
select=c(client_id, birth_number, district_id))
males <- subset(clients,((clients$birth_number / 100) %% 100) < 50, 
select=c(client_id, birth_number, district_id))

# subtracts 5000 to females
females["birth_number"] <- females["birth_number"] - 5000

# add gender columns
females$gender <- "Female"
males$gender <- "Male"

# rebinds datasets
clients <- rbind(females, males)
clients <- clients[ with(clients, order(client_id)),]

# completes the birth year with 19XX
clients["birth_number"] <- clients["birth_number"] + 19000000
# parses field into date
clients$birth_number <- as.Date(as.character(clients$birth_number), "%Y%m%d")

# adds new field for the birth year to use in plotting
clients$birth_year <- as.integer(format(clients$birth_number,"%Y"))

# remove irrelevant data
clients$birth_number <- NULL

# -----------------------------------------------------------------
# accounts data cleaning

# shows accounts data structure
str(accounts)

# remove irrelevant data
accounts$date <- NULL

# -----------------------------------------------------------------
# districts data cleaning

# shows districts data structure
str(districts)

# replace missing values in unemployment rate of '95
districts$unemploymant.rate..95[districts$unemploymant.rate..95 == "?"] <- districts$unemploymant.rate..96[districts$unemploymant.rate..95 == "?"]
# replace missing values in commited crimes of '95
districts$no..of.commited.crimes..95[districts$no..of.commited.crimes..95 == "?"] <- districts$no..of.commited.crimes..96[districts$no..of.commited.crimes..95 == "?"]

# change datatypes of relevant fields
districts$ratio.of.urban.inhabitants <- as.numeric(districts$ratio.of.urban.inhabitants)
districts$unemploymant.rate..95 <- as.numeric(districts$unemploymant.rate..95)
districts$unemploymant.rate..96 <- as.numeric(districts$unemploymant.rate..96)
districts$no..of.commited.crimes..95 <- as.integer(districts$no..of.commited.crimes..95)

# -----------------------------------------------------------------
# transactions data cleaning

# shows transactions data structure
str(transactions)

# calculates average household payed
households <- subset(transactions, transactions$k_symbol == "household")
households$amount <- as.numeric(households$amount)
households <- aggregate(households[,"amount"], list(account_id = households$account_id), mean)
colnames(households)[2] <- "household"

# calculates average pension received
pensions <- subset(transactions, transactions$k_symbol == "old-age pension")
pensions$amount <- as.numeric(pensions$amount)
pensions <- aggregate(pensions[,"amount"], list(account_id = pensions$account_id), mean)
colnames(pensions)[2] <- "pension"

# parses string fields into numeric
transactions$balance <- as.numeric(transactions$balance)

# calculates average client's balance
averageBalances <- aggregate(transactions[,"balance"], list(account_id = transactions$account_id), mean)
colnames(averageBalances)[2] <- "avg_balances"

# calculates average deviation of client's balance
deviationBalances <- aggregate(transactions[,"balance"], list(account_id = transactions$account_id), sd)
colnames(deviationBalances)[2] <- "sd_balances"

# -----------------------------------------------------------------
# merge datasets

# merging clients with districts
merged <- merge(clients, districts, by.x = "district_id", by.y = "code")
# removing irrelevant columns
merged$district_id <- NULL
merged$no..of.municipalities.with.inhabitants...499 <- NULL
merged$no..of.municipalities.with.inhabitants.500.1999 <- NULL
merged$no..of.municipalities.with.inhabitants.2000.9999 <- NULL
merged$no..of.municipalities.with.inhabitants..10000 <- NULL
merged$no..of.cities <- NULL

# merging with dispositions
merged <- merge(merged, dispositions, by = "client_id")
# remove disponents information
merged <- subset(merged, merged$type != "DISPONENT")
# removing irrelevant columns
merged$disp_id <- NULL
merged$district_id <- NULL
merged$type <- NULL

# merging with accounts
merged <- merge(merged, accounts, by = "account_id", all.x = TRUE)
# removing irrelevant columns
merged$date <- NULL
merged$district_id <- NULL

# merging with relevant transactions
merged <- merge(merged, households, by = "account_id", all.x = TRUE)
merged <- merge(merged, pensions, by = "account_id", all.x = TRUE)
merged <- merge(merged, averageBalances, by = "account_id", all.x = TRUE)
merged <- merge(merged, deviationBalances, by = "account_id", all.x = TRUE)

# shows merged data structures
str(merged)

# -----------------------------------------------------------------
# creates file with processed data structure

# writing operation
write.csv(merged, file = "Processed Data Set/Processed/processed_train.csv", row.names = FALSE)





