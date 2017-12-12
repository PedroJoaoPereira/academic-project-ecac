setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")
# include libraries
library(dplyr)

# -----------------------------------------------------------------
# processed by andre
all_train <- read.csv2("Processed Data Set/all_train.csv", TRUE, stringsAsFactors = FALSE)
all_test <- read.csv2("Processed Data Set/all_test.csv", TRUE, stringsAsFactors = FALSE)

# parses fields
all_train$district_id <- as.integer(all_train$district_id)
all_train$avg_mensal_income <- as.numeric(all_train$avg_mensal_income)
all_train$avg_mensal_expenses <- as.numeric(all_train$avg_mensal_expenses)
all_train$avg_transaction_count <- as.numeric(all_train$avg_transaction_count)
all_train$owner_account_id <- as.integer(all_train$owner_account_id)
all_train$min_balance <- as.numeric(all_train$min_balance)
all_train$max_balance <- as.numeric(all_train$max_balance)
all_train$account_id <- as.integer(all_train$account_id)

all_test$district_id <- as.integer(all_test$district_id)
all_test$avg_mensal_income <- as.numeric(all_test$avg_mensal_income)
all_test$avg_mensal_expenses <- as.numeric(all_test$avg_mensal_expenses)
all_test$avg_transaction_count <- as.numeric(all_test$avg_transaction_count)
all_test$owner_account_id <- as.integer(all_test$owner_account_id)
all_test$min_balance <- as.numeric(all_test$min_balance)
all_test$max_balance <- as.numeric(all_test$max_balance)
all_test$account_id <- as.integer(all_test$account_id)

# -----------------------------------------------------------------
# load clients
clients <- read.csv2("Processed Data Set/client.csv", TRUE, stringsAsFactors = FALSE)

# processes clients
females <- subset(clients,((clients$birth_number / 100) %% 100) > 50, 
select=c(client_id, birth_number, district_id))
males <- subset(clients,((clients$birth_number / 100) %% 100) < 50, 
select=c(client_id, birth_number, district_id))

females["birth_number"] <- females["birth_number"] - 5000

females$gender <- "Female"
males$gender <- "Male"

clients <- rbind(females, males)
clients <- clients[ with(clients, order(client_id)),]

clients["birth_number"] <- clients["birth_number"] + 19000000
clients$birth_number <- as.Date(as.character(clients$birth_number), "%Y%m%d")

clients$birth_year <- as.integer(format(clients$birth_number,"%Y"))
clients$birth_number <- NULL

all_info_train <- merge(all_train, clients, by.x = "owner_account_id", by.y = "client_id", all.x = TRUE)
all_info_test <- merge(all_test, clients, by.x = "owner_account_id", by.y = "client_id", all.x = TRUE)

all_info_train$date <- NULL
all_info_train$gender.x <- NULL
all_info_train$district_id.y <- NULL

colnames(all_info_train)[1] <- "owner_id"
colnames(all_info_train)[2] <- "district_id"
colnames(all_info_train)[7] <- "owner_ager"
colnames(all_info_train)[12] <- "gender"

all_info_test$date <- NULL
all_info_test$gender.x <- NULL
all_info_test$district_id.y <- NULL

colnames(all_info_test)[1] <- "owner_id"
colnames(all_info_test)[2] <- "district_id"
colnames(all_info_test)[7] <- "owner_ager"
colnames(all_info_test)[12] <- "gender"

# -----------------------------------------------------------------
# load districts
districts <- read.csv2("Processed Data Set/district.csv", TRUE, stringsAsFactors = FALSE)

# processes districts
districts$unemploymant.rate..95[districts$unemploymant.rate..95 == "?"] <- districts$unemploymant.rate..96[districts$unemploymant.rate..95 == "?"]
districts$no..of.commited.crimes..95[districts$no..of.commited.crimes..95 == "?"] <- districts$no..of.commited.crimes..96[districts$no..of.commited.crimes..95 == "?"]

districts$ratio.of.urban.inhabitants <- as.numeric(districts$ratio.of.urban.inhabitants)
districts$unemploymant.rate..95 <- as.numeric(districts$unemploymant.rate..95)
districts$unemploymant.rate..96 <- as.numeric(districts$unemploymant.rate..96)
districts$no..of.commited.crimes..95 <- as.integer(districts$no..of.commited.crimes..95)

districts$diffUnemploymant <- districts$unemploymant.rate..96 - districts$unemploymant.rate..95
districts$avgUnemploymant <- (districts$unemploymant.rate..96 + districts$unemploymant.rate..95) / 2

districts$diffCrimes <- districts$no..of.commited.crimes..96 - districts$no..of.commited.crimes..95
districts$avgCrimes <- (districts$no..of.commited.crimes..96 + districts$no..of.commited.crimes..95) / 2
districts$ratioCrimes <- districts$avgCrimes / districts$no..of.inhabitants

districts$no..of.municipalities.with.inhabitants...499 <- NULL
districts$no..of.municipalities.with.inhabitants.500.1999 <- NULL
districts$no..of.municipalities.with.inhabitants.2000.9999 <- NULL
districts$no..of.municipalities.with.inhabitants..10000 <- NULL
districts$no..of.cities <- NULL

all_info_train <- merge(all_info_train, districts, by.x = "district_id", by.y = "code", all.x = TRUE)
all_info_test <- merge(all_info_test, districts, by.x = "district_id", by.y = "code", all.x = TRUE)

# -----------------------------------------------------------------
# load loans
loans_train <- read.csv2("Processed Data Set/loan_train.csv", TRUE, stringsAsFactors = FALSE)
loans_test <- read.csv2("Processed Data Set/loan_test.csv", TRUE, stringsAsFactors = FALSE)

all_info_train <- merge(all_info_train, loans_train, by.x = "account_id", by.y = "account_id", all.x = TRUE)
all_info_test <- merge(all_info_test, loans_test, by.x = "account_id", by.y = "account_id", all.x = TRUE)

all_info_train$date <- NULL
all_info_test$date <- NULL

all_info_train$amount[is.na(all_info_train$amount)] <- 0
all_info_train$duration[is.na(all_info_train$duration)] <- 0
all_info_train$payments[is.na(all_info_train$payments)] <- 0
all_info_train$status[is.na(all_info_train$status)] <- 0

all_info_test$amount[is.na(all_info_test$amount)] <- 0
all_info_test$duration[is.na(all_info_test$duration)] <- 0
all_info_test$payments[is.na(all_info_test$payments)] <- 0
all_info_test$status[is.na(all_info_test$status)] <- 0

all_info_train$amount <- as.integer(all_info_train$amount)
all_info_train$duration <- as.integer(all_info_train$duration)
all_info_train$payments <- as.integer(all_info_train$payments)
all_info_train$status <- as.integer(all_info_train$status)

all_info_test$amount <- as.integer(all_info_test$amount)
all_info_test$duration <- as.integer(all_info_test$duration)
all_info_test$payments <- as.integer(all_info_test$payments)
all_info_test$status <- as.integer(all_info_test$status)

# -----------------------------------------------------------------
# load cards
cards_train <- read.csv2("Processed Data Set/card_train.csv", TRUE, stringsAsFactors = FALSE)
cards_test <- read.csv2("Processed Data Set/card_test.csv", TRUE, stringsAsFactors = FALSE)

all_info_train <- merge(all_info_train, dispositions, by.x = "owner_id", by.y = "client_id", all.x = TRUE)
all_info_test <- merge(all_info_test, dispositions, by.x = "owner_id", by.y = "client_id", all.x = TRUE)

all_info_train$account_id.y <- NULL
all_info_train$type <- NULL
all_info_test$account_id.y <- NULL
all_info_test$type <- NULL

colnames(all_info_train)[2] <- "account_id"
colnames(all_info_test)[2] <- "account_id"

all_info_train <- merge(all_info_train, cards_train, by.x = "disp_id", by.y = "disp_id", all.x = TRUE)
all_info_test <- merge(all_info_test, cards_test, by.x = "disp_id", by.y = "disp_id", all.x = TRUE)

# -----------------------------------------------------------------
# removing all ids

all_info_train$disp_id <- NULL
all_info_train$account_id <- NULL
all_info_train$district_id <- NULL
all_info_train$loan_id <- NULL
all_info_train$card_id <- NULL
all_info_train$issued <- NULL

all_info_test$disp_id <- NULL
all_info_test$account_id <- NULL
all_info_test$district_id <- NULL
all_info_test$loan_id <- NULL
all_info_test$card_id <- NULL
all_info_test$issued <- NULL

all_info_train$type[is.na(all_info_train$type)] <- ""
all_info_test$type[is.na(all_info_test$type)] <- ""

# -----------------------------------------------------------------
# creating files

# writing operation
write.csv(all_info_train, file = "Processed Data Set/Processed/processed_train.csv", row.names = FALSE)
write.csv(all_info_test, file = "Processed Data Set/Processed/processed_test.csv", row.names = FALSE)
