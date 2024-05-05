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

# -----------------------------------------------------------------
# load dispositions
dispositions <- read.csv2("Processed Data Set/disp.csv", TRUE, stringsAsFactors = FALSE)

# merge clients and districts
clients_districts <- merge(clients, districts, by.x = "district_id", by.y = "code")

# merging with dispositions
dispositions_m <- merge(clients_districts, dispositions, by = "client_id")
dispositions_m <- subset(dispositions_m, dispositions_m$type != "DISPONENT")

dispositions_m$type <- NULL
dispositions_m$district_id <- NULL

all_train$account_id <- NULL
all_test$account_id <- NULL

# merging with andre processed
all_info_train <- merge(dispositions_m, all_train, by.x = "account_id", by.y = "owner_account_id", all.y = TRUE)
all_info_test <- merge(dispositions_m, all_test, by.x = "account_id", by.y = "owner_account_id", all.y = TRUE)

all_info_train$district_id <- NULL
all_info_train$date <- NULL
all_info_train$gender.y <- NULL
all_info_train$owner_account_age <- NULL

colnames(all_info_train)[3] <- "gender"

all_info_test$district_id <- NULL
all_info_test$date <- NULL
all_info_test$gender.y <- NULL
all_info_test$owner_account_age <- NULL

colnames(all_info_test)[3] <- "gender"

# -----------------------------------------------------------------
# load loans
loans_train <- read.csv2("Processed Data Set/loan_train.csv", TRUE, stringsAsFactors = FALSE)
loans_test <- read.csv2("Processed Data Set/loan_test.csv", TRUE, stringsAsFactors = FALSE)

all_info_train <- merge(all_info_train, loans_train, by.x = "account_id", by.y = "account_id", all.x = TRUE)

all_info_train$loan_id <- NULL
all_info_train$date <- NULL

all_info_test <- merge(all_info_test, loans_test, by.x = "account_id", by.y = "account_id", all.x = TRUE)

all_info_test$loan_id <- NULL
all_info_test$date <- NULL

# -----------------------------------------------------------------
# load cards
cards_train <- read.csv2("Processed Data Set/card_train.csv", TRUE, stringsAsFactors = FALSE)
cards_test <- read.csv2("Processed Data Set/card_test.csv", TRUE, stringsAsFactors = FALSE)

all_info_train <- merge(all_info_train, cards_train, by.x = "disp_id", by.y = "disp_id", all.x = TRUE)

all_info_train$card_id <- NULL
all_info_train$issued <- NULL

all_info_test <- merge(all_info_test, cards_test, by.x = "disp_id", by.y = "disp_id", all.x = TRUE)

all_info_test$card_id <- NULL
all_info_test$issued <- NULL

# -----------------------------------------------------------------
# removing all ids

all_info_train$disp_id <- NULL
all_info_train$account_id <- NULL
all_info_train$client_id <- NULL

all_info_test$disp_id <- NULL
all_info_test$account_id <- NULL
all_info_test$client_id <- NULL

# -----------------------------------------------------------------
# creating files

# writing operation
write.csv(all_info_train, file = "Processed Data Set/Processed/processed_train.csv", row.names = FALSE)
write.csv(all_info_test, file = "Processed Data Set/Processed/processed_test.csv", row.names = FALSE)