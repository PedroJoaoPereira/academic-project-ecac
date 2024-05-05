# set working directory
setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")

# -----------------------------------------------------------------
# loading data region
# loads data from csv
dispositions <- read.csv2("Processed Data Set/disp.csv", TRUE, stringsAsFactors = FALSE)
clients <- read.csv2("Processed Data Set/client.csv", TRUE, stringsAsFactors = FALSE)
accounts <- read.csv2("Processed Data Set/account.csv", TRUE, stringsAsFactors = FALSE)
districts <- read.csv2("Processed Data Set/district.csv", TRUE, stringsAsFactors = FALSE)

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
# merge datasets

# merging dispositions with clients
merged <- merge(dispositions, clients, by.x = "client_id", by.y = "client_id")
# removing irrelevant columns
merged$disp_id <- NULL
merged$district_id <- NULL

# merging merged with accounts
merged <- merge(merged, accounts, by.x = "account_id", by.y = "account_id")
# removing irrelevant columns
merged$date <- NULL

# merging merged with districts
merged <- merge(merged, districts, by.x = "district_id", by.y = "code")
# removing irrelevant columns
merged$region <- NULL
merged$no..of.municipalities.with.inhabitants...499 <- NULL
merged$no..of.municipalities.with.inhabitants.500.1999 <- NULL
merged$no..of.municipalities.with.inhabitants.2000.9999 <- NULL
merged$no..of.municipalities.with.inhabitants..10000 <- NULL
merged$no..of.cities <- NULL

str(merged)