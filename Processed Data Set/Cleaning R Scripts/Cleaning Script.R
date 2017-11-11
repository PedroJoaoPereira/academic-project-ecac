# set working directory
setwd("C:/Users/pedro/Desktop/Progs/Docs/5º Ano - 1º Semestre/ECAC/ECAC")

# loads data from csv
clients <- read.csv2("Processed Data Set/client.csv", TRUE)

# using subset function to filter clients by gender
females <- subset(clients,((clients$birth_number / 100) %% 100) > 50, 
select=c(client_id, birth_number, district_id))
males <- subset(clients,((clients$birth_number / 100) %% 100) < 50, 
select=c(client_id, birth_number, district_id))

# subtracts 5000 to females
females["birth_number"] <- females["birth_number"] - 5000

# add gender columns
females$gender <- "f"
males$gender <- "m"

# rebinds datasets
clients <- rbind(females, males)
clients <- clients[ with(clients, order(client_id)),]

# completes the birth year with 19XX
clients["birth_number"] <- clients["birth_number"] + 19000000

# parses field into date
clients$birth_number <- as.Date(as.character(clients$birth_number), "%Y%m%d")

# adds new field for the birth year to use in plotting
clients$birth_year <- as.numeric(format(clients$birth_number,"%Y"))

head(clients)