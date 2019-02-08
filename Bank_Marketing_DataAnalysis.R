#Import the bank datasets by using read.csv
bank_full <- read.csv("~/Documents/Study/Semester2/ML&Stats/bank/bank-full.csv")
bank_add <- read.csv("~/Documents/Study/Semester2/ML&Stats/bank-additional/bank-additional-full.csv")
bank <- bank_full


head(bank)
head(bank_add)
dim(bank)
dim(bank_add)
str(bank)
str(bank_add)
summary(bank)
summary(bank_add)
