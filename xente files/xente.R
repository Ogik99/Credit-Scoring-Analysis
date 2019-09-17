#Clear Working space 
rm(list= ls())

#setting working directory
working_directory <- choose.dir()
setwd(working_directory)
getwd()

#Loading  required library (Note: Ensure all the libraries are installed if not use install.packages command to install)
library(tidyverse)
library(visdat)

#loading data
train <- as.data.frame(read_csv("train.csv"))
unlinked_data <- as.data.frame(read_csv("unlinked.csv"))

#Getting an overview of the data
data_list <- list(train, unlinked_data)

View(train)
View(unlinked_data)

#inspecting the internal structure of the data
for (dta in data_list){
  print(str(dta))}

#alternatively we can use map function from purrr package
map(data_list, str)

#Getting a general visual overview of the dataset
for (dta in data_list){
  print(vis_dat(dta))}

#Getting an overview of missing data
for (dta in data_list){
  print(vis_miss(dta))}

#checking for common variables/columns
which(names(unlinked_data) %in% names(train))
which(names(train) %in% names(unlinked_data))

#identifying variable to use as key 
train %>%
  count(CustomerId, TransactionId, BatchId,ProviderId) %>%
  filter(n > 1)

#removing Country code and Currency Code columns because they are redundant
index_country <- which(names(train) == "CountryCode")
index_currency <- which(names(train) == "CurrencyCode")

train <- train %>%
  select(-(CurrencyCode:CountryCode)) #option 1

train <-  train[, c(-index_country, -index_currency)] #option 2

unlinked_data <- unlinked_data %>% #option 3
  select(-(CurrencyCode:CountryCode))

#create a new column of transaction charges
train$Amount <- abs(train$Amount)

train <- train %>%
  mutate(TransactionCost = Value - Amount)
  
#replacing values in the `IsDefaulted` variable
train$IsDefaulted <- ifelse(train$IsDefaulted == 1, "default", "non-default")
train$IsDefaulted <- factor(train$IsDefaulted, levels = c(1,0), labels = c("default", "non-default")) #alternative method of replacing values

#converting "character" variables categorical variables
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)

train$IsFinalPayBack <- as.factor(train$IsFinalPayBack)
train$IsThirdPartyConfirmed <- as.factor(train$IsThirdPartyConfirmed)

#alternative method of converting non-numeric variables to factors
num_var <- c("TransactionStartTime","Value","Amount","AmountLoan","TransactionCost")
num_vec <- which(names(train) %in% num_var)
train[,-(num_vec)] <- map(train[,-(num_vec)], as.factor)

#visual overview of outliers
par(mfrow = c(2,2)) #setting a 2 by 2 plotting space
for (ivar in num_index){
  x <- train[,ivar]
  boxplot(x)
  title(main = names(train)[ivar])}
