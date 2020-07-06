library(plyr)
library(dplyr)
library(data.table)
library(countrycode)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(directlabels)
library(readxl)



y02.06<-read.csv('data/claims-2002-2006_0.csv')
y07.09<-read.csv('data/claims-2007-2009_0.csv')
y10.13<-read.csv('data/claims-2010-2013_0.csv')
y14<-read.csv('data/claims-2014.csv')

head(y02.06,8)
head(y14,1)

# colnames(y02.06)
# colnames(y07.09)
# colnames(y10.13)
# colnames(y14)

#claim amount is missing from y10.13,y14



factors_to_chars<-function(data){
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], as.character)
  return(data);
}

reformat_date<-function(data){
  data <- data %>%
    mutate(Incident.Date = as.Date(Incident.Date, "%m/%d/%Y"))
  return(data);
}




# Factorisation:
# I convert all factors to characters, which will facilitate the cleaning and replacement of missing values
y02.06<-factors_to_chars(y02.06)
y07.09<-factors_to_chars(y07.09)
y10.13<-factors_to_chars(y10.13)
y14<-factors_to_chars(y14)




#Realign column headers
colnames(y02.06)[1] <- "Claim.Number"
colnames(y07.09)[1] <- "Claim.Number"
colnames(y10.13)[1] <- "Claim.Number"
colnames(y14)[1] <- "Claim.Number"
colnames(y02.06)[9] <- "Item.Category"
colnames(y07.09)[9] <- "Item.Category"


# Reformat Date
y02.06<-reformat_date(y02.06)
head(y02.06,8)
y07.09<-reformat_date(y07.09)
y10.13<-reformat_date(y10.13)
y14<-y14%>%
  mutate(Incident.Date = as.Date(Incident.Date, "%d-%b-%y"))
head(y14,8)


# Add blank Claim Amount and Status columns in y10.13, y14 dataset
y <- rbind(y10.13, y14) %>%
  mutate (Status = NA) %>%
  mutate (Claim.Amount = NA) %>%
  select (Claim.Number, Date.Received, Incident.Date, Airport.Code, Airport.Name, Airline.Name, Claim.Type, Claim.Site, Item.Category, Claim.Amount, Status, Close.Amount, Disposition)

# Bind the y02.06 and y07.09 dataset
x <- rbind(y02.06, y07.09)


#Get number of missing values per column
sapply(y, function(x) sum(is.na(x)))

# Remove empty rows
y <- y %>%
  filter (Claim.Number != "") %>%
  filter (!is.na(Claim.Number)) # there were 2689 empty rows

# Binding
raw <- rbind(x,y) #  n = 204263
dim(raw)
#Get number of missing values per column
sapply(raw, function(x) sum(is.na(x)))

write.csv(raw,'data/merged_data.csv',row.names=FALSE)

