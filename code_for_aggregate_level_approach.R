library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

library("expss") # this is a package I used for producing cross-tabulation
library("lubridate") # this package enables the `year` function and other date transformation function

d<-read.csv('data/merged_data.csv')

sapply(d, typeof)
colnames(d)

clean_data<-function(data){
  data[sapply(data, is.factor)] <- lapply(data[sapply(data, is.factor)], as.character)
  
  data <- data %>%
    
    replace(., .=="-", "") %>% # replace all dash to blank
    replace(., .=="", NA) %>% # replace all blanks to NA
    mutate(Incident.Date = as.Date(Incident.Date))%>%
    mutate(Date.Received = as.Date(Date.Received, "%d-%b-%y")) %>% # transfer the date field to prefered format
    mutate(Close.Amount = gsub("\\$", "",Close.Amount)) %>% # replace $ and ; in the claim amount field
    mutate(Claim.Amount = gsub("\\$", "", Claim.Amount)) %>%
    mutate(Close.Amount = gsub("\\,", "",Close.Amount)) %>%
    mutate(Claim.Amount = gsub("\\,", "", Claim.Amount)) %>%
    mutate(Close.Amount = as.numeric(Close.Amount)) %>%
    mutate(Claim.Amount = as.numeric(Claim.Amount))%>%
    filter (!is.na(Date.Received) & !is.na(Incident.Date))%>%
    # filter (!is.na(Status)) %>%
    # filter (!is.na(Disposition)) %>%
    # 
    
    #select(-Disposition)%>%
    
    arrange(Incident.Date) 
  
  #CONVERT MISSING VALUES TO 'UNK'
  data$Airport.Code[is.na(data$Airport.Code)]<-'UNK'
  data$Airport.Name[is.na(data$Airport.Name)]<-'UNK'
  data$Airline.Name[is.na(data$Airline.Name)]<-'UNK'
  data$year.received<-as.numeric(format(data$Date.Received,'%Y'))
  data$year.incident<-as.numeric(format(data$Incident.Date,'%Y'))
  
  data <- data %>%
    filter(year.incident>=2005)
  
  # if a case is denied, the correct closed amount should be zero
  data$Close.Amount[which(is.na(data$Close.Amount) & data$Status == "Denied")] <- 0 # replace values
  data$Status[which(is.na(data$Close.Amount) & data$Status == "Settled")] <- "Other" # 
  
  data$Claim.Site[is.na(data$Claim.Site) & is.na(data$Claim.Type)] <- "Other" # I decided to set them as =.
  data$Claim.Type[is.na(data$Claim.Site) & is.na(data$Claim.Type)] <- "Other"
  data$Claim.Type[is.na(data$Claim.Type)] <- "UNK"
  data$day.difference<-as.numeric(data$Date.Received-data$Incident.Date)
  
  data <- data %>%
    filter(day.difference>=0)
  
  return(data)
}


cleaned<-clean_data(d)

unique(cleaned$Claim.Type)

cleaned %>%
  count(year.incident) %>%
  ggplot(aes(x=year.incident,y=n))+
  geom_line()+
  #geom_area()+
  ylab("Total Claims")+
  scale_x_continuous(name = 'Years',breaks = seq(2005,2014,1))+
  ggtitle('Progress of total claims ')+
  theme_minimal()

head(cleaned,20)
tail(cleaned,1)

dist_tsa_claimes<-cleaned%>%
  group_by(day.difference)%>%
  summarise(count=n())%>%
  filter(day.difference<500)


#plpt_dist_tsa_claims<-
ggplot(dist_tsa_claimes,aes(x =day.difference ,y=count))+
  geom_line()+
  geom_area()+
  xlab("Time difference between incident and report of incident (Days)")+
  ylab("# of Claims")+
  ggtitle("Distribution of TSA claims")+
  theme_minimal()

test<-cleaned%>%
  group_by(day.difference,Disposition)%>%
  summarise(tsa_count=n())%>%
  filter (!is.na(Disposition))%>%
  filter(day.difference<800)%>%
  mutate(percent=tsa_count/sum(tsa_count))

ggplot(test,aes(x=day.difference,y=100*percent,fill=Disposition))+geom_area(stat = 'identity')+
  xlab("Time difference between incident and report of incident (Days)")+
  ylab("Percentage %")+
  ggtitle("Proportion of Dispositions for Each Claim Time")+
  theme_minimal()

zoom_test<-test%>%
  filter(day.difference<=60)


ggplot(zoom_test,aes(x=day.difference,y=100*percent,fill=Disposition))+geom_area(stat = 'identity')+
  xlab("Time difference between incident and report of incident (Days)")+
  ylab("Percentage %")+
  ggtitle("Proportion of Dispositions for Each Claim Time (First 60 days)")+
  theme_minimal()

unique(cleaned$Claim.Site)

#not NA in Disposition
filter_na_Disposition<-cleaned %>%
  filter (!is.na(Disposition))

filter_na_Site<-cleaned %>%
  filter (!is.na(Claim.Site))

unique(filter_na_Site$Claim.Site)


to_plot<-filter_na_Disposition %>%
  group_by(year.incident) %>%
  count(Disposition) %>%
  mutate(percent=n/sum(n))


to_plot



ggplot(to_plot,aes(group=Disposition, y=percent, x=as.character(year.incident))) + 
  geom_col(aes(fill = Disposition))+
  geom_text(aes(label = round(100*percent, digits = 2)), position = position_stack(vjust = 0.5))+
  # scale_x_discrete(breaks=1:length(unique(as.character(to_plot$year.incident))),
  #                    labels=unique(as.character(to_plot$year.incident)))+
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+
  ggtitle("Percentage of each status for each year")+
  theme_minimal()



to_plot2<-cleaned %>%
  group_by(year.incident) %>%
  count(Claim.Type) %>%
  mutate(percent=n/sum(n))
  

to_plot2$Claim.Type[to_plot2$percent<0.1]='Other'

to_plot2<-to_plot2 %>%
  group_by(year.incident,Claim.Type) %>%
  summarise(percent = sum(percent))
  




ggplot(to_plot2,aes(group=Claim.Type, y=percent, x=as.character(year.incident))) + 
  geom_col(aes(fill = Claim.Type))+
  geom_text(aes(label = round(100*percent, digits = 2)), position = position_stack(vjust = 0.5))+
  # scale_x_discrete(breaks=1:length(unique(as.character(to_plot$year.incident))),
  #                    labels=unique(as.character(to_plot$year.incident)))+
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+
  ggtitle("Percentage of claim incidents for each year")+
  theme_minimal()


to_plot3<-filter_na_Site %>%
  group_by(year.incident) %>%
  count(Claim.Site) %>%
  mutate(percent=n/sum(n))


to_plot3$Claim.Site[to_plot3$percent<0.1]='Other'

to_plot3<-to_plot3 %>%
  group_by(year.incident,Claim.Site) %>%
  summarise(percent = sum(percent))


to_plot3

ggplot(to_plot3,aes(group=Claim.Site, y=percent, x=as.character(year.incident))) + 
  geom_col(aes(fill = Claim.Site))+
  geom_text(aes(label = round(100*percent, digits = 2)), position = position_stack(vjust = 0.5))+
  # scale_x_discrete(breaks=1:length(unique(as.character(to_plot$year.incident))),
  #                    labels=unique(as.character(to_plot$year.incident)))+
  coord_flip()+
  xlab("Year")+
  ylab("Percentage")+
  ggtitle("Percentage of claim sites where incidents occured for each year")+
  theme_minimal()





