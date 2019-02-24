# load libraries & NOAA data
library(ggplot2)
library(reshape2)
setwd("/home/alanoakes/Documents/Programming Notes/DataScience/Mod5 - Repoducible Research/NoaaStormData")
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url=url,destfile="repdata_data_StormData.csv.bz2")
NOAA.Data<-read.csv("repdata_data_StormData.csv.bz2")
list("Dimensions of Data Set"=dim(NOAA.Data),
     "% NA's in Data Set"=mean(is.na(NOAA.Data)),
     "Total Count of NA's of Data Set"=sum(is.na(NOAA.Data)),
     "Variable Names"=names(NOAA.Data))

# Preprocessing Data to Answer both Questions
# create sample of top ten event types based off frequency
TopTen<-table(NOAA.Data$EVTYPE)
TopTenD<-as.data.frame(TopTen)
TopTenL<-head(TopTenD[order(-TopTenD$Freq),],10)
EventTypes<-TopTenL$Var1
EventTypes<-as.character(EventTypes)
NOAA.Data$EVTYPE<-as.character(NOAA.Data$EVTYPE)
# Question 1
Question1<-NOAA.Data[NOAA.Data$EVTYPE==EventTypes,c(8,23,24)]
names(Question1)<-c("EventType","Fatalities","Injuries")
Question1$EventType<-as.factor(Question1$EventType)
Question1$Fatalities<-as.numeric(Question1$Fatalities)
Question1$Injuries<-as.numeric(Question1$Injuries)
Q1Melt<-melt(Question1,na.rm=T)
# Question 2
Question2<-NOAA.Data[NOAA.Data$EVTYPE==EventTypes,c(8,26,28)]
names(Question2)<-c("EventType","PropDamage","CropDamage")
Question2$EventType<-as.factor(Question2$EventType)
Question2$PropDamage<-as.numeric(Question2$PropDamage)
Question2$CropDamage<-as.numeric(Question2$CropDamage)
Q2Melt<-melt(Question2,na.rm=T)

# Results
# plot
ggplot(data=Q1Melt,aes(x=EventType,y=value,fill=variable))+
  geom_bar(stat="identity")+coord_flip()+theme_linedraw()
# table
Q1<-as.data.frame(tapply(Q1Melt$value,Q1Melt$EventType,sum))
as.data.frame(Q1[order(-Q1),])

ggplot(data=Q2Melt,aes(x=EventType,y=value,fill=variable))+
  geom_bar(stat="identity",position="stack")+
  coord_flip()+theme_linedraw()
# table
Q2<-as.data.frame(tapply(Q2Melt$value,Q2Melt$EventType,sum))
as.data.frame(Q2[order(-Q2),])
# compare to most expensive against most occuring
NOAA.Data$EVTYPE<-as.character(NOAA.Data$EVTYPE)
NOAA.Data$PROPDMGEXP<-as.numeric(NOAA.Data$PROPDMGEXP)
NOAA.Data$CROPDMGEXP<-as.numeric(NOAA.Data$CROPDMGEXP)
Expense<-NOAA.Data$PROPDMGEXP+NOAA.Data$CROPDMGE
ExpEvents<-cbind.data.frame(NOAA.Data$REFNUM,NOAA.Data$EVTYPE,NOAA.Data$PROPDMGEXP,NOAA.Data$CROPDMGEXP)
ExpEvents$Expense<-ExpEvents$`NOAA.Data$PROPDMGEXP`+ExpEvents$`NOAA.Data$CROPDMGEXP`
x1<-tapply(ExpEvents$Expense,ExpEvents$`NOAA.Data$EVTYPE`,sum)
x1.1<-as.data.frame(x1)
head(as.data.frame(x1.1[order(-x1),]),10)
