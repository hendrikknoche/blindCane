#import and save
library(tidyverse)
library(readbulk)
df = readbulk::read_bulk('data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

df$testID<-as.numeric(gsub("[^0-9.-]", "", substr(df$File,7,10)))
df$Test.ID<-NULL
df$timeSinceStart<-as.double(substr(df$Timer,9,16))*1000
df<-df[,c(18:21,23,1:17,22)]
df$FOD<-as.factor(df$FOD)
df<-rename(df, Range = Detection_range_in_Meters) 
df<-rename(df, day = Day_nr.) 
df$Object_collision<-gsub('null', '', df$Object_collision)
df$objBefore<-c('',df[1:(nrow(df)-1),]$Object_collision)
df$objColl<-ifelse(substr(df$Object_collision,1,1)=="B" & df$objBefore=='',1,0)
df$Time_stamp<- as.POSIXct(df$Time_stamp,format="%d/%m/%Y %H:%M:%S")
df<-df[order(df$Time_stamp),]
df$timeSinceExpStarted<-time(df$Time_stamp) -min(time(df$Time_stamp))
# gsub('_', '-', data1$c)
# df$te
# helper$FOD<- as.factor(helper,levels = c("Baseline", "Corridor", "large")
save(df, file='data_all.rda', compress=TRUE)



