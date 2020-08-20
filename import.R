#import and save
library(tidyverse)
library(readbulk)

#Combine all data files into one data frame
df = readbulk::read_bulk('data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

#Make a new testID based on the file name
df$testID<-as.numeric(gsub("[^0-9.-]", "", substr(df$File,7,10)))

#Delete Test.ID Columns
df$Test.ID<-NULL

#Make a total time
df$timeSinceStart<-as.double(substr(df$Timer,9,16))*1000


df<-df[,c(18:21,23,1:17,22)]

#Make FOD into a factor
df$FOD<-as.factor(df$FOD)

#Rename Columns
df<-rename(df, Range = Detection_range_in_Meters) 
df<-rename(df, day = Day_nr.) 

#count collisions
df$Object_collision<-gsub('null', '', df$Object_collision)
df$objBefore<-c('',df[1:(nrow(df)-1),]$Object_collision)
df$objColl<-ifelse(substr(df$Object_collision,1,1)=="B" & df$objBefore=='',1,0)

#count detections
df$Object_detected<-gsub('null', '', df$Object_detected)
df$objDetectBefore<-c('',df[1:(nrow(df)-1),]$Object_detected) 
df$objDet<-ifelse(substr(df$Object_detected,1,1)=="B" & df$objDetectBefore=='',1,0)

#Use time stamp to calculate how long a test took
df$Time_stamp<- as.POSIXct(df$Time_stamp,format="%d/%m/%Y %H:%M:%S")
df<-df[order(df$Time_stamp),]
df$timeSinceExpStarted<-time(df$Time_stamp) -min(time(df$Time_stamp))

df$NewTimer<-lag(df$Timer,1)
df$ScenarioBefore<-lag(df$Scenario,default=99999)
df$ScenarioStarts<-ifelse(!(df$Scenario==df$ScenarioBefore),1,0)
df$RunningScenarioCounter <-cumsum(df$ScenarioStarts)

# gsub('_', '-', data1$c)
# df$te
# helper$FOD<- as.factor(helper,levels = c("Baseline", "Corridor", "large")

#Make data frame into a .rda file for faster running time

# add consistentTimeline

df$ObjDetID<-cumsum(df$objDet)
df$RunningTime <- cumsum(df$Time_in_MS*1000)


df$ObjDetChangeHlp <- lag(df$Object_detected)

save(df, file='data_all.rda', compress=TRUE)



