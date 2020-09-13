#import and save
library(tidyverse)
library(readbulk)
library(lubridate)
library(zoo)

options("digits.secs"=6)

# Combine all data files into one data frame
dfp = readbulk::read_bulk('dataParticipants', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

# Sort the data based on test ID
dfp<-rename(dfp, testID = Test.ID) 
dfp<-dfp[order(dfp$testID),]

# Make a total time in seconds
# dfp$timeSinceStart<-as.double(substr(dfp$Timer,9,16))

# Rearrange the columns 
# dfp<-dfp[,c(18:21,22,1:17,22)]

# Make FOD into a factor
dfp$FOD<-as.factor(dfp$FOD)

#Rename Columns
dfp<-rename(dfp, Range = Detection_range_in_Meters) 
dfp<-rename(dfp, day = Day_nr.) 
dfp<-rename(dfp, ParticipantID = Participant.ID) 

#count collisions
dfp$Object_collision <- gsub('null', '', dfp$Object_collision)
dfp$objcollBefore <- c('',dfp[1:(nrow(dfp)-1),]$Object_collision)
dfp$objColl <- ifelse(substr(dfp$Object_collision,1,1) == "B" & dfp$objcollBefore == '',1,0)

#count detections
dfp$Object_detected<-gsub('null', '', dfp$Object_detected)
dfp$objDetBefore<-c('',dfp[1:(nrow(dfp)-1),]$Object_detected) 
dfp$objDet<-ifelse(substr(dfp$Object_detected,1,1) == "B" & dfp$objDetBefore == '',1,0)
dfp$objDetStop<-ifelse(substr(dfp$objDetBefore,1,1) == "B" & dfp$Object_detected == '',1,0)

#Use time stamp to calculate how long a test took
dfp$Time_stamp<- as.POSIXct(dfp$Time_stamp, format="%m/%d/%Y %H:%M:%S")
#dfp<-dfp[order(dfp$Time_stamp),]

#This looks like it is just counting up at each row.
#dfp$timeSinceExpStarted<-time(dfp$Time_stamp) - min(time(dfp$Time_stamp))

#convert the timer into time in seconds
dfp$Time_in_S <- as.POSIXct(dfp$Timer, format="%H:%M:%OS")
dfp$Time_in_S <- second(dfp$Time_in_S)

#Make a total time in seconds
#dfp$timeSinceStart<-as.double(substr(dfp$Timer,1,16))


dfp$NewTimer<-lag(dfp$Time_in_S,1)
dfp$ScenarioBefore<-lag(dfp$Scenario,default=99999)
dfp$ScenarioStarts<-ifelse(!(dfp$Scenario==dfp$ScenarioBefore),1,0)
dfp$RunningScenarioCounter <-cumsum(dfp$ScenarioStarts)

# gsub('_', '-', data1$c)
# dfp$te
# helper$FOD<- as.factor(helper,levels = c("Baseline", "Corridor", "large")

#Make data frame into a .rda file for faster running time

# add consistentTimeline

dfp$ObjDetID <- cumsum(dfp$objDet)
dfp$RunningTime <- dfp$Time_in_S


#dfp$ObjDetChangeHlp <- lag(dfp$Object_detected)


#create a row number to keep track of things
dfp$rowNum<-1:nrow(dfp)
# create median smoothed speed column 
dfp %<>% group_by(testID) %>% mutate(rollingSpeedMedian=rollmedian(x=Person_Speed,k=5,fill=NA,align = "left"))%>%ungroup()

dfp$Range[dfp$Range == "0.7"] <- "1"
dfp$Range <-as.numeric(dfp$Range)


dfp$ParticipantID <-ifelse(dfp$testID > 420 & dfp$testID < 446, 2, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 445 & dfp$testID < 471, 3, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 470 & dfp$testID < 496, 4, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 495 & dfp$testID < 521, 5, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 520 & dfp$testID < 546, 6, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 545 & dfp$testID < 571, 7, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 570 & dfp$testID < 596, 8, dfp$ParticipantID)
dfp$ParticipantID <-ifelse(dfp$testID > 595 & dfp$testID < 621, 9, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 620 & dfp$testID < 646, 10, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 645 & dfp$testID < 671, 11, dfp$ParticipantID) 


save(dfp, file='data_all_Participants.rda', compress=TRUE)




