#import and save
library(tidyverse)
library(readbulk)
library(lubridate)
library(zoo)

options("digits.secs"=6)

#Combine all data files into one data frame
dft = readbulk::read_bulk('data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)
#create a row number to keep track of things
dft$rowNum<-1:nrow(dft)
#Make a new testID based on the file name
dft$testID<-as.numeric(gsub("[^0-9.-]", "", substr(dft$File,7,10)))

#Delete Test.ID Columns as the fist 18 are wrong
dft$Test.ID<-NULL


dft<-dft[order(dft$testID),]

#Make a total time in seconds
#dft$timeSinceStart<-as.double(substr(dft$Timer,9,16))

#Rearrange the columns 
#dft<-dft[,c(18:21,22,1:17,22)]

#Make FOD into a factor
dft$FOD<-as.factor(dft$FOD)

#Rename Columns
dft<-rename(dft, Range = Detection_range_in_Meters) 
dft<-rename(dft, day = Day_nr.) 

#count collisions
dft$Object_collision <- gsub('null', '', dft$Object_collision)
dft$objcollBefore <- c('',dft[1:(nrow(dft)-1),]$Object_collision)
dft$objColl <- ifelse(substr(dft$Object_collision,1,1) == "B" & dft$objcollBefore == '',1,0)

#count detections
dft$Object_detected<-gsub('null', '', dft$Object_detected)
dft$objDetBefore<-c('',dft[1:(nrow(dft)-1),]$Object_detected) 
dft$objDet<-ifelse(substr(dft$Object_detected,1,1) == "B" & dft$objDetBefore == '',1,0)
dft$objDetStop<-ifelse(substr(dft$objDetBefore,1,1) == "B" & dft$Object_detected == '',1,0)

#Use time stamp to calculate how long a test took
dft$Time_stamp<- as.POSIXct(dft$Time_stamp, format="%m/%d/%Y %H:%M:%S")
#dft<-dft[order(dft$Time_stamp),]

#This looks like it is just counting up at each row.
#dft$timeSinceExpStarted<-time(dft$Time_stamp) - min(time(dft$Time_stamp))

#convert the timer into time in seconds
dft$Time_in_MS <- as.POSIXct(dft$Timer, format="%H:%M:%OS")
dft$Time_in_MS <- second(dft$Time_in_MS)

#Make a total time in seconds
#dft$timeSinceStart<-as.double(substr(dft$Timer,1,16))


dft$NewTimer<-lag(dft$Time_in_MS,1)
dft$ScenarioBefore<-lag(dft$Scenario,default=99999)
dft$ScenarioStarts<-ifelse(!(dft$Scenario==dft$ScenarioBefore),1,0)
dft$RunningScenarioCounter <-cumsum(dft$ScenarioStarts)

# gsub('_', '-', data1$c)
# dft$te
# helper$FOD<- as.factor(helper,levels = c("Baseline", "Corridor", "large")
load(file='data_all_Training.rda')

#Make data frame into a .rda file for faster running time

# add consistentTimeline
dft$newTestStarts<-ifelse(dft$testID>lag(dft$testID,default=0),1,0)
dft$ObjDetID <- cumsum(dft$objDet+dft$newTestStarts)
dft$RunningTime <- dft$Time_in_MS
dft$TimeSincePreRow <- ifelse(dft$NewTimer>dft$RunningTime, 0, dft$RunningTime-dft$NewTimer)
dft[1,]$TimeSincePreRow = 0
dft$GapObjDetID<-cumsum(dft$objDetStop+dft$newTestStarts)

dft$GapObjDetID<-ifelse(substr(dft$Object_detected,1,1) == "B",NA,dft$GapObjDetID)
dft$ActObjectDetID<-ifelse(substr(dft$Object_detected,1,1) == "B",dft$ObjDetID,NA)

dfx<-
  dft%>% select(GapObjDetID, TimeSincePreRow,rowNum) %>% group_by(GapObjDetID) %>% summarise(Gapdurationx=sum(TimeSincePreRow)) %>% View() 
  right_join(dft) %>% arrange(rowNum)%>% slice(5992:6008)%>%View()

#dft$ObjDetChangeHlp <- lag(dft$Object_detected)



# create median smoothed speed column 
dft %<>% group_by(testID) %>% mutate(rollingSpeedMedian=rollmedian(x=Person_Speed,k=5,fill=NA,align = "left"))%>%ungroup()

save(dft, file='data_all_Training.rda', compress=TRUE)



