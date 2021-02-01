#import and save
library(tidyverse)
library(readbulk)
library(lubridate)
library(zoo)

options("digits.secs"=6)

# Combine all data files into one data frame
dfp = readbulk::read_bulk('dataParticipants', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

#Save the imported files
save(dfp, file='data_Participants_Raw.rda', compress=TRUE)

#Load
load("data_Participants_Raw.rda")

# Make a new testID based on the file name
dfp$testID <- as.numeric(gsub("[^0-9.-]", "", substr(dfp$File, 7, 10)))

# Delete Test.ID Columns as the fist 18 are wrong
dfp$Test.ID <- NULL

# order data correctly #create a row number to keep track of things
dfp <- dfp %>%
  arrange(testID, Timer) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

# Use time stamp to calculate how long a test took
dfp$Time_stamp <- as.POSIXct(dfp$Time_stamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
dfp$Time_in_MS <- as.POSIXct(dfp$Timer, format = "%H:%M:%OS")
dfp$Time_in_MS <- second(dfp$Time_in_MS)+minute(dfp$Time_in_MS)*60+hour(dfp$Time_in_MS)*3660

# Rename Columns
dfp <- dplyr::rename(dfp, Range = Detection_range_in_Meters)
dfp <- dplyr::rename(dfp, day = Day_nr.)
dfp <- dplyr::rename(dfp, PersonSpeed = Person_Speed)
dfp <- dplyr::rename(dfp, TimeSeconds = Time_in_MS)
dfp <- dplyr::rename(dfp, TimeStamp = Time_stamp)
dfp <- dplyr::rename(dfp, ObjectDistance = Distance_to_object)
dfp <- dplyr::rename(dfp, ObjectCollision = Object_collision)
dfp <- dplyr::rename(dfp, ObjectDetected = Object_detected)
dfp <- dplyr::rename(dfp, ParticipantID = Participant.ID)

# Make FOD into a factor
dfp$FOD <- as.factor(dfp$FOD)
dfp$FOD<-recode_factor(dfp$FOD, Baseline="White Cane", WholeRoom="AWC: Conical View", Corridor="AWC: Tunnel View")
dfp$FOD <- factor(dfp$FOD, levels=c("White Cane", "AWC: Conical View", "AWC: Tunnel View"))

# Fix Range
dfp$Range[dfp$Range == 0.7] <- 1

#Re-name the participant ID
dfp$ParticipantID <-ifelse(dfp$testID > 420 & dfp$testID < 446, 1, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 445 & dfp$testID < 471, 2, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 470 & dfp$testID < 496, 3, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 495 & dfp$testID < 521, 4, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 520 & dfp$testID < 546, 5, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 545 & dfp$testID < 571, 6, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 570 & dfp$testID < 596, 7, dfp$ParticipantID)
dfp$ParticipantID <-ifelse(dfp$testID > 595 & dfp$testID < 621, 8, dfp$ParticipantID) 
dfp$ParticipantID <-ifelse(dfp$testID > 620 & dfp$testID < 646, 9, dfp$ParticipantID)  
dfp$ParticipantID <-ifelse(dfp$testID > 645 & dfp$testID < 671, 10, dfp$ParticipantID) 

# The time of the row above
dfp<- dfp %>% group_by(ParticipantID,testID) %>% mutate(NewTimer=lag(TimeSeconds,1))
# $NewTimer <- lag(dfp$Time_in_MS, 1)


# count collisions
dfp$ObjectCollision <- gsub("null", "", dfp$ObjectCollision)
dfp$objcollBefore <- c("", dfp[1:(nrow(dfp) - 1), ]$ObjectCollision) #shift one row
dfp$objColl <- ifelse(substr(dfp$ObjectCollision, 1, 1) == "B" & dfp$objcollBefore == "", 1, 0)
dfp$objCollStop <- ifelse(substr(dfp$ObjectCollision, 1, 1) == "B" & lead(dfp$ObjectCollision) == "", 1, 0)

# count detections
dfp$ObjectDetected <- gsub("null", "", dfp$ObjectDetected)
dfp$objDetBefore <- c("", dfp[1:(nrow(dfp) - 1), ]$ObjectDetected) #shift one row
dfp$objDet <- ifelse(substr(dfp$ObjectDetected, 1, 1) == "B" & dfp$objDetBefore == "", 1, 0)
dfp$objDetStop <- ifelse(substr(dfp$ObjectDetected, 1, 1) == "B" & lead(dfp$ObjectDetected) == "", 1, 0)

# Keep track on when a new scenario starts
dfp$ScenarioBefore <- lag(dfp$Scenario, default = 99999)
dfp$ScenarioStarts <- ifelse(!(dfp$Scenario == dfp$ScenarioBefore), 1, 0)
dfp$RunningScenarioCounter <- cumsum(dfp$ScenarioStarts)

# Add consistentTimeline
dfp$newTestStarts <- ifelse(dfp$testID > lag(dfp$testID, default = 0), 1, 0)
dfp$ObjDetID <- cumsum(dfp$objDet)
dfp$ObjDetIDTest <- cumsum(dfp$objDet + dfp$newTestStarts)

# dfp$RunningTime <- dfp$TimeSeconds
dfp$TimeSincePreRow <- ifelse(dfp$NewTimer > dfp$TimeSeconds, 0, dfp$TimeSeconds - dfp$NewTimer)
dfp[1, ]$TimeSincePreRow <- 0
dfp$GapObjDetID <- cumsum(dfp$objDetStop + dfp$newTestStarts)
dfp$totalTime <- cumsum(dfp$TimeSincePreRow)

# Calculate Gap duration  
dfp %<>%
  filter(substr(ObjectDetected, 1, 1) != "B") %>%
  select(GapObjDetID, TimeSincePreRow) %>%
  group_by(GapObjDetID) %>%
  dplyr::summarise(Gapduration = sum(TimeSincePreRow)) %>% 
  right_join(dfp) %>% arrange(rowNum) %>% relocate(Gapduration)

# Calculate detection duration  
dfp %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(ObjDetIDTest, TimeSincePreRow) %>%
  group_by(ObjDetIDTest) %>%
  dplyr::summarise(ObjDetDuration = sum(TimeSincePreRow)) %>% 
  right_join(dfp) %>% arrange(rowNum) %>% relocate(ObjDetDuration,ObjectDetected)

# create median smoothed speed column
dfp %<>% filter(PersonSpeed < 3) %>% group_by(testID) %>%
  mutate(rollingSpeedMedian = rollmedian(x = PersonSpeed, k = 5, fill = NA, align = "left")) %>%
  ungroup()

# analysis on how detections affect speed
onsets <- dfp %>%
  filter(objDet == 1) %>%
  select(ObjDetIDTest,
         VibStartTime = TimeSeconds,
         SpeedAtVibStart = rollingSpeedMedian)

offsets <- dfp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetIDTest,
         VibStopTime = TimeSeconds,
         SpeedAtVibStop = rollingSpeedMedian
  )

#mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
#mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)

dfp <- left_join(dfp, onsets)
dfp <- left_join(dfp, offsets)

dfp$TimeSinceVibStart <- dfp$TimeSeconds - dfp$VibStartTime
dfp$TimeSinceVibStop <- ifelse(substr(dfp$ObjectDetected, 1, 1) == "B",NA, dfp$TimeSeconds - dfp$VibStopTime)

dfp$SpeedDiffFromStart <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStart
dfp$SpeedDiffFromStop <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStop

# Rearrange the columns
col_order <- c("rowNum", "day", "ParticipantID", "testID", "newTestStarts", "Range", "FOD", 
               "Scenario", "ScenarioStarts", "ScenarioBefore", "RunningScenarioCounter",                
               "TimeStamp", "Timer", "TimeSeconds", "NewTimer", "TimeSincePreRow", "totalTime",           
               "ObjectCollision", "objColl", "objcollBefore", "objCollStop",                      
               "ObjectDetected", "objDetBefore", "objDet", "ObjDetID", "ObjDetIDTest", "objDetStop", "ObjectDistance", "ObjDetDuration", "GapObjDetID", "Gapduration",
               "VibStartTime", "VibStopTime", "TimeSinceVibStart", "TimeSinceVibStop", "SpeedDiffFromStart", "SpeedDiffFromStop",                   
               "PersonSpeed", "rollingSpeedMedian", "SpeedAtVibStart", "SpeedAtVibStop",
               "Person_pos_X", "Person_pos_Y", "Person_orientation", 
               "Whitecane_pos_X", "Whitecane_pos_Y", "Whitecane_orientation", 
               "Object_pos_X", "Object_pos_Y", 
               "File" )

dfp <- dfp[, col_order]

# Make data frame into a .rda file for faster running time
save(dfp, file='data_Participants_All.rda', compress=TRUE)

# Group Data set based on TestID
dfpSumTestID <- dfp %>% 
  filter(PersonSpeed<3)%>%
  group_by(testID, day, Scenario, FOD, Range)%>%
  dplyr::summarize(avgSpeed = mean(PersonSpeed),
            medianSpeed = median(PersonSpeed),
            maxSpeed = max(PersonSpeed),
            minSpeed = min(PersonSpeed),
            objectDetected = sum(objDet,na.rm = TRUE),
            objectCollisions = sum(objColl,na.rm = TRUE),
            Time = max(TimeSeconds))%>% 
  arrange(testID)

#add Coloum with sum of total time spent 
dfpSumTestID$totalTimeTraining<-round(cumsum(dfpSumTestID$Time))

#add Coloum with total time spent for a given FOD with a given Range
dfpSumTestID <- dfpSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
dfpSumTestID <- dfpSumTestID%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
dfpSumTestID <- dfpSumTestID%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

# Save the grouped data
save(dfpSumTestID, file = "data_Participants_SumTestID.rda", compress = TRUE)


