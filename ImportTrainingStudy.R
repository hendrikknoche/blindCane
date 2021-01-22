# import and save
library(tidyverse)
library(magrittr)
library(readbulk)
library(lubridate)
library(zoo)
library("tibble")

options("digits.secs" = 6)

# Combine all data files into one data frame
dft <- readbulk::read_bulk("data", sep = ";", na.strings = "none", stringsAsFactors = FALSE)

#Save the imported files
save(dft, file = "data_Training_Raw.rda", compress = TRUE)

#Load
load("data_Training_Raw.rda")

# Make a new testID based on the file name
dft$testID <- as.numeric(gsub("[^0-9.-]", "", substr(dft$File, 7, 10)))

# Delete Test.ID Columns as the fist 18 are wrong
dft$Test.ID <- NULL

# order data correctly #create a row number to keep track of things
dft <- dft %>%
  arrange(testID, Timer) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

# Use time stamp to calculate how long a test took
dft$Time_stamp <- as.POSIXct(dft$Time_stamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
dft$Time_in_MS <- as.POSIXct(dft$Timer, format = "%H:%M:%OS")
dft$Time_in_MS <- second(dft$Time_in_MS)

# The time of the row above
dft$NewTimer <- lag(dft$Time_in_MS, 1)

# Rename Columns
dft <- rename(dft, Range = Detection_range_in_Meters)
dft <- rename(dft, day = Day_nr.)
dft <- rename(dft, PersonSpeed = Person_Speed)
dft <- rename(dft, TimeSeconds = Time_in_MS)
dft <- rename(dft, TimeStamp = Time_stamp)
dft <- rename(dft, ObjectDistance = Distance_to_object)
dft <- rename(dft, ObjectCollision = Object_collision)
dft <- rename(dft, ObjectDetected = Object_detected)
dft <- rename(dft, ParticipantID = Participant.ID)

# Make FOD into a factor
dft$FOD <- as.factor(dft$FOD)

# count collisions
dft$ObjectCollision <- gsub("null", "", dft$ObjectCollision)
dft$objcollBefore <- c("", dft[1:(nrow(dft) - 1), ]$ObjectCollision) #shift one row
dft$objColl <- ifelse(substr(dft$ObjectCollision, 1, 1) == "B" & dft$objcollBefore == "", 1, 0)
dft$objCollStop <- ifelse(substr(dft$ObjectCollision, 1, 1) == "B" & lead(dft$ObjectCollision) == "", 1, 0)

# count detections
dft$ObjectDetected <- gsub("null", "", dft$ObjectDetected)
dft$objDetBefore <- c("", dft[1:(nrow(dft) - 1), ]$ObjectDetected) #shift one row
dft$objDet <- ifelse(substr(dft$ObjectDetected, 1, 1) == "B" & dft$objDetBefore == "", 1, 0)
dft$objDetStop <- ifelse(substr(dft$ObjectDetected, 1, 1) == "B" & lead(dft$ObjectDetected) == "", 1, 0)

# Keep track on when a new scenario starts
dft$ScenarioBefore <- lag(dft$Scenario, default = 99999)
dft$ScenarioStarts <- ifelse(!(dft$Scenario == dft$ScenarioBefore), 1, 0)
dft$RunningScenarioCounter <- cumsum(dft$ScenarioStarts)

# Add consistentTimeline
dft$newTestStarts <- ifelse(dft$testID > lag(dft$testID, default = 0), 1, 0)
dft$ObjDetID <- cumsum(dft$objDet)
dft$ObjDetIDTest <- cumsum(dft$objDet + dft$newTestStarts)
# dft$RunningTime <- dft$TimeSeconds
dft$TimeSincePreRow <- ifelse(dft$NewTimer > dft$TimeSeconds, 0, dft$TimeSeconds - dft$NewTimer)
dft[1, ]$TimeSincePreRow <- 0
dft$GapObjDetID <- cumsum(dft$objDetStop + dft$newTestStarts)
dft$totalTime <- cumsum(dft$TimeSincePreRow)

# Calculate Gap duration  
dft %<>%
  filter(substr(ObjectDetected, 1, 1) != "B") %>%
  select(GapObjDetID, TimeSincePreRow) %>%
  group_by(GapObjDetID) %>%
  summarise(Gapduration = sum(TimeSincePreRow)) %>% 
  right_join(dft) %>% arrange(rowNum) %>% relocate(Gapduration)

# Calculate detection duration  
dft %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(ObjDetIDTest, TimeSincePreRow) %>%
  group_by(ObjDetIDTest) %>%
  summarise(ObjDetDuration = sum(TimeSincePreRow)) %>% 
  right_join(dft) %>% arrange(rowNum) %>% relocate(ObjDetDuration,ObjectDetected)

# create median smoothed speed column
dft %<>% filter(PersonSpeed < 3) %>% group_by(testID) %>%
  mutate(rollingSpeedMedian = rollmedian(x = PersonSpeed, k = 5, fill = NA, align = "left")) %>%
  ungroup()

# analysis on how detections affect speed
onsets <- dft %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
         VibStartTime = TimeSeconds,
         SpeedAtVibStart = rollingSpeedMedian)

offsets <- dft %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
         VibStopTime = TimeSeconds,
         SpeedAtVibStop = rollingSpeedMedian
  )

#mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
#mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)

dft <- left_join(dft, onsets)
dft <- left_join(dft, offsets)

dft$TimeSinceVibStart <- dft$TimeSeconds - dft$VibStartTime
dft$TimeSinceVibStop <- ifelse(substr(dft$ObjectDetected, 1, 1) == "B",NA, dft$TimeSeconds - dft$VibStopTime)

dft$SpeedDiffFromStart <- dft$rollingSpeedMedian - dft$SpeedAtVibStart
dft$SpeedDiffFromStop <- dft$rollingSpeedMedian - dft$SpeedAtVibStop

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

dft <- dft[, col_order]

# Make data frame into a .rda file for faster running time
save(dft, file = "data_Training_All.rda", compress = TRUE)

# Group Data set based on TestID
dftSumTestID <- dft %>% 
  filter(PersonSpeed<3)%>%
  group_by(testID, day, Scenario, FOD, Range)%>%
  summarize(avgSpeed = mean(PersonSpeed),
            medianSpeed = median(PersonSpeed),
            maxSpeed = max(PersonSpeed),
            minSpeed = min(PersonSpeed),
            objectDetected = sum(objDet,na.rm = TRUE),
            objectCollisions = sum(objColl,na.rm = TRUE),
            Time = max(TimeSeconds))%>% 
  arrange(testID)

#add Coloum with sum of total time spent 
dftSumTestID$totalTimeTraining<-round(cumsum(dftSumTestID$Time))

#add Coloum with total time spent for a given FOD with a given Range
dftSumTestID <- dftSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
dftSumTestID <- dftSumTestID%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
dftSumTestID <- dftSumTestID%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

# Save the grouped data
save(dftSumTestID, file = "data_Training_SumTestID.rda", compress = TRUE)







# Make a total time in seconds
# dft$timeSinceStart<-as.double(substr(dft$Timer,9,16))

# Rearrange the columns
# dft<-dft[,c(18:21,22,1:17,22)]

# dft$GapObjDetID<-ifelse(substr(dft$Object_detected,1,1) == "B",NA,dft$GapObjDetID)
# dft$ActObjectDetID<-ifelse(substr(dft$Object_detected,1,1) == "B",dft$ObjDetID,NA)

# This looks like it is just counting up at each row.
# dft$timeSinceExpStarted<-time(dft$Time_stamp) - min(time(dft$Time_stamp))

# dft %<>%
#   filter(substr(Object_detected, 1, 1) != "B") %>%
#   select(GapObjDetID, TimeSincePreRow, rowNum) %>%
#   group_by(GapObjDetID) %>%
#   summarise(Gapdurationx = sum(TimeSincePreRow)) %>%
#   View()

# tempdf <- dft %>%
#   filter(substr(Object_detected, 1, 1) != "B") %>%
#   select(GapObjDetID, TimeSincePreRow, rowNum) %>%
#   group_by(GapObjDetID) %>%
#   summarise(Gapdurationx = sum(TimeSincePreRow))
# dft %>%
#   filter(!is.na(GapObjDetID)) %>%
#   select(GapObjDetID, TimeSincePreRow, rowNum) %>%
#   group_by(GapObjDetID) %>%
#   summarise(Gapdurationx = sum(TimeSincePreRow))
#   right_join(tempdf, as.tibble(dft)) %>%
#   arrange(rowNum) %>%
#   slice(5992:6008) %>%
#   View()

# save(dft,file = "data_all_Training.rda")
# load(file = "data_all_Training.rda")