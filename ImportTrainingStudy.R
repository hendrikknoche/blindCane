#import and save

#------- Libraries -------
library(tidyverse)
library(magrittr)
library(readbulk)
library(lubridate)
library(zoo)
library("tibble")

#------- Import and save raw data --------

options("digits.secs" = 6)

# Combine all data files into one data frame
dft <- readbulk::read_bulk("data", sep = ";", na.strings = "none", stringsAsFactors = FALSE)

#Save the imported files
save(dft, file = "data_Training_Raw.rda", compress = TRUE)

#------- Load Raw Data From rda --------

#Load
load("data_Training_Raw.rda")

#------- Clean: testID and order row number --------

# Make a new testID based on the file name
dft$testID <- as.numeric(gsub("[^0-9.-]", "", substr(dft$File, 7, 10)))
dft$newTestStarts <- ifelse(dft$testID > lag(dft$testID, default = 0), 1, 0)

# Delete Test.ID Columns as the fist 18 are wrong
dft$Test.ID <- NULL

# order data correctly #create a row number to keep track of things
dft <- dft %>%
  arrange(testID, Timer) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

#Add coloum with run number
dft <- dft %>% 
  group_by(Participant.ID) %>% 
  mutate(RunNumber = cumsum(newTestStarts))

#------- Clean: data type and name --------

# Make FOD into a factor
dft$FOD <- as.factor(dft$FOD)
dft$FOD <- recode_factor(dft$FOD, 
                         Baseline="White Cane", 
                         WholeRoom="Conical View AWC", 
                         Corridor="Tunnel View AWC")
dft$FOD <- factor(dft$FOD, 
                  levels=c("White Cane", 
                           "Conical View AWC", 
                           "Tunnel View AWC"))

# Rename Columns
dft%<>% dplyr::rename(Range = Detection_range_in_Meters,
                      day = Day_nr.,
                      PersonSpeed = Person_Speed,
                      TimeSeconds = Time_in_MS,
                      TimeStamp = Time_stamp,
                      ObjectDistance = Distance_to_object,
                      ObjectCollision = Object_collision,
                      ObjectDetected = Object_detected,
                      ParticipantID = Participant.ID)


#------- Clean: Time --------

# Use time stamp to calculate how long a test took
dft$TimeSeconds <- as.POSIXct(dft$TimeStamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
dft$TimeSeconds <- as.POSIXct(dft$Timer, format = "%H:%M:%OS")
dft$TimeSeconds <- second(dft$TimeSeconds) + minute(dft$TimeSeconds) * 60 + hour(dft$TimeSeconds) * 3660

# The time of the row above
dft <- dft %>% 
  group_by(ParticipantID, testID) %>% 
  mutate(NewTimer = lag(TimeSeconds, 1)) %>% 
  ungroup()

# Time since the row above
dft$TimeSincePrevRow <- ifelse(dft$NewTimer > dft$TimeSeconds, 0, dft$TimeSeconds - dft$NewTimer)
dft[1, ]$TimeSincePrevRow <- 0

# Calculate the total time spent
dft$totalTime <- cumsum(dft$TimeSincePrevRow)

#------- Clean: Active Vibrator --------

dft <- dft %>% 
  dplyr::group_by(testID) %>% 
  dplyr::mutate(ActiveVibrator = case_when(
    ObjectDistance > 1.35 & ObjectDistance < 2.35 ~ "1",
    ObjectDistance > 2.35 & ObjectDistance < 3.35 ~ "2",
    ObjectDistance > 3.35 ~ "3"))

#------- Setup Scenario Tracker --------

# Keep track on when a new scenario starts
dft$ScenarioBefore <- lag(dft$Scenario, default = 99999)
dft$ScenarioStarts <- ifelse(!(dft$Scenario == dft$ScenarioBefore), 1, 0)
dft$RunningScenarioCounter <- cumsum(dft$ScenarioStarts)

#------- Make rolling median speed ---------

# create median smoothed speed column
dft %<>% 
  filter(PersonSpeed < 3) %>% 
  group_by(testID) %>%
  mutate(rollingSpeedMedian = rollmedian(x = PersonSpeed, 
                                         k = 5, 
                                         fill = NA, 
                                         align = "left")) %>%
  ungroup()


#------- Collision and their effect --------

# Setup flags and variables for Collisions
dft %<>% 
  dplyr::group_by(testID) %>% 
  dplyr::mutate(ObjectCollision = gsub("null", "", ObjectCollision),
                objCollStart = ifelse(substr(ObjectCollision, 1, 1) == "B" & 
                                        dplyr::lag(ObjectCollision) == "", 1, 0),
                #objColl = ifelse(substr(ObjectCollision, 1, 1) == "B" & 
                #dplyr::lag(ObjectCollision) == "", 1, 0), 
                objCollStop =  ifelse(substr(ObjectCollision, 1, 1) == "B" & 
                                        dplyr::lead(ObjectCollision) == "", 1, 0),
                objCollAfter = lead(ObjectCollision),
                objCollInTestID = cumsum(objCollStart),
                objCollGapInTestID = cumsum(objCollStop)) %>%
  # Calculate the change in speed after a coll starts and stops
  dplyr::group_by(testID, objCollInTestID) %>%
  dplyr::mutate(TimeSinceObjCollStart = cumsum(TimeSincePrevRow),
                TimeSinceObjCollStart = ifelse(objCollInTestID == 0, 
                                               NA, 
                                               TimeSinceObjCollStart)) %>%
  dplyr::group_by(testID,objCollGapInTestID) %>%
  dplyr::mutate(TimeSinceObjCollStop = cumsum(TimeSincePrevRow),
                TimeSinceObjCollStop = ifelse(objCollGapInTestID == 0, 
                                              NA, 
                                              TimeSinceObjCollStop)) %>%
  ungroup()

# analysis on how detections affect speed
onsetsCollision <- dft %>%
  filter(objCollStart == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStartTime = TimeSeconds,
         SpeedAtCollStart = rollingSpeedMedian)

offsetsCollision <- dft %>%
  filter(objCollStop == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStopTime = TimeSeconds,
         SpeedAtCollStop = rollingSpeedMedian
  )

dft <- left_join(dft, onsetsCollision)
dft <- left_join(dft, offsetsCollision)

remove(onsetsCollision, offsetsCollision)

# Calculate the development in speed since the collision started and stopped. 
dft$SpeedChangeFromCollStart <- dft$rollingSpeedMedian - dft$SpeedAtCollStart
dft$SpeedChangeFromCollStop <- dft$rollingSpeedMedian - dft$SpeedAtCollStart

# Calculate collision duration  
dft %<>%
  filter(substr(ObjectCollision, 1, 1) == "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(objCollDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>% 
  relocate(objCollDuration,ObjectCollision)

# Calculate Gap duration between collisions
dft %<>%
  filter(substr(ObjectCollision, 1, 1) != "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(CollGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>% 
  relocate(CollGapDuration)

#------- Total Detection and effect on speed -------

# Add consistentTimeline
#dft$ObjDetID <- cumsum(dft$objDet + dft$newTestStarts)

dft %<>% 
  dplyr::group_by(testID) %>% 
  dplyr::mutate(ObjectDetected = gsub("null", "", ObjectDetected),
                objDetStart = ifelse(substr(ObjectDetected, 1, 1) == "B" & 
                                       dplyr::lag(ObjectDetected) == "", 1, 0),
                objDetStop =  ifelse(substr(ObjectDetected, 1, 1) == "B" & 
                                       dplyr::lead(ObjectDetected) == "", 1, 0),
                objDetAfter = lead(ObjectDetected),
                objDetInTestID = cumsum(objDetStart),
                objDetGapInTestID = cumsum(objDetStop)) %>%
  # Calculate the change in speed after a coll starts and stops
  dplyr::group_by(testID, objDetInTestID) %>%
  dplyr::mutate(TimeSinceObjDetStart = cumsum(TimeSincePrevRow),
                TimeSinceObjDetStart = ifelse(objDetInTestID == 0, 
                                              NA, 
                                              TimeSinceObjDetStart)) %>%
  dplyr::group_by(testID,objDetGapInTestID) %>%
  dplyr::mutate(TimeSinceObjDetStop = cumsum(TimeSincePrevRow),
                TimeSinceObjDetStop = ifelse(objDetGapInTestID == 0, 
                                             NA, 
                                             TimeSinceObjDetStop)) %>%
  ungroup()

# create speed changes from detection start
dft %<>% 
  filter(objDetStart == 1) %>%
  select(objDetInTestID, rollingSpeedMedian, ObjectDistance) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(SpeedAtDetStart = dplyr::first(rollingSpeedMedian),
                   objDetStartDistance = dplyr::first(ObjectDistance)) %>% 
  right_join(dft, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromDetStart = rollingSpeedMedian - SpeedAtDetStart) %>%
  ungroup()

# analysis on how detections affect speed
onsetsDetection <- dft %>%
  filter(objDetStart == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStartTime = TimeSeconds
         #objDetStartDistance = ObjectDistance
  )

offsetsDetection <- dft %>%
  filter(objDetStop == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStopTime = TimeSeconds,
         SpeedAtDetStop = rollingSpeedMedian
  )

dft <- left_join(dft, onsetsDetection)
dft <- left_join(dft, offsetsDetection)

remove(onsetsDetection, offsetsDetection)

# Calculate detection duration  
dft %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(objDetInTestID, TimeSincePrevRow) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(ObjDetDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>% 
  relocate(ObjDetDuration,ObjectDetected)

# Calculate Gap duration between detection
dft %<>%
  filter(substr(ObjectDetected, 1, 1) != "B") %>%
  select(objDetGapInTestID, TimeSincePrevRow) %>%
  group_by(objDetGapInTestID) %>%
  dplyr::summarise(DetGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>% 
  relocate(DetGapDuration)

#------- Physical detection and effect on speed -------

#setup physical detection
dft %<>% dplyr::group_by(testID) %>% 
  dplyr::mutate(PhysDetOngoing = ifelse(substr(ObjectDetected, 1, 1) == "B" & ObjectDistance <= 1.3, 1, 0),
                PhysDetStart = ifelse(PhysDetOngoing == 1 & dplyr::lag(PhysDetOngoing) == 0 , 1, 0),
                PhysDetStop = ifelse(PhysDetOngoing == 1 & dplyr::lead(PhysDetOngoing) == 0, 1, 0),
                PhysDetInTestID = cumsum(PhysDetStart),
                PhysDetGapInTestID = cumsum(PhysDetStop)) %>%
  dplyr::group_by(testID, PhysDetInTestID) %>%
  dplyr::mutate(TimeSincePhysDetStart = cumsum(TimeSincePrevRow),
                TimeSincePhysDetStart = ifelse(PhysDetInTestID == 0, NA, TimeSincePhysDetStart)) %>%
  dplyr::group_by(testID, PhysDetGapInTestID) %>%
  dplyr::mutate(TimeSincePhysDetStop = cumsum(TimeSincePrevRow),
                TimeSincePhysDetStop = ifelse(PhysDetGapInTestID == 0, NA, TimeSincePhysDetStop)) %>%
  ungroup()

#Track the last detected obstacle
LastObstaclePhysDetected <- dft %>% 
  dplyr::group_by(testID, PhysDetInTestID) %>% 
  dplyr::filter(ObjectDetected != "") %>%
  dplyr::summarise(LastObjPhysDet = ObjectDetected)  %>% 
  filter(!(PhysDetInTestID == lead(PhysDetInTestID) & testID == lead(testID)) | is.na(lead(PhysDetInTestID)))

dft <- merge(dft, LastObstaclePhysDetected, by=c("testID","PhysDetInTestID"), all=T) %>%
  arrange(rowNum)

remove(LastObstaclePhysDetected)

# create speed changes from physDetstart
dft %<>% 
  filter(PhysDetStart == 1) %>%
  select(PhysDetInTestID, rollingSpeedMedian) %>%
  group_by(PhysDetInTestID) %>%
  dplyr::summarise(SpeedAtPhysDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(dft, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromPhysDetStart = rollingSpeedMedian - SpeedAtPhysDetStart) 

#------- Augmented detection and effect on speed -------

#setup augVibrations
dft %<>% dplyr::group_by(testID) %>% 
  dplyr::mutate(AugDetOngoing = ifelse(substr(ObjectDetected, 1, 1) == "B" & ObjectDistance > 1.3, 1, 0),
                AugDetStart = ifelse(AugDetOngoing == 1 & dplyr::lag(AugDetOngoing) == 0 , 1, 0),
                AugDetStop = ifelse(AugDetOngoing == 1 & dplyr::lead(AugDetOngoing) == 0, 1, 0),
                AugDetInTestID = cumsum(AugDetStart),
                AugDetGapInTestID = cumsum(AugDetStop)) %>%
  dplyr::group_by(testID,AugDetInTestID) %>%
  dplyr::mutate(TimeSinceAugDetStart = cumsum(TimeSincePrevRow),
                TimeSinceAugDetStart = ifelse(AugDetInTestID==0, NA, TimeSinceAugDetStart)) %>%
  dplyr::group_by(testID,AugDetGapInTestID) %>%
  dplyr::mutate(TimeSinceAugDetStop = cumsum(TimeSincePrevRow),
                TimeSinceAugDetStop = ifelse(AugDetGapInTestID == 0, NA, TimeSinceAugDetStop)) %>%
  ungroup()

#Track the last detected obstacle
LastObstacleAugDetected <- dft %>% 
  dplyr::group_by(testID, AugDetInTestID) %>% 
  dplyr::filter(ObjectDetected != "") %>%
  dplyr::summarise(LastObjAugDet = ObjectDetected)  %>% 
  filter(!(AugDetInTestID == lead(AugDetInTestID) & testID == lead(testID)) | is.na(lead(AugDetInTestID)))

dft <- merge(dft, LastObstacleAugDetected, by=c("testID","AugDetInTestID"), all=T) %>%
  arrange(rowNum)

remove(LastObstacleAugDetected)

# create speed changes from augDetstart
dft %<>% 
  filter(AugDetStart == 1) %>%
  select(AugDetInTestID, rollingSpeedMedian) %>%
  group_by(AugDetInTestID) %>%
  dplyr::summarise(SpeedAtAugDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromAugDetStart = rollingSpeedMedian - SpeedAtAugDetStart) 

#------- Rearrange the columns -----

# Rearrange the columns
col_order <- c("rowNum", "day", "ParticipantID", "testID", "newTestStarts", "RunNumber", "Range", "FOD",
               # Scenario:
               "Scenario", "ScenarioBefore", "ScenarioStarts", "RunningScenarioCounter",  
               # Time:
               "TimeStamp", "Timer", "TimeSeconds", "NewTimer", "TimeSincePrevRow", "totalTime",  
               # Speed:
               "PersonSpeed", "rollingSpeedMedian",
               # total Detection
               "ObjectDetected", "objDetInTestID", "ObjectDistance", "objDetStartDistance", "ActiveVibrator", 
               "objDetStart", "DetStartTime", "TimeSinceObjDetStart", "SpeedAtDetStart", "SpeedChangeFromDetStart",
               "objDetStop", "DetStopTime", "TimeSinceObjDetStop", "SpeedAtDetStop", "objDetAfter", "ObjDetDuration", 
               "objDetGapInTestID", "DetGapDuration",
               # Physical Detection
               "PhysDetInTestID", "PhysDetOngoing", "PhysDetStart", "TimeSincePhysDetStart", "SpeedAtPhysDetStart", 
               "SpeedChangeFromPhysDetStart", "PhysDetStop", "TimeSincePhysDetStop", "LastObjPhysDet", 
               "PhysDetGapInTestID",
               # Augmented Detection
               "AugDetInTestID", "AugDetOngoing", "AugDetStart", "TimeSinceAugDetStart", "SpeedAtAugDetStart", 
               "SpeedChangeFromAugDetStart", "AugDetStop", "TimeSinceAugDetStop", "LastObjAugDet",  
               "AugDetGapInTestID",
               # Collision
               "ObjectCollision", "objCollInTestID", "objCollStart", "CollStartTime", "TimeSinceObjCollStart", "SpeedAtCollStart", "SpeedChangeFromCollStart", 
               "objCollStop", "TimeSinceObjCollStop", "SpeedChangeFromCollStop", "objCollDuration", "objCollAfter", 
               "objCollGapInTestID", "CollGapDuration",
               # Position person
               "Person_pos_X", "Person_pos_Y", "Person_orientation", 
               # Position awc
               "Whitecane_pos_X", "Whitecane_pos_Y", "Whitecane_orientation", 
               # Position obstacle
               "Object_pos_X", "Object_pos_Y", 
               # File
               "File" )

dft <- dft[, col_order]

remove(col_order)

#------- Save -----

# Make data frame into a .rda file for faster running time and to load it in other scripts
save(dft, file='data_Training_All.rda', compress=TRUE)

#------- Make a condenced dataframe ----------

# Group Data set based on TestID
dftSumTestID <- dft %>% 
  group_by(ParticipantID, testID, day, Scenario, FOD, Range)%>%
  dplyr::summarize(avgSpeed = mean(PersonSpeed),
                   medianSpeed = median(PersonSpeed),
                   maxSpeed = max(PersonSpeed),
                   minSpeed = min(PersonSpeed),
                   avgVibDuration = mean(ObjDetDuration, na.rm = TRUE),
                   avgGabDuration = mean(DetGapDuration),
                   objectDetected = sum(objDetStart, na.rm = TRUE),
                   physObjectDetected = sum(PhysDetStart, na.rm = TRUE),
                   augObjectDetected = sum(AugDetStart, na.rm = TRUE),
                   objectCollisions = sum(objCollStart, na.rm = TRUE),
                   Time = max(TimeSeconds))%>% 
  arrange(testID)

#add Coloum with sum of total time spent 
dftSumTestID$totalTimeTraining<-round(cumsum(dftSumTestID$Time))

#add Coloum with total time spent for a given FOD with a given Range
dftSumTestID <- dftSumTestID %>% 
  group_by(ParticipantID)%>%
  mutate(TrainingByParticipant=round(cumsum(Time)))

#Add coloum with run number
dftSumTestID <- dftSumTestID %>% 
  group_by(ParticipantID) %>% 
  mutate(RunNumber = 1:n())

#add Coloum with total time spent for a given FOD with a given Range
dftSumTestID <- dftSumTestID %>%
  group_by(FOD,day,Range) %>%
  mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD with a given Range
dftSumTestID <- dftSumTestID %>%
  group_by(FOD,day,Range) %>%
  mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
dftSumTestID <- dftSumTestID %>%
  group_by(FOD,day) %>%
  mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
dftSumTestID <- dftSumTestID %>%
  group_by(day) %>%
  mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

# Save the grouped data
save(dftSumTestID, file = "data_Training_SumTestID.rda", compress = TRUE)

##### Old -----

# Use time stamp to calculate how long a test took
dft$Time_stamp <- as.POSIXct(dft$Time_stamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
dft$Time_in_MS <- as.POSIXct(dft$Timer, format = "%H:%M:%OS")
dft$Time_in_MS <- second(dft$Time_in_MS)+minute(dft$Time_in_MS)*60+hour(dft$Time_in_MS)*3660



# Make FOD into a factor
dft$FOD <- as.factor(dft$FOD)
dft$FOD <- recode_factor(dft$FOD, Baseline="White Cane", WholeRoom="Conical View AWC", Corridor="Tunnel View AWC")
dft$FOD <- factor(dft$FOD, levels=c("White Cane", "Conical View AWC", "Tunnel View AWC"))

# The time of the row above
dft<- dft %>% group_by(ParticipantID,testID) %>% mutate(NewTimer=lag(TimeSeconds,1))
# $NewTimer <- lag(dft$Time_in_MS, 1)

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
  dplyr::summarise(Gapduration = sum(TimeSincePreRow)) %>% 
  right_join(dft) %>% arrange(rowNum) %>% relocate(Gapduration)

# Calculate detection duration  
dft %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(ObjDetIDTest, TimeSincePreRow) %>%
  group_by(ObjDetIDTest) %>%
  dplyr::summarise(ObjDetDuration = sum(TimeSincePreRow)) %>% 
  right_join(dft) %>% 
  arrange(rowNum) %>% 
  relocate(ObjDetDuration,ObjectDetected)

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
            avgVibDuration = mean(ObjDetDuration),
            avgGabDuration = mean(Gapduration),
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