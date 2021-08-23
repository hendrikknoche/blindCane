#import and save

#------- Libraries -------
library(tidyverse)
library(readbulk)
library(lubridate)
library(zoo)
library(magrittr)

#------- Import and save raw data --------

options("digits.secs"=6)

#temporal masking - for how long does an ended vibration alert mask such that we cannot distinguish it from a previous alert?
vibContinuationMaskingLength = 3


# Combine all data files into one data frame
dfp = readbulk::read_bulk('dataParticipants', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

#Save the imported files
save(dfp, file='data_Participants_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("data_Participants_Raw.rda")

#------- Clean: testID and order row number --------

# Make a new testID based on the file name
dfp$testID <- as.numeric(gsub("[^0-9.-]", "", substr(dfp$File, 7, 10)))
dfp$newTestStarts <- ifelse(dfp$testID > lag(dfp$testID, default = 0), 1, 0)

# Delete Test.ID Columns as the first 18 are wrong
dfp$Test.ID <- NULL

# order data correctly by creating a row number to keep track of things
dfp <- dfp %>%
  arrange(testID, Timer) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

#Add coloum with run number
dfp <- dfp %>% 
  group_by(Participant.ID) %>% 
  mutate(RunNumber = cumsum(newTestStarts))

#------- Clean: data type and name --------

# Make FOD into a factor
dfp$FOD <- as.factor(dfp$FOD)
dfp$FOD <- recode_factor(dfp$FOD, 
                         Baseline="White Cane", 
                         WholeRoom="Conical View AWC", 
                         Corridor="Tunnel View AWC")
dfp$FOD <- factor(dfp$FOD, 
                  levels=c("White Cane", 
                           "Conical View AWC", 
                           "Tunnel View AWC"))

# Rename Columns
dfp%<>% dplyr::rename(Range = Detection_range_in_Meters,
                         day = Day_nr.,
                         PersonSpeed = Person_Speed,
                         TimeSeconds = Time_in_MS,
                         TimeStamp = Time_stamp,
                         ObjectDistance = Distance_to_object,
                         ObjectCollision = Object_collision,
                         ObjectDetected = Object_detected,
                         ParticipantID = Participant.ID)

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

# Fix Range
dfp$Range[dfp$Range == 0.7] <- 1

#------- Clean: Time --------

# Use time stamp to calculate how long a test took
dfp$TimeSeconds <- as.POSIXct(dfp$TimeStamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
dfp$TimeSeconds <- as.POSIXct(dfp$Timer, format = "%H:%M:%OS")
dfp$TimeSeconds <- second(dfp$TimeSeconds) + minute(dfp$TimeSeconds) * 60 + hour(dfp$TimeSeconds) * 3660

# The time of the row above
dfp <- dfp %>% 
  group_by(ParticipantID, testID) %>% 
  mutate(NewTimer = lag(TimeSeconds, 1)) %>% 
  ungroup()

# Time since the row above
dfp$TimeSincePrevRow <- ifelse(dfp$NewTimer > dfp$TimeSeconds, 0, dfp$TimeSeconds - dfp$NewTimer)
dfp[1, ]$TimeSincePrevRow <- 0

# Calculate the total time spent
dfp$totalTime <- cumsum(dfp$TimeSincePrevRow)

#------- Clean: Active Vibrator --------

dfp <- dfp %>% 
  dplyr::group_by(testID) %>% 
  dplyr::mutate(ActiveVibrator = case_when(
    ObjectDistance > 1.35 & ObjectDistance < 2.35 ~ "1",
    ObjectDistance > 2.35 & ObjectDistance < 3.35 ~ "2",
    ObjectDistance > 3.35 ~ "3"))

#------- Setup Scenario Tracker --------

# Keep track on when a new scenario starts
dfp$ScenarioBefore <- lag(dfp$Scenario, default = 99999)
dfp$ScenarioStarts <- ifelse(!(dfp$Scenario == dfp$ScenarioBefore), 1, 0)
dfp$RunningScenarioCounter <- cumsum(dfp$ScenarioStarts)

#------- Make rolling median speed ---------

# create median smoothed speed column
dfp %<>% 
  filter(PersonSpeed < 3) %>% 
  group_by(testID) %>%
  mutate(rollingSpeedMedian = rollmedian(x = PersonSpeed, 
                                         k = 5, 
                                         fill = NA, 
                                         align = "left")) %>%
  ungroup()


#------- Collision and their effect --------

# Setup flags and variables for Collisions
dfp %<>% 
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
onsetsCollision <- dfp %>%
  filter(objCollStart == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStartTime = TimeSeconds,
         SpeedAtCollStart = rollingSpeedMedian)

offsetsCollision <- dfp %>%
  filter(objCollStop == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStopTime = TimeSeconds,
         SpeedAtCollStop = rollingSpeedMedian
  )

dfp <- left_join(dfp, onsetsCollision)
dfp <- left_join(dfp, offsetsCollision)

remove(onsetsCollision, offsetsCollision)

# Calculate the development in speed since the collision started and stopped. 
dfp$SpeedChangeFromCollStart <- dfp$rollingSpeedMedian - dfp$SpeedAtCollStart
dfp$SpeedChangeFromCollStop <- dfp$rollingSpeedMedian - dfp$SpeedAtCollStart

# Calculate collision duration  
dfp %<>%
  filter(substr(ObjectCollision, 1, 1) == "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(objCollDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dfp) %>% 
  arrange(rowNum) %>% 
  relocate(objCollDuration,ObjectCollision)

# Calculate Gap duration between collisions
dfp %<>%
  filter(substr(ObjectCollision, 1, 1) != "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(CollGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dfp) %>% 
  arrange(rowNum) %>% 
  relocate(CollGapDuration)

# ------ Total Detection and effect on speed -------

# Add consistentTimeline
#dfp$ObjDetID <- cumsum(dfp$objDet + dfp$newTestStarts)

dfp %<>% 
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
dfp %<>% 
  filter(objDetStart == 1) %>%
  select(objDetInTestID, rollingSpeedMedian, ObjectDistance) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(SpeedAtDetStart = dplyr::first(rollingSpeedMedian),
                   objDetStartDistance = dplyr::first(ObjectDistance)) %>% 
  right_join(dfp, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromDetStart = rollingSpeedMedian - SpeedAtDetStart) %>%
  ungroup()

# analysis on how detections affect speed
onsetsDetection <- dfp %>%
  filter(objDetStart == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStartTime = TimeSeconds
         #objDetStartDistance = ObjectDistance
         )

offsetsDetection <- dfp %>%
  filter(objDetStop == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStopTime = TimeSeconds,
         SpeedAtDetStop = rollingSpeedMedian
  )

dfp <- left_join(dfp, onsetsDetection)
dfp <- left_join(dfp, offsetsDetection)

remove(onsetsDetection, offsetsDetection)

# Calculate detection duration  
dfp %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(objDetInTestID, TimeSincePrevRow) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(ObjDetDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dfp) %>% 
  arrange(rowNum) %>% 
  relocate(ObjDetDuration,ObjectDetected)

# Calculate Gap duration between detection
dfp %<>%
  filter(substr(ObjectDetected, 1, 1) != "B") %>%
  select(objDetGapInTestID, TimeSincePrevRow) %>%
  group_by(objDetGapInTestID) %>%
  dplyr::summarise(DetGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(dfp) %>% 
  arrange(rowNum) %>% 
  relocate(DetGapDuration)

# ------ Physical detection and effect on speed -------

#setup physical detection
dfp %<>% dplyr::group_by(testID) %>% 
  dplyr::mutate(PhysDetOngoing = ifelse(substr(ObjectDetected, 1, 1) == "B" & ObjectDistance <= 1.35, 1, 0),
                PhysDetStart = ifelse(PhysDetOngoing == 1 & dplyr::lag(PhysDetOngoing) == 0 , 1, 0),
                PhysDetStop = ifelse(PhysDetOngoing == 1 & dplyr::lead(PhysDetOngoing) == 0, 1, 0),
                PhysDetInTestID = cumsum(PhysDetStart),
                PhysDetGapInTestID = cumsum(PhysDetStop)) %>%
  dplyr::group_by(testID,PhysDetInTestID) %>%
  dplyr::mutate(TimeSincePhysDetStart = cumsum(TimeSincePrevRow),
                TimeSincePhysDetStart = ifelse(PhysDetInTestID==0,NA,TimeSincePhysDetStart))%>%
  dplyr::group_by(testID,PhysDetGapInTestID) %>%
  dplyr::mutate(TimeSincePhysDetStop = cumsum(TimeSincePrevRow),
                TimeSincePhysDetStop = ifelse(PhysDetGapInTestID==0,NA,TimeSincePhysDetStop))%>%
  ungroup()

# create speed changes from physDetstart
dfp %<>% 
  filter(PhysDetStart == 1) %>%
  select(PhysDetInTestID, rollingSpeedMedian) %>%
  group_by(PhysDetInTestID) %>%
  dplyr::summarise(SpeedAtPhysDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(dfp, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromPhysDetStart = rollingSpeedMedian - SpeedAtPhysDetStart) 

# ------ Augmented detection and effect on speed -------

#setup augVibrations
dfp %<>% dplyr::group_by(testID) %>% 
  dplyr::mutate(AugDetOngoing = ifelse(substr(ObjectDetected, 1, 1) == "B" & ObjectDistance > 1.35, 1, 0),
                AugDetStart = ifelse(AugDetOngoing == 1 & dplyr::lag(AugDetOngoing) == 0 , 1, 0),
                AugDetStop = ifelse(AugDetOngoing == 1 & dplyr::lead(AugDetOngoing) == 0, 1, 0),
                AugDetInTestID = cumsum(AugDetStart),
                AugDetGapInTestID = cumsum(AugDetStop))%>%
  dplyr::group_by(testID,AugDetInTestID) %>%
  dplyr::mutate(TimeSinceAugDetStart = cumsum(TimeSincePrevRow),
                TimeSinceAugDetStart = ifelse(AugDetInTestID==0, NA, TimeSinceAugDetStart))%>%
  dplyr::group_by(testID,AugDetGapInTestID) %>%
  dplyr::mutate(TimeSinceAugDetStop = cumsum(TimeSincePrevRow),
                TimeSinceAugDetStop = ifelse(AugDetGapInTestID == 0, NA, TimeSinceAugDetStop))%>%
  ungroup()

# create speed changes from augDetstart
dfp %<>% 
  filter(AugDetStart == 1) %>%
  select(AugDetInTestID, rollingSpeedMedian) %>%
  group_by(AugDetInTestID) %>%
  dplyr::summarise(SpeedAtAugDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(dfp) %>% 
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
               "SpeedChangeFromPhysDetStart", "PhysDetStop", "TimeSincePhysDetStop",  
               "PhysDetGapInTestID",
               # Augmented Detection
               "AugDetInTestID", "AugDetOngoing", "AugDetStart", "TimeSinceAugDetStart", "SpeedAtAugDetStart", 
               "SpeedChangeFromAugDetStart", "AugDetStop", "TimeSinceAugDetStop",  
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

dfp <- dfp[, col_order]

remove(col_order)

#------- Save -----

# Make data frame into a .rda file for faster running time and to load it in other scripts
save(dfp, file='data_Participants_All.rda', compress=TRUE)

#------- Make a condenced dataframe ----------

# Group Data set based on TestID
dfpSumTestID <- dfp %>% 
  group_by(ParticipantID, testID, day, Scenario, FOD, Range)%>%
  dplyr::summarize(avgSpeed = mean(PersonSpeed),
            medianSpeed = median(PersonSpeed),
            maxSpeed = max(PersonSpeed),
            minSpeed = min(PersonSpeed),
            avgVibDuration = mean(ObjDetDuration),
            avgGabDuration = mean(DetGapDuration),
            objectDetected = sum(objDetStart, na.rm = TRUE),
            physObjectDetected = sum(PhysDetStart, na.rm = TRUE),
            augObjectDetected = sum(AugDetStart, na.rm = TRUE),
            objectCollisions = sum(objCollStart, na.rm = TRUE),
            Time = max(TimeSeconds))%>% 
  arrange(testID)

#add Coloum with sum of total time spent 
dfpSumTestID$totalTimeTraining<-round(cumsum(dfpSumTestID$Time))

#add Coloum with total time spent for a given FOD with a given Range
dfpSumTestID <- dfpSumTestID %>% group_by(ParticipantID)%>%mutate(TrainingByParticipant=round(cumsum(Time)))

#Add coloum with run number
dfpSumTestID <- dfpSumTestID %>% group_by(ParticipantID) %>% mutate(RunNumber = 1:n())

#add Coloum with total time spent for a given FOD with a given Range
dfpSumTestID <- dfpSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD with a given Range
dfpSumTestID <- dfpSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
dfpSumTestID <- dfpSumTestID%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
dfpSumTestID <- dfpSumTestID%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

#------- Save the grouped data -------
# Make data frame into a .rda file for faster running time and to load it in other scripts
save(dfpSumTestID, file = "data_Participants_SumTestID.rda", compress = TRUE)


