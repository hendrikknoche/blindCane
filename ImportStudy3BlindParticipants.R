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
#vibContinuationMaskingLength = 3


# Combine all data files into one data frame
df_bp = readbulk::read_bulk('3StudyData', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

#Save the imported files
save(df_bp, file='data_3StudyBlindParticipant_Raw.rda', compress=TRUE)

#------- Load Raw Data From rda --------

#Load
load("data_3StudyBlindParticipant_Raw.rda")

#------- Clean: testID and order row number --------

# Make a new testID based on the file name
df_bp$testID <- as.numeric(gsub("[^0-9.-]", "", substr(df_bp$File, 7, 10)))
df_bp$newTestStarts <- ifelse(df_bp$testID > lag(df_bp$testID, default = 0), 1, 0)

# Delete Test.ID Columns as the first 18 are wrong
df_bp$Test.ID <- NULL

# order data correctly by creating a row number to keep track of things
df_bp <- df_bp %>%
  arrange(testID, Timer) %>%
  mutate(rowNum = 1:n()) %>%
  relocate(rowNum)

#Add coloum with run number
df_bp <- df_bp %>% 
  group_by(Participant.ID) %>% 
  mutate(RunNumber = cumsum(newTestStarts))

df_bp <- df_bp[!(df_bp$testID == 0),] 

#------- Clean: data type and name --------

df_bp$FOD[df_bp$testID == 22] <- "WholeRoom"

# Make FOD into a factor
df_bp$FOD <- as.factor(df_bp$FOD)
df_bp$FOD <- recode_factor(df_bp$FOD, 
                         Baseline="White Cane", 
                         WholeRoom="Conical View AWC",
                         Wholeroom="Conical View AWC",
                         Corridor="Tunnel View AWC")
df_bp$FOD <- factor(df_bp$FOD, 
                  levels=c("White Cane", 
                           "Conical View AWC", 
                           "Tunnel View AWC"))

# Rename Columns
df_bp%<>% dplyr::rename(Range = Detection_range_in_Meters,
                      day = Day_nr.,
                      PersonSpeed = Person_Speed,
                      TimeSeconds = Time_in_MS,
                      TimeStamp = Time_stamp,
                      ObjectDistance = Distance_to_object,
                      ObjectCollision = Object_collision,
                      ObjectDetected = Object_detected,
                      ParticipantID = Participant.ID)


# Fix Range
df_bp$Range[df_bp$Range == 0.7] <- 1

#------- Clean: Time --------

# Use time stamp to calculate how long a test took
df_bp$TimeSeconds <- as.POSIXct(df_bp$TimeStamp, format = "%m/%d/%Y %H:%M:%S")

# convert the timer into time in seconds
df_bp$TimeSeconds <- as.POSIXct(df_bp$Timer, format = "%H:%M:%OS")
df_bp$TimeSeconds <- second(df_bp$TimeSeconds) + minute(df_bp$TimeSeconds) * 60 + hour(df_bp$TimeSeconds) * 3660

# The time of the row above
df_bp <- df_bp %>% 
  group_by(ParticipantID, testID) %>% 
  mutate(NewTimer = lag(TimeSeconds, 1)) %>% 
  ungroup()

# Time since the row above
df_bp$TimeSincePrevRow <- ifelse(df_bp$NewTimer > df_bp$TimeSeconds, 0, df_bp$TimeSeconds - df_bp$NewTimer)
df_bp[1, ]$TimeSincePrevRow <- 0

# Calculate the total time spent
df_bp$totalTime <- cumsum(df_bp$TimeSincePrevRow)

#------- Clean: Active Vibrator --------

df_bp <- df_bp %>% 
  dplyr::group_by(testID) %>% 
  dplyr::mutate(ActiveVibrator = case_when(
    ObjectDistance > 1.35 & ObjectDistance < 2.35 ~ "1",
    ObjectDistance > 2.35 & ObjectDistance < 3.35 ~ "2",
    ObjectDistance > 3.35 ~ "3"))

#------- Setup Scenario Tracker --------

# Keep track on when a new scenario starts
df_bp$ScenarioBefore <- lag(df_bp$Scenario, default = 99999)
df_bp$ScenarioStarts <- ifelse(!(df_bp$Scenario == df_bp$ScenarioBefore), 1, 0)
df_bp$RunningScenarioCounter <- cumsum(df_bp$ScenarioStarts)

#------- Make rolling median speed ---------

# create median smoothed speed column
df_bp %<>% 
  filter(PersonSpeed < 3) %>% 
  group_by(testID) %>%
  mutate(rollingSpeedMedian = rollmedian(x = PersonSpeed, 
                                         k = 5, 
                                         fill = NA, 
                                         align = "left")) %>%
  ungroup()


#------- Collision and their effect --------

# Setup flags and variables for Collisions
df_bp %<>% 
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
onsetsCollision <- df_bp %>%
  filter(objCollStart == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStartTime = TimeSeconds,
         SpeedAtCollStart = rollingSpeedMedian)

offsetsCollision <- df_bp %>%
  filter(objCollStop == 1) %>%
  select(rowNum,
         objCollInTestID,
         CollStopTime = TimeSeconds,
         SpeedAtCollStop = rollingSpeedMedian
  )

df_bp <- left_join(df_bp, onsetsCollision)
df_bp <- left_join(df_bp, offsetsCollision)

remove(onsetsCollision, offsetsCollision)

# Calculate the development in speed since the collision started and stopped. 
df_bp$SpeedChangeFromCollStart <- df_bp$rollingSpeedMedian - df_bp$SpeedAtCollStart
df_bp$SpeedChangeFromCollStop <- df_bp$rollingSpeedMedian - df_bp$SpeedAtCollStart

# Calculate collision duration  
df_bp %<>%
  filter(substr(ObjectCollision, 1, 1) == "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(objCollDuration = sum(TimeSincePrevRow)) %>% 
  right_join(df_bp) %>% 
  arrange(rowNum) %>% 
  relocate(objCollDuration,ObjectCollision)

# Calculate Gap duration between collisions
df_bp %<>%
  filter(substr(ObjectCollision, 1, 1) != "B") %>%
  select(objCollInTestID, TimeSincePrevRow) %>%
  group_by(objCollInTestID) %>%
  dplyr::summarise(CollGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(df_bp) %>% 
  arrange(rowNum) %>% 
  relocate(CollGapDuration)

# ------ Total Detection and effect on speed -------

# Add consistentTimeline
#df_bp$ObjDetID <- cumsum(df_bp$objDet + df_bp$newTestStarts)

df_bp %<>% 
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
df_bp %<>% 
  filter(objDetStart == 1) %>%
  select(objDetInTestID, rollingSpeedMedian, ObjectDistance) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(SpeedAtDetStart = dplyr::first(rollingSpeedMedian),
                   objDetStartDistance = dplyr::first(ObjectDistance)) %>% 
  right_join(df_bp, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromDetStart = rollingSpeedMedian - SpeedAtDetStart) %>%
  ungroup()

# analysis on how detections affect speed
onsetsDetection <- df_bp %>%
  filter(objDetStart == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStartTime = TimeSeconds
         #objDetStartDistance = ObjectDistance
  )

offsetsDetection <- df_bp %>%
  filter(objDetStop == 1) %>%
  select(rowNum,
         objDetInTestID,
         DetStopTime = TimeSeconds,
         SpeedAtDetStop = rollingSpeedMedian
  )

df_bp <- left_join(df_bp, onsetsDetection)
df_bp <- left_join(df_bp, offsetsDetection)

remove(onsetsDetection, offsetsDetection)

# Calculate detection duration  
df_bp %<>%
  filter(substr(ObjectDetected, 1, 1) == "B") %>%
  select(objDetInTestID, TimeSincePrevRow) %>%
  group_by(objDetInTestID) %>%
  dplyr::summarise(ObjDetDuration = sum(TimeSincePrevRow)) %>% 
  right_join(df_bp) %>% 
  arrange(rowNum) %>% 
  relocate(ObjDetDuration,ObjectDetected)

# Calculate Gap duration between detection
df_bp %<>%
  filter(substr(ObjectDetected, 1, 1) != "B") %>%
  select(objDetGapInTestID, TimeSincePrevRow) %>%
  group_by(objDetGapInTestID) %>%
  dplyr::summarise(DetGapDuration = sum(TimeSincePrevRow)) %>% 
  right_join(df_bp) %>% 
  arrange(rowNum) %>% 
  relocate(DetGapDuration)

# ------ Physical detection and effect on speed -------

#setup physical detection
df_bp %<>% dplyr::group_by(testID) %>% 
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
df_bp %<>% 
  filter(PhysDetStart == 1) %>%
  select(PhysDetInTestID, rollingSpeedMedian) %>%
  group_by(PhysDetInTestID) %>%
  dplyr::summarise(SpeedAtPhysDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(df_bp, na_matches = "never") %>% 
  arrange(rowNum) %>%
  dplyr::mutate(SpeedChangeFromPhysDetStart = rollingSpeedMedian - SpeedAtPhysDetStart) 

# ------ Augmented detection and effect on speed -------

#setup augVibrations
df_bp %<>% dplyr::group_by(testID) %>% 
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
df_bp %<>% 
  filter(AugDetStart == 1) %>%
  select(AugDetInTestID, rollingSpeedMedian) %>%
  group_by(AugDetInTestID) %>%
  dplyr::summarise(SpeedAtAugDetStart = dplyr::first(rollingSpeedMedian)) %>% 
  right_join(df_bp) %>% 
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

df_bp <- df_bp[, col_order]

remove(col_order)

#------- Save -----

# Make data frame into a .rda file for faster running time and to load it in other scripts
save(df_bp, file='data_3StudyBlindParticipant_All.rda', compress=TRUE)

#------- Make a condenced dataframe ----------

# Group Data set based on TestID
df_bpSumTestID <- df_bp %>% 
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
df_bpSumTestID$totalTimeTraining<-round(cumsum(df_bpSumTestID$Time))

#add Coloum with total time spent for a given FOD with a given Range
df_bpSumTestID <- df_bpSumTestID %>% group_by(ParticipantID)%>%mutate(TrainingByParticipant=round(cumsum(Time)))

#Add coloum with run number
df_bpSumTestID <- df_bpSumTestID %>% group_by(ParticipantID) %>% mutate(RunNumber = 1:n())

#add Coloum with total time spent for a given FOD with a given Range
df_bpSumTestID <- df_bpSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD with a given Range
df_bpSumTestID <- df_bpSumTestID%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
df_bpSumTestID <- df_bpSumTestID%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
df_bpSumTestID <- df_bpSumTestID%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

#------- Save the grouped data -------
# Make data frame into a .rda file for faster running time and to load it in other scripts
save(df_bpSumTestID, file = "data_Participants_bpSumTestID.rda", compress = TRUE)


