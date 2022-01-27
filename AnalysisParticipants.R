# Analysis 10 Participants Study ---------------------------------

#----- Libraries -----
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(Rmisc)
library("car")
library(lme4)
library(MuMIn)
library("ggpubr") 

# not sure if we use these libraries
# library(ggpubr)
# library(grid)
# library(png)
# library(ggimage)

library(broom)
library(here)
library(gsheet)
library(ggforce)
library(ggpubr)
library(grid)
library(png)
library(ggimage)
library(zoo)
library(afex)
library(emmeans)

options("digits.secs"=6)

#----- GetData -----

# Load the data which was imported and cleaned by the "ImportBlindFoldedParticipants" script. 
load("data_Participants_All.rda")
load("data_Participants_SumTestID.rda")

#----- Data Overview: Detection Distance -----

# Density plot of the alert distance for each FOD.
dfp %>%
  filter(ObjectDistance > 0) %>%
  ggplot(aes(x = ObjectDistance, color = factor(FOD))) +
  geom_density() +
  theme_bw() +
  ylab("Density") +
  xlab("Alert Distance") +
  # facet_grid(rows = vars(FOD)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Data Overview: Detection Duration -----

meanAlertDuration <- dfpSumTestID %>%
  filter(ObjDetDuration > 0.01) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(mean(ObjDetDuration),
                   sd(ObjDetDuration))

meanAvgWalkingSpeed

# Density plot of the alert duration for each FOD.
dfp %>%
  filter(ObjDetDuration > 0.01) %>%
  ggplot(aes(
    x = ObjDetDuration,
    color = factor(FOD)
  )) +
  geom_density() +
  theme_bw() +
  ylab("Density") +
  xlab("Alert Duration") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Data Overview: Variance between Scenario -----
ggplot(dfpSumTestID, aes(x=factor(Scenario), y=avgSpeed)) + 
  geom_boxplot() +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Scenario") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Data Overview: Variance between Participants -----
ggplot(dfpSumTestID, aes(x=factor(ParticipantID), y=avgSpeed)) + 
  geom_boxplot() +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Participant") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

ggplot(dfpSumTestID, aes(x = objectDetected, y = avgSpeed)) + 
  geom_point(colour = "darkgray") +
  geom_smooth(se = F) +  
  #  geom_smooth(method = 'lm', se = F, aes(group = 1)) + 
  theme_bw()  +
  ylab("Average Walking Speed (m/s)") +
  xlab("Number of Alers per Run") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

dfpSumTestID <- dfpSumTestID %>%
  mutate(ParticipantID = as.factor(ParticipantID))

ggplot(dfpSumTestID, aes(x = objectDetected, y = log(avgSpeed), col = ParticipantID)) + 
  geom_point() +
  geom_smooth(aes(group = ParticipantID), method = 'lm', se = F) + 
  theme_bw()  +
  ylab("Log-transformed Average Walking Speed (m/s)") +
  xlab("Number of Alers per Run") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

dfpSumTestID <- dfpSumTestID %>%
  mutate(RunNumber = as.factor(RunNumber))

ggplot(dfpSumTestID, aes(x = objectDetected, y = log(avgSpeed), col = RunNumber)) + 
  geom_point() +
  #geom_smooth(aes(group = ParticipantID), method = 'lm', se = F) + 
  geom_smooth(aes(group = RunNumber), method = 'lm', se = F) +
  #  geom_smooth(method = 'lm', se = F, aes(group = 1)) + 
  theme_bw()  +
  ylab("Log-transformed Average Walking Speed (m/s)") +
  xlab("Number of Alers per Run") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")



#----- Data Overview: Variance between Run Number -----
ggplot(dfpSumTestID, aes(x=factor(RunNumber), y=avgSpeed)) + 
  geom_boxplot() +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Run Number") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

ggplot(dfpSumTestID, aes(x=RunNumber, y=avgSpeed)) +
  geom_point() +
  geom_smooth(method = lm)+
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Run Number") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Mean: Alerts -----

# Calculate the mean of the total number of Detections per run
mean(dfpSumTestID$objectDetected)
sd(dfpSumTestID$objectDetected)
qqmath(log(dfpSumTestID$objectDetected), id = 0.05)

summary(glm(objectDetected ~ FOD, data = dfpSumTestID)) 

# Calculate the mean of number of physical Detections per run
mean(dfpSumTestID$physObjectDetected)
sd(dfpSumTestID$physObjectDetected)
qqmath(log(dfpSumTestID$physObjectDetected), id = 0.05)

summary(glm(physObjectDetected ~ augObjectDetected, data = dfpSumTestID))

library(ggpubr)

cor(dfpSumTestID$augObjectDetected,dfpSumTestID$physObjectDetected, method="pearson")

# Calculate the mean of number of augmented Detections per run
mean(dfpSumTestID$augObjectDetected)
sd(dfpSumTestID$augObjectDetected)
qqmath(log(dfpSumTestID$augObjectDetected), id = 0.05)

meanAlerts <- dfpSumTestID %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(mean(avgSpeed),
                   mean(objectDetected),
                   sd(objectDetected),
                   mean(physObjectDetected),
                   #sd(physObjectDetected),
                   mean(augObjectDetected),
                   #sd(augObjectDetected)
                   )

meanAlerts

# Linear Regression to check significant 
summary(lm(objectDetected ~ FOD, data = dfpSumTestID)) 

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

#Plot walking speed per condition
dfpdaggAlert <- dfpSumTestID %>%
  dplyr::group_by(Range, FOD) %>%
  dplyr::summarise(newObjectDetected = mean(objectDetected),
                   smean = mean(objectDetected, na.rm = TRUE),
                   ssd = sd(objectDetected, na.rm = TRUE),
                   count = n()) %>%
  dplyr::mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = dfpdaggAlert, aes(x = Range, 
                                     y = newObjectDetected, 
                                     group = FOD, 
                                     color = FOD,
                                     shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size = 5) +
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1) +
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0.1)) +
  scale_fill_hue(name="Condition", 
                 labels=c("White Cane", 
                          "Body-preview aEMA", 
                          "Normal aEMA"))+
  #ggtitle("Number of Objects Detected per Range and Condition")+
  ylab("Average Number of Alerts") +
  xlab("Preview Range in Meter") +
  scale_y_continuous()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("") 


#Plot alerts vs fod
dfpdaggAlertFOD <- dfpSumTestID %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(totalObjectDetected = mean(objectDetected),
                   physicalObjectDetected = mean(physObjectDetected),
                   augmentedObjectDetected = mean(augObjectDetected),
                   smean = mean(objectDetected, na.rm = TRUE),
                   ssd = sd(objectDetected, na.rm = TRUE),
                   physsmean = mean(physObjectDetected, na.rm = TRUE),
                   physssd = sd(physObjectDetected, na.rm = TRUE),
                   augsmean = mean(augObjectDetected, na.rm = TRUE),
                   augssd = sd(augObjectDetected, na.rm = TRUE),
                   count = n()) %>%
  dplyr::mutate(
    totalse = ssd / sqrt(count),
    totallowerci = lower_ci(smean, totalse, count),
    totalupperci = upper_ci(smean, totalse, count),
    physse = physssd / sqrt(count),
    physlowerci = lower_ci(physsmean, physse, count),
    physupperci = upper_ci(physsmean, physse, count),
    augse = physssd / sqrt(count),
    auglowerci = lower_ci(augsmean, augse, count),
    augupperci = upper_ci(augsmean, augse, count))

dfpdaggAlertFOD$FOD <- factor(dfpdaggAlertFOD$FOD, 
                  levels=c("White Cane", 
                           "Tunnel View AWC", 
                           "Conical View AWC"))


dfpdaggAlertFOD$augmentedObjectDetected[dfpdaggAlertFOD$augmentedObjectDetected == 0] <- NA

dfpdaggAlertFOD$augsmean[dfpdaggAlertFOD$augsmean == 0] <- NA

dfpdaggAlertFOD$augssd[dfpdaggAlertFOD$augssd == 0] <- NA

dfpdaggAlertFOD[1, "auglowerci"] <- NA

dfpdaggAlertFOD[1, "augupperci"] <- NA

ggplot(data = dfpdaggAlertFOD, aes(x = FOD, 
                                group = FOD)) +
  geom_point(aes(y = totalObjectDetected), color="purple", position = position_dodge(0), alpha=1, size = 5) +
  geom_line(aes(y = totalObjectDetected), position = position_dodge(0), 
            alpha = 1, 
            size = 1) +
  geom_errorbar(aes(ymin = totallowerci, 
                    ymax = totalupperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0)) +
  geom_point(aes(y = physicalObjectDetected), color="red2", position = position_dodge(0.2), alpha=1, size = 5) +
  geom_line(aes(y = physicalObjectDetected), position = position_dodge(0.2), 
            alpha = 1, 
            size = 1) +
    geom_errorbar(aes(ymin = physlowerci, 
                                         ymax = physupperci), 
                                     width = 0.2, 
                                     color = "Black", 
                                     position = position_dodge(0.2)) +
  geom_point(aes(y = augmentedObjectDetected), color="skyblue",  position = position_dodge(-0.2), alpha=1, size = 5) +
  geom_line(aes(y = augmentedObjectDetected), position = position_dodge(-0.2), 
            alpha = 1, 
            size = 1) +
    geom_errorbar(aes(ymin = auglowerci, 
                      ymax = augupperci), 
                  width = 0.2, 
                  color = "Black", 
                  position = position_dodge(-0.2)) +
  ylab("Average Number of Alerts") +
  xlab("") +
  scale_y_continuous()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("") 


#----- Mean: Range LMM -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                   #(1 | Scenario),
                   (1 | ParticipantID),
                 data = dfpSumTestID, 
                 REML = FALSE)

r.squaredGLMM(WS.1.Pid) 
summary(WS.1.Pid)

# adding scenario
WS.FOD.Pid <- lmer(avgSpeed ~ FOD +
                     #(1 | Scenario) +
                     (1 | ParticipantID),
                   data = dfpSumTestID, 
                   REML = FALSE
)

r.squaredGLMM(WS.FOD.Pid) 
summary(WS.FOD.Pid)
anova(WS.1.Pid, WS.FOD.Pid)

dfpSumTestID$Range <- as.factor(dfpSumTestID$Range)

# adding scenario
WS.FOD.Range.Pid <- lmer(avgSpeed ~ FOD + Range +
                     #(1 | Scenario) +
                     (1 | ParticipantID),
                   data = dfpSumTestID, 
                   REML = FALSE
)

r.squaredGLMM(WS.FOD.Range.Pid) 
summary(WS.FOD.Range.Pid)
anova(WS.FOD.Pid, WS.FOD.Range.Pid)

#----- Mean: Alerts LMM -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                   #(1 | Scenario),
                   (1 | ParticipantID),
                 data = dfpSumTestID, 
                 REML = FALSE)

r.squaredGLMM(WS.1.Pid) 
summary(WS.1.Pid)
confint(WS.1.Pid)

# adding scenario
WS.FOD.Pid <- lmer(avgSpeed ~ FOD + objectDetected +
                     #(1 | Scenario) +
                     (1 | ParticipantID),
                   data = dfpSumTestID, 
                   REML = FALSE
)

r.squaredGLMM(WS.FOD.Pid) 
summary(WS.FOD.Pid)
confint(WS.FOD.Pid)

# significant - yes
anova(WS.1.Pid, WS.FOD.Pid)

WS.Alert.Pid <- lmer(avgSpeed ~ objectDetected +
                         #(1 | Scenario) +
                         (1 | ParticipantID),
                       data = dfpSumTestID, 
                       REML = FALSE
)

r.squaredGLMM(WS.Alert.Pid) 
summary(WS.Alert.Pid)
confint(WS.Alert.Pid)
anova(WS.1.Pid, WS.Alert.Pid)

#----- Mean: Alerts per obstacle -----

test <- dfp %>%
  dplyr::filter(objDetStart == 1) %>%
  dplyr::group_by(testID, ParticipantID, Scenario, FOD) %>%
  dplyr::select(ObjectDetected, rollingSpeedMedian) %>%
  dplyr::count(ObjectDetected) %>%
  dplyr::rename(NumberOfAlerts = n)

test

moreTest <- test %>%
  dplyr::group_by(testID, ParticipantID, Scenario, FOD) %>%
  dplyr::summarise(NumberOfObjDet = n(),
                   avgNumberOfAlerts = mean(NumberOfAlerts))

moreTest

dfpSumTestID <- merge(x = dfpSumTestID, y = moreTest, by = c("testID", "ParticipantID", "Scenario", "FOD"), all.x = TRUE)

dfpSumTestID$NumberOfObjDet[is.na(dfpSumTestID$NumberOfObjDet)] <- 0

dfpSumTestID$avgNumberOfAlerts[is.na(dfpSumTestID$avgNumberOfAlerts)] <- 0

MeansTest <- dfpSumTestID %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgNumberOfObjDet = mean(NumberOfObjDet),
                   sdNumberOfObjDet = sd(NumberOfObjDet),
                   sdNumberOfAlerts = sd(avgNumberOfAlerts),
                   avgNumberOfAlerts = mean(avgNumberOfAlerts))

MeansTest

summary(glm(NumberOfObjDet ~ FOD, data = dfpSumTestID, family = poisson)) 

summary(glm(avgNumberOfAlerts ~ FOD, data = dfpSumTestID, family = poisson))


# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                   #(1 | Scenario),
                   (1 | ParticipantID),
                 data = dfpSumTestID, 
                 REML = FALSE)

r.squaredGLMM(WS.1.Pid) 
summary(WS.1.Pid)
confint(WS.1.Pid)

# adding scenario
WS.FOD.Pid <- lmer(avgSpeed ~ FOD +
                     #(1 | Scenario) +
                     (1 | ParticipantID),
                   data = dfpSumTestID, 
                   REML = FALSE
)

r.squaredGLMM(WS.FOD.Pid) 
summary(WS.FOD.Pid)
confint(WS.FOD.Pid)

# significant - yes
anova(WS.1.Pid, WS.FOD.Pid)

WS.FOD.NoO.Pid <- lmer(avgSpeed ~ FOD + NumberOfObjDet +
                          #(1 | Scenario) +
                          (1 | ParticipantID),
                        data = dfpSumTestID, 
                        REML = FALSE
)


r.squaredGLMM(WS.FOD.NoO.Pid) 
summary(WS.FOD.NoO.Pid)
# significant - yes
anova(WS.FOD.Pid, WS.FOD.NoO.Pid)


# Add number of alerts 
WS.FOD.NoO.NpA.Pid <- lmer(avgSpeed ~  FOD + NumberOfObjDet + avgNumberOfAlerts +
                                 #(1 | Scenario) +
                                 (1 | ParticipantID),
                               data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(WS.FOD.NoO.NpA.Pid) 
summary(WS.FOD.NoO.NpA.Pid)
anova(WS.FOD.NoO.Pid, WS.FOD.NoO.NpA.Pid)

#----- Mean: Physical Alerts per obstacle -----

PhysAlertNumObj <- dfp %>%
  dplyr::filter(PhysDetStart == 1) %>%
  dplyr::group_by(testID, FOD) %>%
  dplyr::count(ObjectDetected) %>%
  dplyr::rename(NumberOfPhysAlerts = n)

PhysAlertNumObj

PhysAlertPerObj <- PhysAlertNumObj %>%
  dplyr::group_by(testID, FOD) %>%
  dplyr::summarise(NumberOfObjPhysDet = n(),
                   avgNumberOfPhysAlerts = mean(NumberOfPhysAlerts))

PhysAlertPerObj
 

dfpSumTestID <- merge(x = dfpSumTestID, y = PhysAlertPerObj, by = c("testID", "FOD"), all.x = TRUE)

dfpSumTestID$NumberOfObjPhysDet[is.na(dfpSumTestID$NumberOfObjPhysDet)] <- 0

dfpSumTestID$avgNumberOfPhysAlerts[is.na(dfpSumTestID$avgNumberOfPhysAlerts)] <- 0


summary(glm(NumberOfObjPhysDet ~ FOD, data = dfpSumTestID))

summary(glm(avgNumberOfPhysAlerts ~ FOD, data = dfpSumTestID))


summary(lm(avgSpeed ~ NumberOfObjDet + avgNumberOfAlerts, data = PhysAlertPerObjWithSpeed)) 

MeansPhysAlertPerObj <- dfpSumTestID %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgNumberOfObjDet = mean(NumberOfObjPhysDet),
                   sdNumberOfObjDet = sd(NumberOfObjPhysDet),
                   avgNumberOfAlertsPerObj = mean(avgNumberOfPhysAlerts),
                   sdNumberOfAlerts = sd(avgNumberOfPhysAlerts))

MeansPhysAlertPerObj

#----- Mean: Augmented Alerts per obstacle -----

AugAlertNumObj <- dfp %>%
  dplyr::filter(AugDetStart == 1) %>%
  dplyr::group_by(testID, FOD) %>%
  dplyr::count(ObjectDetected) %>%
  dplyr::rename(NumberOfAlerts = n)

AugAlertNumObj

AugAlertPerObj <- AugAlertNumObj %>%
  dplyr::group_by(testID, FOD) %>%
  dplyr::summarise(NumberOfObjDet = n(),
                   avgNumberOfAlerts = mean(NumberOfAlerts))

AugAlertPerObj

summary(lm(NumberOfObjDet ~ FOD, data = AugAlertPerObj)) 
summary(lm(avgNumberOfAlerts ~ FOD, data = AugAlertPerObj)) 


PhysAugAlertPerObjWithSpeed <- merge(x = PhysAlertPerObj, y = testSpeed, by = c("testID", "FOD"), all.x = TRUE)

PhysAugAlertPerObjWithSpeed


summary(lm(avgSpeed ~ NumberOfObjDet + avgNumberOfAlerts, data = PhysAugAlertPerObjWithSpeed)) 

MeansTest <- moreTest %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgNumberOfObjDet = mean(NumberOfObjDet),
                   sdNumberOfObjDet = sd(NumberOfObjDet),
                   NumberOfAlerts = mean(avgNumberOfAlerts),
                   sdNumberOfAlerts = sd(avgNumberOfAlerts))

MeansTest

#----- Mean: Augmented and Physical Alerts per obstacle -----

PhysAlertObjects <- dfp %>%
  dplyr::filter(PhysDetStart == 1) %>%
  dplyr::group_by(ParticipantID, testID, FOD, Range) %>%
  dplyr::count(ObjectDetected) %>%
  dplyr::rename(NumberOfPhysAlertsPerObj = n) 

sumPhysAlertObjects <- PhysAlertObjects %>%
  dplyr::group_by(ParticipantID, testID, FOD, Range) %>%
  dplyr::summarise(NumberOfObjPhysDet = n(),
                   avgNumberOfPhysAlertsPerObj = mean(NumberOfPhysAlertsPerObj))

AugAlertObjects <- dfp %>%
  dplyr::filter(AugDetStart == 1) %>%
  dplyr::group_by(ParticipantID,testID, FOD, Range) %>%
  dplyr::count(ObjectDetected) %>%
  dplyr::rename(NumberOfAugAlertsPerObj = n)

sumAugAlertObjects <- AugAlertObjects %>%
  dplyr::group_by(ParticipantID, testID, FOD, Range) %>%
  dplyr::summarise(NumberOfObjAugDet = n(),
                   avgNumberOfAugAlertsPerObj = mean(NumberOfAugAlertsPerObj))

AlertsPerObstacle <- merge(sumPhysAlertObjects, sumAugAlertObjects, all=TRUE)

AlertsPerObstacle[is.na(AlertsPerObstacle)] <- 0

AlertsPerObstacle <- merge(dfpSumTestID, AlertsPerObstacle, all=TRUE)

AlertsPerObstacle[is.na(AlertsPerObstacle)] <- 0

summary(lm(avgSpeed ~ NumberOfObjPhysDet + NumberOfObjAugDet, data = AlertsPerObstacle)) 

summary(lm(avgSpeed ~ avgNumberOfPhysAlertsPerObj + avgNumberOfAugAlertsPerObj, data = AlertsPerObstacle)) 

summary(lm(avgSpeed ~ objectCollisions + NumberOfObjPhysDet + avgNumberOfPhysAlertsPerObj + NumberOfObjAugDet + avgNumberOfAugAlertsPerObj, data = AlertsPerObstacle)) 

summary(lm(avgSpeed ~ FOD * (NumberOfObjPhysDet + NumberOfObjAugDet), data = AlertsPerObstacle)) 

#----- LMM: Kenetic vs Vibration -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                   (1 | ParticipantID),
                 data = dfpSumTestID, REML = FALSE )

summary(WS.1.Pid)
r.squaredGLMM(WS.1.Pid) 

# add scenario
WS.Coll.Pid <- lmer(avgSpeed ~ objectCollisions +
                     (1 | ParticipantID),
                   data = dfpSumTestID, REML = FALSE)

summary(WS.Coll.Pid)
r.squaredGLMM(WS.Coll.Pid) 
anova(WS.1.Pid, WS.Coll.Pid)

# Add Collisions
WS.Coll.Phys.Pid <- lmer(avgSpeed ~ objectCollisions + physObjectDetected  + 
                      (1 | ParticipantID),
                    data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.Coll.Phys.Pid) 
summary(WS.Coll.Phys.Pid)
anova(WS.Coll.Phys.Pid, WS.Coll.Pid)

# Add obstacles kinetic detected 
WS.Coll.Phys.Aug.Pid <- lmer(avgSpeed ~ objectCollisions + physObjectDetected + augObjectDetected + 
                              (1 | ParticipantID),
                            data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.Coll.Phys.Aug.Pid) 
summary(WS.Coll.Phys.Aug.Pid)
anova(WS.Coll.Phys.Aug.Pid, WS.Coll.Phys.Pid)

# Add Collisions
WS.Phys.Pid <- lmer(avgSpeed ~physObjectDetected  + 
                          (1 | ParticipantID),
                        data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.Phys.Pid) 
summary(WS.Phys.Pid)
anova(WS.Phys.Pid, WS.1.Pid)

# Add Collisions
WS.FOD.Aug.Pid <- lmer(avgSpeed ~ augObjectDetected  + 
                          (1 | ParticipantID),
                        data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.FOD.Aug.Pid) 
summary(WS.FOD.Aug.Pid)
anova(WS.FOD.Aug.Pid, WS.1.Pid)

# Add obstacles kinetic detected 
WS.FOD.Phys.Aug.Pid <- lmer(avgSpeed ~ physObjectDetected + augObjectDetected + 
                               (1 | ParticipantID),
                             data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.FOD.Phys.Aug.Pid) 
summary(WS.FOD.Phys.Aug.Pid)
anova(WS.FOD.Phys.Aug.Pid, WS.Phys.Pid)

#----- LMM: kinetic unique detections and alerts per obstecal -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                   (1 | ParticipantID),
                 data = AlertsPerObstacle, REML = FALSE )

summary(WS.1.Pid)
r.squaredGLMM(WS.1.Pid) 

# Add obstacles kinetic detected 
WS.Phys.Pid <- lmer(avgSpeed ~ NumberOfObjPhysDet + 
                               (1 | ParticipantID),
                             data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.Phys.Pid) 
summary(WS.Phys.Pid)
anova(WS.Phys.Pid, WS.1.Pid)

# Add kinetic alerts per obstacle 
WS.Phys.Per.Pid <- lmer(avgSpeed ~ NumberOfObjPhysDet + avgNumberOfPhysAlertsPerObj +
                                   (1 | ParticipantID),
                                 data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.Phys.Per.Pid) 
summary(WS.Phys.Per.Pid)
anova(WS.Phys.Per.Pid, WS.Phys.Pid)

#----- LMM: With everything -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                        (1 | ParticipantID),
                      data = AlertsPerObstacle, REML = FALSE )

summary(WS.1.Pid)
r.squaredGLMM(WS.1.Pid) 

# add scenario
WS.FOD.Pid <- lmer(avgSpeed ~ FOD +
                             (1 | ParticipantID),
                           data = AlertsPerObstacle, REML = FALSE)

summary(WS.1.Pid)
r.squaredGLMM(WS.Power.Phys.Part) 
anova(WS.1.Pid, WS.FOD.Pid)

# Add Collisions
WS.FOD.Coll.Pid <- lmer(avgSpeed ~ FOD + objectCollisions + 
                             (1 | ParticipantID),
                           data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.FOD.Coll.Pid) 
summary(WS.FOD.Coll.Pid)
anova(WS.FOD.Coll.Pid, WS.FOD.Pid)

# Add obstacles kinetic detected 
WS.FOD.Coll.Phys.Pid <- lmer(avgSpeed ~ FOD + objectCollisions + NumberOfObjPhysDet + 
                                 (1 | ParticipantID),
                               data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.FOD.Coll.Phys.Pid) 
summary(WS.FOD.Coll.Phys.Pid)
anova(WS.FOD.Coll.Phys.Pid, WS.FOD.Coll.Pid)

# Add kinetic alerts per obstacle 
WS.FOD.Coll.Phys.Per.Pid <- lmer(avgSpeed ~ FOD + objectCollisions + NumberOfObjPhysDet + avgNumberOfPhysAlertsPerObj +
                                     (1 | ParticipantID),
                                   data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.FOD.Coll.Phys.Per.Pid) 
summary(WS.FOD.Coll.Phys.Per.Pid)
anova(WS.FOD.Coll.Phys.Per.Pid, WS.FOD.Coll.Phys.Pid)

# Add obstacles vibration detected 
WS.FOD.Coll.Phys.Per.Aug.Pid <- lmer(avgSpeed ~ FOD + objectCollisions + NumberOfObjPhysDet + avgNumberOfPhysAlertsPerObj + NumberOfObjAugDet +
                                         (1 | ParticipantID),
                                       data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.FOD.Coll.Phys.Per.Aug.Pid) 
summary(WS.FOD.Coll.Phys.Per.Aug.Pid)
anova(WS.FOD.Coll.Phys.Per.Aug.Pid, WS.FOD.Coll.Phys.Per.Pid)

# Add vibrations alerts per obstacle physObjectDetected + augObjectDetected +
WS.FOD.Coll.Phys.Per.Aug.Per.Pid <- lmer(avgSpeed ~  objectCollisions + NumberOfObjPhysDet * avgNumberOfPhysAlertsPerObj + NumberOfObjAugDet * avgNumberOfAugAlertsPerObj +
                                         (1 | ParticipantID),
                                       data = AlertsPerObstacle, REML = FALSE)

WS.FOD.Coll.Phys.Per.Aug.Per.Pid <- lmer(avgSpeed ~  objectCollisions + NumberOfObjPhysDet + NumberOfObjAugDet + 
                                           (1 | ParticipantID),
                                         data = AlertsPerObstacle, REML = FALSE)

r.squaredGLMM(WS.FOD.Coll.Phys.Per.Aug.Per.Pid) 
summary(WS.FOD.Coll.Phys.Per.Aug.Per.Pid)
anova(WS.FOD.Coll.Phys.Per.Aug.Per.Pid, WS.FOD.Coll.Phys.Per.Aug.Pid)

#----- Mean: Walking speed -----

# Total Mean Walking Speed
mean(dfpSumTestID$avgSpeed)
sd(dfpSumTestID$avgSpeed)

meanAvgWalkingSpeed <- dfpSumTestID %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(mean(avgSpeed),
                   median(avgSpeed),
                   sd(avgSpeed))

meanAvgWalkingSpeed

# Linear Regression to check significant 
summary(lm(avgSpeed ~ FOD, data = dfpSumTestID)) 

summary(lm(log(avgSpeed) ~ objectDetected, data = dfpSumTestID)) 

summary(lm(avgSpeed ~ physObjectDetected + augObjectDetected, data = dfpSumTestID)) 

summary(lm(avgSpeed ~ physObjectDetected * augObjectDetected, data = dfpSumTestID)) 

summary(lm(log(avgSpeed) ~ FOD*Range, data = dfpSumTestID)) 




summary(lm(avgSpeed ~ objectCollisions + physObjectDetected + augObjectDetected, data = dfpSumTestID)) 




TunnelWalkingSpeed <- dfpSumTestID[!(dfpSumTestID$FOD=="Conical View AWC"),]
summary(lm(log(avgSpeed) ~ FOD*Range, data = TunnelWalkingSpeed)) 

ConicalWalkingSpeed <- dfpSumTestID[!(dfpSumTestID$FOD=="Tunnel View AWC"),]
summary(lm(log(avgSpeed) ~ FOD*Range, data = ConicalWalkingSpeed)) 

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

#Plot walking speed per condition
dfpdaggSpeedTrain <- dfpSumTestID %>%
  dplyr::group_by(Range, FOD, ParticipantID) %>%
  dplyr::summarise(newAvgSpeed = mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  dplyr::mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = dfpdaggSpeedTrain, aes(x = Range, 
                                  y = newAvgSpeed, 
                                  group = FOD, 
                                  color = FOD,
                                  shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size = 5) +
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1) +
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0.1)) +
  scale_fill_hue(name="Condition", 
                 labels=c("White Cane", 
                          "Body-preview aEMA", 
                          "Normal aEMA"))+
  #ggtitle("Number of Objects Detected per Range and Condition")+
  ylab("Average Walking Speed") +
  xlab("Preview Range in Meter") +
  scale_y_continuous()+
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Mean: Walking speed Alert Start -----
  
# Total Mean Walking Speed
mean(dfpSumTestID$avgSpeed)
sd(dfpSumTestID$avgSpeed)
  
meanAvgWalkingSpeedAlertStart <- dfp %>%
  dplyr::filter(objDetStart == 1) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(mean(rollingSpeedMedian),
                     sd(rollingSpeedMedian))
  
meanAvgWalkingSpeedAlertStart

#----- Mean: Collisions -----

mean(dfpSumTestID$objectCollisions)
sd(dfpSumTestID$objectCollisions)

meanCollisions <- dfpSumTestID %>%
  group_by(FOD) %>%
  dplyr::summarise(meanCollisions = mean(objectCollisions),
                   sdCollision = sd(objectCollisions))

meanCollisions

summary(glm(objectCollisions ~ FOD, data = dfpSumTestID)) 


summary(glm(objectCollisions ~ physObjectDetected + augObjectDetected, data = dfpSumTestID)) 

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                    #(1 | Scenario),
                    (1 | ParticipantID),
                  data = dfpSumTestID, 
                  REML = FALSE)

r.squaredGLMM(WS.1.Pid) 
summary(WS.1.Pid)
confint(WS.1.Pid)

# adding scenario
WS.FOD.Pid <- lmer(avgSpeed ~ FOD +
                          #(1 | Scenario) +
                          (1 | ParticipantID),
                        data = dfpSumTestID, 
                        REML = FALSE
)

r.squaredGLMM(WS.FOD.Pid) 
summary(WS.FOD.Pid)
confint(WS.FOD.Pid)

# significant - yes
anova(WS.1.Pid, WS.FOD.Pid)

# adding scenario
WS.FOD.Range.Pid <- lmer(avgSpeed ~ FOD + Range +
                     #(1 | Scenario) +
                     (1 | ParticipantID),
                   data = dfpSumTestID, 
                   REML = FALSE
)

r.squaredGLMM(WS.FOD.Range.Pid) 
summary(WS.FOD.Range.Pid)
confint(WS.FOD.Range.Pid)

# significant - yes
anova(WS.FOD.Pid, WS.FOD.Range.Pid)

WS.FOD.Coll.Pid <- lmer(avgSpeed ~ objectCollisions +
                              #(1 | Scenario) +
                              (1 | ParticipantID),
                            data = dfpSumTestID, 
                            REML = FALSE
)


r.squaredGLMM(WS.FOD.Coll.Pid) 
summary(WS.FOD.Coll.Pid)
# significant - yes
anova(WS.1.Pid, WS.FOD.Coll.Pid)

#----- Mean: Collisions per Obstacle -----

# Number of obstacles collided with
CollNumObj <- dfp %>%
  dplyr::filter(objCollStart == 1) %>%
  dplyr::group_by(testID, FOD, TimeSeconds) %>%
  dplyr::count(ObjectCollision) %>%
  dplyr::rename(NumberOfColl = n) %>% 
  dplyr::summarise(avgSpeed = mean(rollingSpeedMedian))

CollNumObj

# Number of collisions per obstacle 
CollPerObj <- CollNumObj %>%
  dplyr::group_by(testID, FOD) %>%
  dplyr::summarise(NumberOfObjColl = n(),
                   avgNumberOfColl = mean(NumberOfColl))

CollPerObj

summary(lm(NumberOfObjColl ~ FOD, data = CollPerObj)) 

summary(lm(avgNumberOfColl ~ FOD, data = CollPerObj)) 

MeansCollPerObj <- CollPerObj %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgNumberOfObjColl = mean(NumberOfObjColl),
                   sdNumberOfObjColl = sd(NumberOfObjColl),
                   avgNumberOfCollPerObj = mean(avgNumberOfColl),
                   sdNumberOfColl = sd(avgNumberOfColl))

MeansCollPerObj

#----- Mean: Collisions and Physical Alerts -----

# how many of the collisions was the user alerted to just before the collision?
PhysAlertBeforeColl <- dfp %>%
  dplyr::group_by(ParticipantID, FOD) %>%
  dplyr::filter(objCollStart == 1) %>% 
  dplyr::select(ObjectCollision, LastObjPhysDet) %>%
  dplyr::mutate(NumberColl = 1:n(),
                TotalNumberColl = max(NumberColl)) %>%
  dplyr::filter(ObjectCollision == LastObjPhysDet) %>% 
  dplyr::mutate(NumberPhysBeforeColl = 1:n(), 
                TotalNumberPhysBeforeColl = max(NumberPhysBeforeColl)) %>%
  dplyr::summarise(ProcentagePhysBeforeColl = mean(TotalNumberPhysBeforeColl / TotalNumberColl * 100),
                   NumberOfColl = max(TotalNumberColl)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(ProcentagePhysBeforeColl = mean(ProcentagePhysBeforeColl))

PhysAlertBeforeColl 

PhysAlertBeforeColl %>%
  ggplot(aes(x = ProcentagePhysBeforeColl, color = FOD)) +
  geom_density() +
  theme_classic()
  
# coll and alerts per obstacle

alertsAndCollPerObj <- merge(MeansPhysAlertPerObj, MeansCollPerObj, by="FOD")

alertsAndCollPerObj <- alertsAndCollPerObj[ , -which(names(alertsAndCollPerObj) %in% c("sdNumberOfObjColl","sdNumberOfColl","sdNumberOfObjDet","sdNumberOfAlerts"))]

alertsAndCollPerObj

alertsAndCollPerObj %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(ProcentagePhysDetVSColl = mean(avgNumberOfObjColl  / avgNumberOfObjDet  * 100),
                   ProcentageNumPhysDetVSNumColl = mean(avgNumberOfCollPerObj / avgNumberOfAlertsPerObj  * 100))


# Prev alerted obstacles

abc <- PhysAlertNumObj %>%
  dplyr::rename(PhysAlertTime = "TimeSeconds",
         ObjPhysAlertedTo = "ObjectDetected",
         NumberOfAlertsPerObj = "NumberOfAlerts")

abcd <- CollNumObj %>%
  dplyr::rename(CollTime = "TimeSeconds",
                ObjCollWith = "ObjectCollision",
                NumberOfCollPerObj = "NumberOfColl")

test1 <- merge(PhysAlertNumObj, CollNumObj, all=TRUE) %>%
  arrange(testID)

test2 <- test1 %>%
  group_by(testID, FOD) %>%
  mutate(CollAlertedTo = ifelse(ObjectCollision == lag(ObjectDetected), "Alerted to",
                                   ifelse(ObjectCollision != lag(ObjectDetected), "Not Alerted to", NA))) %>%
  ungroup()

test2

test2 %>%
  group_by(FOD) %>%
  count(CollAlertedTo, na.rm = TRUE)

#----- Mean: Collisions and Augmented Alerts -----

AugAlertBeforeColl <- dfp %>%
  dplyr::group_by(FOD) %>%
  dplyr::filter(objCollStart == 1) %>% 
  dplyr::select(ObjectCollision, LastObjAugDet) %>%
  dplyr::mutate(NumberColl = 1:n(),
                TotalNumberColl = max(NumberColl)) %>%
  dplyr::filter(ObjectCollision == LastObjAugDet) %>% 
  dplyr::mutate(NumberAugBeforeColl = 1:n(), 
                TotalNumberAugBeforeColl = max(NumberAugBeforeColl)) %>%
  dplyr::summarise(ProcentageAugBeforeColl = mean(TotalNumberAugBeforeColl/TotalNumberColl*100))

AugAlertBeforeColl

#----- Transforming Data and testing for normality -----

### Check if logging the data will have any effect on the data

#_______________________________________________________________________________________________________
## Data before any transformation
#_______________________________________________________________________________________________________

# How is the data if we just add a linear function
summary(lm(avgSpeed ~ objectDetected, data = dfpSumTestID)) 
# R-squared:  0.20

ggplot(dfpSumTestID, aes(x = objectDetected, y = avgSpeed)) + 
  geom_point(colour = 'gray') +
  geom_smooth(se=FALSE) +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Scenario") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

### Linear power function

# Finding the residuals - linear power function
lm <- lm(avgSpeed ~ objectDetected, data = dfpSumTestID)

lmResiduals <- resid(lm)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(dfpSumTestID$objectDetected, lmResiduals) + abline(0,0) # Could be a lot better the bottom of the line is ot heavy

# Test the Residuals on a qqplot to look for normality
qqPlot(lmResiduals) # Does not look too good the end is off

# Test for normality
shapiro.test(lmResiduals) # not good - data does not look normally distributed

### Summery : Data does not look normally distributed R2 = 0.20

# /////////////////////////////////////////////////////////////////////////////////////////////////

# mixed linear Power function

# Finding the residuals mixed Power function
WalkingSpeed <- lmer(avgSpeed ~ objectDetected + 
                             (1 + objectDetected | ParticipantID) +
                             (1 + objectDetected | RunNumber) +
                             (1 + objectDetected | Scenario),
                           data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(WalkingSpeed) 
# marginal R squared = 0.15 (fixed effects),
# conditional R squared = 0.84 (fixed effects plus random effects)

WalkingSpeed.Residuals <- resid(WalkingSpeed)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(dfpSumTestID$objectDetected, WalkingSpeed.Residuals) + abline(0,0) # looks okay, a bit too compact in the start

# Test the Residuals on a qqplot to look for normality
qqPlot(WalkingSpeed.Residuals) # not too good - jumps off towards each end 

# Test for normality
shapiro.test(WalkingSpeed.Residuals) # not too good - data should not be normal distributed

# Looks normally distributed but very low R2 especially for the mixed model


#_______________________________________________________________________________________________________
## Data after logging speed
#_______________________________________________________________________________________________________

# How is the data if we make a log transformation on the walking speed. Making it Exponential function
summary(lm(log(avgSpeed) ~ objectDetected, data = dfpSumTestID)) 
# R-squared:  0.27

ggplot(dfpSumTestID, aes(x = objectDetected, y = log(avgSpeed))) + 
  geom_point(colour = 'gray') +
  geom_smooth(se=FALSE) +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Scenario") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

# Making the Linear Exponential function
lmExponential <- lm(log(avgSpeed) ~ objectDetected, data = dfpSumTestID)

# Finding the residuals
lmExponentialResiduals <- resid(lmExponential) 

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(dfpSumTestID$objectDetected+1), lmExponentialResiduals) + abline(0,0) # looks good

# Test the Residuals on a qqplot to look for normality
qqPlot(lmExponentialResiduals) # looks good - a bit towards the end

# Test for normality
shapiro.test(lmExponentialResiduals) # close to not be rejected so not too good not too bad

### Summery : A logged linear function is close to normally distributed. R2 = 0.27

# Mixed Exponential function

# Finding the residuals - Mixed Exponential function
WalkingSpeed.Exponential <- lmer(log(avgSpeed) ~ objectDetected + 
                                   (1 + objectDetected | ParticipantID) +
                                   (1 + objectDetected | RunNumber),
                                 data = dfpSumTestID, REML = FALSE
)

# Calculating the R2
r.squaredGLMM(WalkingSpeed.Exponential)
# marginal R squared = 0.21 (fixed effects),
# conditional R squared = 0.79 (fixed effects plus random effects)

# Finding the residuals
WalkingSpeed.Exponential.Residuals <- resid(WalkingSpeed.Exponential) 

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(dfpSumTestID$objectDetected+1), WalkingSpeed.Exponential.Residuals) + abline(0,0) # looks good

# Test the Residuals on a qqplot to look for normality
qqPlot(WalkingSpeed.Exponential.Residuals) # looks good - a bit towards the end

# Test for normality
shapiro.test(WalkingSpeed.Exponential.Residuals) # Very good - data should be normal distributed

### Summery: A logged mixed linear function should be normally distributed R2 = 0.21


#_______________________________________________________________________________________________________
## Data after logging speed and object detections
#_______________________________________________________________________________________________________

# How is the data if we make a log transformation on the walking speed and alerts. Making it a power function 
summary(lm(log(avgSpeed) ~ log(objectDetected+1), data = dfpSumTestID)) 
#R-squared:  0.25

ggplot(dfpSumTestID, aes(x = log(objectDetected), y = log(avgSpeed))) + 
  geom_point(colour = 'gray') +
  geom_smooth(se=FALSE) +
  theme_bw() +
  ylab("Average Walking Speed (m/s)") +
  xlab("Scenario") +
  # facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")
### Summary : Based on the above comparison logging the walking speed seems to be the best fit

### Linear power function

# Finding the residuals - linear power function
lmPower <- lm(log(avgSpeed) ~ log(objectDetected+1), data = dfpSumTestID)

lmPowerResiduals <- resid(lmPower)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(dfpSumTestID$objectDetected+1), lmPowerResiduals) + abline(0,0) # Could be a lot better the bottom of the line is ot heavy

# Test the Residuals on a qqplot to look for normality
qqPlot(lmPowerResiduals) # Does not look too good the end is off

# Test for normality
shapiro.test(lmPowerResiduals) # not good - data does not look normally distributed

### Summery : A Linear power function is not a good transformation of the data. R2 = 0.25

# mixed linear Power function

# Finding the residuals mixed Power function
WalkingSpeed.Power <- lmer(log(avgSpeed) ~ log(objectDetected+1) + 
                                    (1+ log(1 + objectDetected) | ParticipantID) +
                                    (1 + log(1 + objectDetected) | RunNumber) +
                                    (1 + log(1 + objectDetected) | Scenario),
                                  data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(WalkingSpeed.Power) 
# marginal R squared = 0.11 (fixed effects),
# conditional R squared = 0.84 (fixed effects plus random effects)

WalkingSpeed.Power.Residuals <- resid(WalkingSpeed.Power)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(dfpSumTestID$objectDetected+1), WalkingSpeed.Power.Residuals) + abline(0,0) # looks good, a bit heavy

# Test the Residuals on a qqplot to look for normality
qqPlot(WalkingSpeed.Power.Residuals) # looks good - a bit towards each end 

# Test for normality
shapiro.test(WalkingSpeed.Power.Residuals) # Very good - data should be normal distributed

# Looks normally distributed but very low R2 espacally for the mixed model

#_______________________________________________________________________________________________________
## Conclusion
#_______________________________________________________________________________________________________

### After testing four different transformation the Mixed Linear Function (Exponential only one log) seemst to explain the data the most

#----- LMEM: Walking Speed vs alerts -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                        #(1 | Scenario),
                        (1 | ParticipantID),
                      data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(WS.1.Pid) 
summary(WS.1.Pid)
confint(WS.1.Pid)
plot(WS.1.Pid, type = c("p", "smooth"))
qqmath(WS.1.Pid, id = 0.05)

#Add FOD
WS.FOD.Pid <- lmer(avgSpeed ~ FOD +
                             (1 | ParticipantID),
                           data = dfpSumTestID, 
                           REML = FALSE
)


r.squaredGLMM(WS.FOD.Pid) 
summary(WS.FOD.Pid)
# significant - yes
anova(WS.1.Pid, WS.FOD.Pid)

# Add number of alerts 
WS.FOD.Alerts.Pid <- lmer(avgSpeed ~ FOD + objectDetected + 
                                    (1 | ParticipantID),
                                  data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(WS.FOD.Alerts.Pid) 
summary(WS.FOD.Alerts.Pid)

# significant - yes
anova(WS.FOD.Pid, WS.FOD.Alerts.Pid)



#----- LMEM: Walking Speed vs Phy-Alerts -----

# Making the NULL model, only including participants
WS.1.Pid <- lmer(avgSpeed ~ 1 +
                        (1 | ParticipantID),
                      data = dfpSumTestID, REML = FALSE )

# Add number of alerts 
WS.FOD.Pid <- lmer(avgSpeed ~ FOD + 
                        (1 | ParticipantID),
                        data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.1.Pid, WS.FOD.Pid)

# Add number of alerts 
WS.FOD.PAlert.Pid <- lmer(avgSpeed ~ FOD + physObjectDetected + 
                                 (1 | ParticipantID),
                                 data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.FOD.Pid, WS.FOD.PAlert.Pid)

# add slope to participants
WS.Power.Part.Slope.Scen.phyAlert <- lmer(log(avgSpeed) ~ FOD + physObjectDetected + augO + 
                                         (1 | ParticipantID),
                                       data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen.phyAlert, WS.Power.Part.Slope.Scen.phyAlert)

# add slope to scenario
WS.Power.Part.Slope.Scen.Slope.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                               (1 + log(1 + physObjectDetected) | ParticipantID) +
                                               (1 + log(1 + physObjectDetected) | Scenario),
                                             data = dfpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.phyAlert, WS.Power.Part.Slope.Scen.Slope.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                   (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                   (1 + log(1 + physObjectDetected) | Scenario) +
                                                   (1 | RunNumber),
                                                 data = dfpSumTestID, REML = FALSE
)

# significant - Yes 
anova(WS.Power.Part.Slope.Scen.Slope.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                         (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                         (1 + log(1 + physObjectDetected) | Scenario) +
                                                         (1 + log(1 + physObjectDetected) | RunNumber),
                                                       data = dfpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert.augAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                            (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                            (1 | Scenario) +
                                                            (1 | RunNumber),
                                                          data = dfpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert)







WS.Power.Part.Scen.Run <- lmer(log(avgSpeed) ~ 1 + 
                                 (1 | ParticipantID) +
                                 (1 | Scenario) +
                                 (1 | RunNumber),
                               data = dfpSumTestID, REML = FALSE
)

# add run number
WS.Power.Part.Scen.Run.phyAlert <- lmer(log(avgSpeed) ~ 1 + augObjectDetected +
                                 (1 | ParticipantID) +
                                 (1 | Scenario) +
                                 (1 | RunNumber),
                               data = dfpSumTestID, REML = FALSE
)

anova(WS.Power.Part.Scen.Run, WS.Power.Part.Scen.Run.phyAlert)

# add run number
WS.Power.Part.Scen.Run.phyAlert.augAlert <- lmer(log(avgSpeed) ~ 1 + augObjectDetected + physObjectDetected +  + 
                                          (1 | ParticipantID) +
                                          (1 | Scenario) +
                                          (1 | RunNumber),
                                        data = dfpSumTestID, REML = FALSE
)

anova(WS.Power.Part.Scen.Run.phyAlert.augAlert, WS.Power.Part.Scen.Run.phyAlert)

#----- LMEM: Walking Speed vs Aug-Alerts -----

# Making the NULL model, only including participants
WS.Power.Part <- lmer(log(avgSpeed) ~ 1 +
                        (1 | ParticipantID),
                      data = dfpSumTestID, REML = FALSE )

# add scenario
WS.Power.Part.Scen <- lmer(log(avgSpeed) ~ 1 +
                             (1 | ParticipantID) +
                             (1 | Scenario),
                           data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part, WS.Power.Part.Scen)

# Add number of alerts 
WS.Power.Part.Scen.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                      (1 | ParticipantID) +
                                      (1 | Scenario),
                                    data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen, WS.Power.Part.Scen.augAlert)

# add slope to participants
WS.Power.Part.Slope.Scen.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                            (1 + log(1 + augObjectDetected) | ParticipantID) +
                                            (1 | Scenario),
                                          data = dfpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen.augAlert, WS.Power.Part.Slope.Scen.augAlert)

# add slope to scenario
WS.Power.Part.Slope.Scen.Slope.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                  (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                  (1 + log(1 + augObjectDetected) | Scenario),
                                                data = dfpSumTestID, REML = FALSE
)

# significant - yes 
anova(WS.Power.Part.Slope.Scen.augAlert, WS.Power.Part.Slope.Scen.Slope.augAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                      (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                      (1 + log(1 + augObjectDetected) | Scenario) +
                                                      (1 | RunNumber),
                                                    data = dfpSumTestID, REML = FALSE
)

# significant - Yes 
anova(WS.Power.Part.Slope.Scen.Slope.augAlert, WS.Power.Part.Slope.Scen.Slope.Run.augAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                            (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                            (1 + log(1 + augObjectDetected) | Scenario) +
                                                            (1 + log(1 + augObjectDetected) | RunNumber),
                                                          data = dfpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.augAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.augAlert)

#----- LMEM: alerts VS condition -----

# Making the NULL model, only including participants
Log.Alert.1.PCPs <- lmer(objectDetected ~ 1 +
                        (1 | ParticipantID),
                      data = dfpSumTestID, REML = FALSE)

r.squaredGLMM(Log.Alert.1.PCPs) 
summary(Log.Alert.1.PCPs)
confint(Log.Alert.1.PCPs)
plot(Log.Alert.1.PCPs, type = c("p", "smooth"))
qqmath(Log.Alert.1.PCPs, id = 0.05)

# adding scenario
Log.Alert.FOD.PCPs <- lmer(objectDetected ~ FOD +
                             (1 | ParticipantID) +
                             (1 | RunNumber),
                           data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.Alert.FOD.PCPs) 
summary(Log.Alert.FOD.PCPs)
confint(Log.Alert.FOD.PCPs)
plot(Log.Alert.FOD.PCPs, type = c("p", "smooth"))


# significant - yes
anova(Log.Alert.1.PCPs, Log.Alert.FOD.PCPs)

# Add number of alerts 
Log.Alert.FOD.PCPs.slop <- lmer(objectDetected ~ FOD + 
                                 (1 | ParticipantID),
                               data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.Alert.FOD.PCPs.slop) 
summary(Log.Alert.FOD.PCPs.slop)

# significant - yes
anova(Log.Alert.FOD.PCPs, Log.Alert.FOD.PCPs.slop)

# add slope to participants
Log.Alert.FOD.PCPs.slop.Run <- lmer(objectDetected ~  FOD + 
                                      (1 | ParticipantID) +
                                      (1 | RunNumber),
                                    data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.Alert.FOD.PCPs.slop.Run) 
summary(Log.Alert.FOD.PCPs.slop.Run)

# significant - yes
anova(Log.Alert.FOD.PCPs.slop, Log.Alert.FOD.PCPs.slop.Run)

# add slope to scenario
Log.WS.Alerts.PCPs.Slope.Scen.Run <- lmer(log(avgSpeed) ~ objectDetected + 
                                            (1 + objectDetected | ParticipantID) +
                                            (1 | Scenario) +
                                            (1 | RunNumber),
                                          data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.WS.Alerts.PCPs.Slope.Scen.Run) 
summary(Log.WS.Alerts.PCPs.Slope.Scen.Run)

# significant - Yes
anova(Log.WS.Alerts.PCPs.Run.Scen, Log.WS.Alerts.PCPs.Slope.Scen.Run)

# add run number
Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope <- lmer(log(avgSpeed) ~ objectDetected + 
                                                  (1 + objectDetected | ParticipantID) +
                                                  (1 | Scenario) +
                                                  (1 + objectDetected | RunNumber),
                                                data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope)
summary(Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope)

# significant - Yes 
anova(Log.WS.Alerts.PCPs.Slope.Scen.Run, Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope)

# add run number
Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope <- lmer(log(avgSpeed) ~ objectDetected + 
                                                        (1 + objectDetected | ParticipantID) +
                                                        (1 + objectDetected | Scenario) +
                                                        (1 + objectDetected | RunNumber),
                                                      data = dfpSumTestID, REML = FALSE
)

r.squaredGLMM(Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope)
summary(Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope)

# significant - No 
anova(Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope, Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope)



# LMER model - linear mixed effect models ----------------


lme1 <- lmer(objColl_Value ~ FOD + (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
lmeNull <- lmer(objColl_Value ~ (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
anova(lme1, lmeNull)


#----- Plot Alert vs speed -----
dfpSumTestID %>%
  ggplot(aes(
    x = log(objectDetected+1),
    #x = log(physObjectDetected+1),
    #x = log(augObjectDetected+1),
    y = log(avgSpeed)#,
    #color = factor(ParticipantID)
  )) +
  geom_point() +
  geom_smooth(se = F) +
  #geom_smooth(method=lm,se = F) +
  theme_bw() +
  ylab("Average Walking Speed (m/s) per Test Run") +
  xlab("Number of Alerts per Test Run") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

dfpSumTestID %>%
  ggplot(aes(
    #x = log(objectDetected+1),
    x = log(physObjectDetected+1),
    y = log(augObjectDetected+1),
    #y = log(avgSpeed)#,
    size = avgSpeed
  )) +
  geom_point() +
  #geom_smooth(se = F) +
  #geom_smooth(method=lm,se = F) +
  theme_bw() +
  ylab("Augmented Alerts") +
  xlab("Number of Physical Alerts") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Plot Alert Cost -----

#----- Plot Alert Cost: Total -----
dfp %>%
  filter(TimeSinceObjDetStart < 4 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_smooth() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14)
  ) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

#----- Plot Alert Cost: Per AWC (cleaned for paper) -----
dfp %>%
  filter(TimeSinceObjDetStart < 5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian#,
    #colour = FOD
  )) +
  geom_smooth(se = F) +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

dfp %>%
  filter(TimeSinceObjDetStart < 4 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian#,
    #colour = FOD
  )) +
  #geom_smooth(color = "orange", se = F) +
  #geom_point(aes(group=FOD, colour = FOD))+
  geom_smooth(aes(colour = FOD, linetype = as.factor(Range)), se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- How Much and How Often - Alert Cost: Per AWC -----

#_______________________________________________________________________________________________________
# Overall Avg for walking speed at start and after 1.2 of an alert
#_______________________________________________________________________________________________________

#Speed at alert start
avgWalkingSpeedAtDetStart <- dfp %>%
  dplyr::filter(TimeSinceObjDetStart > 0 & TimeSinceObjDetStart < 0.1 & !is.na(TimeSinceObjDetStart)) %>%
  dplyr::group_by(ParticipantID, FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedDetStart = mean(rollingSpeedMedian, na.rm=TRUE)) 

#Speed at alert after 1.2s
avgWalkingSpeedAtafter1SecDetStart <- dfp %>%
  filter(TimeSinceObjDetStart > 1.2 & TimeSinceObjDetStart < 1.3 & !is.na(TimeSinceObjDetStart)) %>%
  dplyr::group_by(ParticipantID, FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedDetSlut = mean(rollingSpeedMedian, na.rm=TRUE))

# difference in speed at and after alerts 
SpeedAtandAfterAlert <- merge(avgWalkingSpeedAtDetStart, avgWalkingSpeedAtafter1SecDetStart)

OverallAvgSpeed <- SpeedAtandAfterAlert %>%
  group_by(FOD) %>%
  dplyr::summarise(SlowSpeedStart = mean(avgSpeedDetStart),
                   SlowSpeedSlut = mean(avgSpeedDetSlut))

OverallAvgSpeed

#_______________________________________________________________________________________________________
# Split Avg for walking speed at start and after 1.2 of an alert for increasing and decreasing speeds
#_______________________________________________________________________________________________________

SpeedAtandAfterAlert <- SpeedAtandAfterAlert %>%
  dplyr::mutate(SpeedDiff = avgSpeedDetSlut - avgSpeedDetStart) %>%
  group_by(FOD) %>%
  dplyr::mutate(TotalNumberOfAlert = max(1:n()))

SlowedDown <- SpeedAtandAfterAlert %>%
  filter(SpeedDiff < 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(SlowSpeedStart = mean(avgSpeedDetStart),
                   SlowSpeedSlut = mean(avgSpeedDetSlut),
                   AvgSpeedDown = mean(SpeedDiff),
                   sdSpeedDown = sd(SpeedDiff),
                   TotalNumberOfAlert = max(TotalNumberOfAlert),
                   NumberOfAlertSpeedDown = max(NumberOfAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfAlertSpeedDown / TotalNumberOfAlert * 100,
                SDProcentageSpeedDrop = sd(ProcentageSpeedDrop)) 

SlowedDown

SpeedUp <- SpeedAtandAfterAlert %>%
  filter(SpeedDiff > 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(FastSpeedStart = mean(avgSpeedDetStart),
                   FastSpeedSlut = mean(avgSpeedDetSlut),
                   AvgSpeedUp = mean(SpeedDiff),
                   sdSpeedUp = sd(SpeedDiff),
                   TotalNumberOfAlert = max(TotalNumberOfAlert),
                   NumberOfAlertSpeedUp = max(NumberOfAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfAlertSpeedUp/TotalNumberOfAlert*100)

SpeedUp

#----- Plot Alert Cost: Split increase and decrease -----

test <- dfp %>%
  filter(TimeSinceObjDetStart > 0 & TimeSinceObjDetStart <= 0.1 & !is.na(TimeSinceObjDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedDetStart = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(dfp) 

test <- dfp %>%
  filter(TimeSinceObjDetStart > 1.2 & TimeSinceObjDetStart < 1.3 & !is.na(TimeSinceObjDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedDetSlut = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(test) %>%
  arrange(rowNum)

test <- test %>%
  dplyr::mutate(SpeedDiff = avgSpeedDetSlut - avgSpeedDetStart) %>%
  dplyr::mutate(SpeedChange = ifelse(SpeedDiff < 0, "Slowed Down After Alert",
                              ifelse(SpeedDiff > 0, "Speeded Up After Alert", NA))) %>%
  dplyr::mutate(AlertDistance = ifelse(1 > ObjectDistance & ObjectDistance > 0, 1,
                                ifelse(2 > ObjectDistance & ObjectDistance >= 1, 2,
                                ifelse(3 > ObjectDistance & ObjectDistance >= 2, 3,
                                ifelse(ObjectDistance >= 3, 4, NA)))))

test %>%
  filter(TimeSinceObjDetStart < 5 & TimeSinceObjDetStart > 0 & !is.na(SpeedChange) & SpeedChange == "Slowed Down After Alert") %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian#,
    # = SpeedChange
  )) +
  #geom_smooth(color = "orange", se = F) +
  #geom_point(aes(group=FOD, colour = FOD))+
  geom_smooth(aes(colour = as.factor(Range)), se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")


#----- Plot Alert Cost: Per AWC and Range -----
dfp %>%
  # filter(TimeSinceObjDetStart < 5 & TimeSinceObjDetStart > 0) %>%
  filter(objDetStartDistance > 2) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = Range
  )) +
  geom_smooth() +
  theme_bw() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  # facet_grid(rows = vars(Range))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

#----- Plot Alert Cost: Per Range (cleaned for paper) -----
dfp %>%
  filter(TimeSinceObjDetStart < 5 & TimeSinceObjDetStart > 0 & !is.na(TimeSinceObjDetStart)) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = factor(Range)
  )) +
  # geom_hline(yintercept=0) +
  geom_smooth(se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  guides(colour = guide_legend(title="Range")) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

test <- dfp %>%
  filter(objDetStart == 1) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgSpeedDetStart = mean(rollingSpeedMedian))


test2 <- dfp %>%
  filter(TimeSinceObjDetStart > 1.2 & TimeSinceObjDetStart < 1.3 & !is.na(TimeSinceObjDetStart)) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgSpeedDetStart = mean(rollingSpeedMedian))

#----- Plot Alert Cost: All participants (Not Working) -----
dfp %>%
  filter(TimeSinceObjDetStart < 4 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_smooth(aes(
    shape = factor(ParticipantID)
  ),
  colour = "gray80",
  se = FALSE
  ) +
  geom_hline(yintercept = 0) +
  geom_smooth(
    fill = "skyblue2"
  ) +
  coord_cartesian(ylim = c(-0.08, 0.08)) +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14)
  ) #+
# scale_color_discrete("FOA") +
# scale_shape_discrete("FOA")

dfp %>%
  filter(TimeSinceObjDetStart < 4 & TimeSinceObjDetStart > 0 & ParticipantID != 2 & ParticipantID != 9 & ParticipantID != 10) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_smooth(aes(
    shape = factor(ParticipantID)
  ),
  colour = "gray80",
  se = FALSE
  ) +
  # scale_color_manual(values="#999999") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Change in Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14)
  ) #+
# scale_color_discrete("FOA") +
# scale_shape_discrete("FOA")

#----- Plot Alert Cost: Phys vs Aug (cleaned for paper) -----

dfp %>%
  filter(TimeSincePhysDetStart < 5 & TimeSincePhysDetStart > 0 & TimeSinceAugDetStart < 5 & TimeSinceAugDetStart > 0) %>%
  ggplot() +
  # geom_hline(yintercept=0, linetype = "dashed") +
  geom_smooth(aes(
    x = TimeSincePhysDetStart,
    y = rollingSpeedMedian,
    # linetype = factor(day),
    color = "Kinetic Alerts"
  ),
  se = F
  ) +
  geom_smooth(aes(
    x = TimeSinceAugDetStart,
    y = rollingSpeedMedian,
    # linetype = factor(day),
    color = "Vibration Alerts"
  ),
  se = F
  ) +
  # geom_smooth(data = augDetDataTW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#D16103", se = F) +
  # geom_smooth(data = augDetDataCW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#52854C", se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  #facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) 

#___________________________________________________________________________________
## Only Alerts that slowed down the user
#___________________________________________________________________________________


testPhys <- dfp %>%
  filter(TimeSincePhysDetStart > 0 & TimeSincePhysDetStart <= 0.1 & !is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedPhysDetStart = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(dfp) 

testPhys <- dfp %>%
  filter(TimeSincePhysDetStart > 1.2 & TimeSincePhysDetStart < 1.3 & !is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedPhysDetSlut = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(testPhys) %>%
  arrange(rowNum)

testPhys <- testPhys %>%
  #dplyr::select(ParticipantID, FOD, Range, testID, rollingSpeedMedian, TimeSincePhysDetStart, avgSpeedPhysDetStart, avgSpeedPhysDetSlut) %>%
  dplyr::mutate(PhysSpeedDiff = avgSpeedPhysDetSlut - avgSpeedPhysDetStart) %>%
  dplyr::mutate(PhysSpeedChange = ifelse(PhysSpeedDiff < 0, "Slowed Down After Alert",
                                     ifelse(PhysSpeedDiff > 0, "Speeded Up After Alert", NA)))

testAug <- dfp %>%
  filter(TimeSinceAugDetStart > 0 & TimeSinceAugDetStart <= 0.1 & !is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedAugDetStart = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(dfp) 

testAug <- dfp %>%
  filter(TimeSinceAugDetStart > 1.2 & TimeSinceAugDetStart < 1.3 & !is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(avgSpeedAugDetSlut = mean(rollingSpeedMedian, na.rm=TRUE)) %>%
  right_join(testAug) %>%
  arrange(rowNum)

testAug <- testAug %>%
  #dplyr::select(ParticipantID, FOD, Range, testID, rollingSpeedMedian, TimeSinceAugDetStart, avgSpeedAugDetSlut, avgSpeedAugDetStart) %>%
  dplyr::mutate(AugSpeedDiff = avgSpeedAugDetSlut - avgSpeedAugDetStart) %>%
  dplyr::mutate(AugSpeedChange = ifelse(AugSpeedDiff < 0, "Slowed Down After Alert",
                                         ifelse(AugSpeedDiff > 0, "Speeded Up After Alert", NA)))

testPhysAug <- right_join(testPhys, testAug)

testPhysAug %>%
  filter(TimeSincePhysDetStart < 5 &  TimeSinceAugDetStart < 5 &  !is.na(AugSpeedChange) & AugSpeedChange == "Slowed Down After Alert" & !is.na(PhysSpeedChange) & PhysSpeedChange == "Slowed Down After Alert") %>%
  ggplot() +
  geom_smooth(aes(
    x = TimeSincePhysDetStart,
    y = rollingSpeedMedian,
  #  color =  as.factor(ParticipantID),
    group = as.factor(ParticipantID)
  ),
  color = "gray",
  se = F
  ) +
  geom_smooth(aes(
    x = TimeSinceAugDetStart,
    y = rollingSpeedMedian,
  #  color =  as.factor(ParticipantID),
    group = as.factor(ParticipantID)
  ),
  color = "gray",
  se = F
  ) +
  geom_smooth(aes(
    x = TimeSincePhysDetStart,
    y = rollingSpeedMedian,
    linetype = as.factor(PhysSpeedChange),
    color = "Kinetic Alerts"
  ),
  se = F
  ) +
  geom_smooth(aes(
    x = TimeSinceAugDetStart,
    y = rollingSpeedMedian,
    linetype = as.factor(AugSpeedChange),
    color = "Vibration Alerts"
  ),
  se = F
  ) +
  # geom_smooth(data = augDetDataTW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#D16103", se = F) +
  # geom_smooth(data = augDetDataCW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#52854C", se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  #facet_grid(rows = vars(FOD))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(linetype=FALSE)

#----- How Much and How Often - Alert Cost: Phys -----

#_______________________________________________________________________________________________________
# Overall Avg for walking speed at physical alert start and after 1.2 of an alert
#_______________________________________________________________________________________________________

# Means for physical detection start
avgWalkingSpeedAtPhysDetStart <- dfp %>%
  filter(TimeSincePhysDetStart > 0 & TimeSincePhysDetStart < 0.1 & !is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgSpeedPhysDetStart = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #medianSpeedPhysDetStart = median(rollingSpeedMedian, na.rm=TRUE),
                   #sdSpeedPhysDetStart = sd(rollingSpeedMedian, na.rm=TRUE)
                   )

avgWalkingSpeedAtPhysDetStart

# Means 1.2s after physical detection start
avgWalkingSpeedAtafter1SecPhysDetStart <- dfp %>%
  filter(TimeSincePhysDetStart > 1.2 & TimeSincePhysDetStart < 1.3 & !is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgAfterSpeedPhysDetStart = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #medianAfterSpeedPhysDetStart = median(rollingSpeedMedian, na.rm=TRUE),
                   #sdAfterSpeedPhysDetStart = sd(rollingSpeedMedian, na.rm=TRUE)
                   )

avgWalkingSpeedAtafter1SecPhysDetStart

PhysavgWalkingSpeed <- merge(avgWalkingSpeedAtPhysDetStart, avgWalkingSpeedAtafter1SecPhysDetStart)

PhysavgWalkingSpeed

#_______________________________________________________________________________________________________
# Split Avg for walking speed at physical alert start and after 1.2 of an alert for increasing and decreasing speeds
#_______________________________________________________________________________________________________

#Speed at Physical alert start
PhysSpeedStart <- dfp %>%
  dplyr::filter(TimeSincePhysDetStart > 0 & TimeSincePhysDetStart < 0.1 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE)) 

#Speed at Physical alert after 1.2s
PhysSpeedSlut <- dfp %>%
  filter(TimeSincePhysDetStart > 1.2 & TimeSincePhysDetStart < 1.3 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE))

# difference in speed at and after physical alerts 
PhysSpeedAtandAfterAlert <- merge(PhysSpeedStart, PhysSpeedSlut)

PhysSpeedAtandAfterAlert <- PhysSpeedAtandAfterAlert %>%
  dplyr::mutate(PhysSpeedDiff = PhysSpeedSlut - PhysSpeedStart) %>%
  group_by(FOD) %>%
  dplyr::mutate(TotalNumberOfPhysAlert = max(1:n()))

PhysSlowedDown <- PhysSpeedAtandAfterAlert %>%
  filter(PhysSpeedDiff < 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedDown = mean(PhysSpeedDiff),
                   sdSpeedDown = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedDown = max(NumberOfPhysAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfPhysAlertSpeedDown / TotalNumberOfPhysAlert * 100) 

PhysSlowedDown

PhysSpeedUp <- PhysSpeedAtandAfterAlert %>%
  dplyr::group_by(FOD) %>%
  dplyr::filter(PhysSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedUp = mean(PhysSpeedDiff),
                   medianSpeedUp  = median(PhysSpeedDiff),
                   sdSpeedUp = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedUp = max(NumberOfPhysAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfPhysAlertSpeedUp/TotalNumberOfPhysAlert*100)

PhysSpeedUp

#----- How Much and How Often - Alert Cost: Aug -----

#_______________________________________________________________________________________________________
# Overall Avg for walking speed at augmented alert start and after 1.2 of an alert
#_______________________________________________________________________________________________________

# Means for augmented detection start
avgWalkingSpeedAtAugDetStart <- dfp %>%
  filter(TimeSinceAugDetStart > 0 & TimeSinceAugDetStart < 0.1 & !is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgSpeedAugDetStart = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #medianSpeedAugDetStart = median(rollingSpeedMedian, na.rm=TRUE),
                   #sdSpeedAugDetStart = sd(rollingSpeedMedian, na.rm=TRUE)
  )

avgWalkingSpeedAtAugDetStart

# Means 1.2s after augmented detection start
avgWalkingSpeedAtafter1SecAugDetStart <- dfp %>%
  filter(TimeSinceAugDetStart > 1.2 & TimeSinceAugDetStart < 1.3 & !is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgAfterSpeedAugDetStart = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #medianAfterSpeedAugDetStart = median(rollingSpeedMedian, na.rm=TRUE),
                   #sdAfterSpeedAugDetStart = sd(rollingSpeedMedian, na.rm=TRUE)
  )

avgWalkingSpeedAtafter1SecAugDetStart

AugAvgWalkingSpeed <- merge(avgWalkingSpeedAtAugDetStart, avgWalkingSpeedAtafter1SecAugDetStart)

AugAvgWalkingSpeed

#_______________________________________________________________________________________________________
# Split Avg for walking speed at augmented alert start and after 1.2 of an alert for increasing and decreasing speeds
#_______________________________________________________________________________________________________

#Speed at Augmented alert start
AugSpeedStart <- dfp %>%
  dplyr::filter(TimeSinceAugDetStart > 0 & TimeSinceAugDetStart < 0.1 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(AugSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE),
                   GapDuration = max(TimeSinceObjDetStart))

mean(AugSpeedStart$AugSpeedStart)

#Speed at augmented alert after 1.2s
AugSpeedSlut <- dfp %>%
  filter(TimeSinceAugDetStart > 1.2 & TimeSinceAugDetStart < 1.3 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(AugSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE))

mean(AugSpeedSlut$AugSpeedSlut)

# difference in speed at and after augmented alerts 
AugSpeedAtandAfterAlert <- merge(AugSpeedStart, AugSpeedSlut)

AugSpeedAtandAfterAlert <- AugSpeedAtandAfterAlert %>%
  dplyr::mutate(AugSpeedDiff = AugSpeedSlut - AugSpeedStart) %>%
  group_by(FOD) %>%
  dplyr::mutate(TotalNumberOfAugAlert = max(1:n()))

AugSlowedDown <- AugSpeedAtandAfterAlert %>%
  filter(AugSpeedDiff < 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfAugAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedDown = mean(AugSpeedDiff),
                   sdSpeedDown = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedDown = max(NumberOfAugAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfAugAlertSpeedDown / TotalNumberOfAugAlert * 100) 

AugSlowedDown

AugSpeedUp <- AugSpeedAtandAfterAlert %>%
  dplyr::group_by(FOD) %>%
  dplyr::filter(AugSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfAugAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedUp = mean(AugSpeedDiff),
                   sdSpeedUp = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedUp = max(NumberOfAugAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfAugAlertSpeedUp/TotalNumberOfAugAlert*100)

AugSpeedUp


PhysSlowedDown
AugSlowedDown
PhysSpeedUp
AugSpeedUp

#----- How Much and How Often - Alert Cost: Phys vs Aug -----

PhysTest2 <- dfp %>%
  dplyr::group_by(testID, ParticipantID, FOD) %>%
  dplyr::mutate(LagTimeSinceStart = dplyr::lag(TimeSinceObjDetStart))

#Speed at Physical alert start
PhysSpeedStart <- PhysTest2 %>%
  dplyr::filter(TimeSincePhysDetStart > 0 & TimeSincePhysDetStart < 0.1 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(testID, ParticipantID, FOD, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE),
                   TimeSinceLastAlert = max(LagTimeSinceStart)) 

#Speed at Physical alert after 1.2s
PhysSpeedSlut <- dfp %>%
  filter(TimeSincePhysDetStart > 1.2 & TimeSincePhysDetStart < 1.3 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE))


# difference in speed at and after physical alerts 
PhysSpeedAtandAfterAlert <- merge(PhysSpeedStart, PhysSpeedSlut)

PhysSpeedAtandAfterAlert <- PhysSpeedAtandAfterAlert %>%
  dplyr::mutate(PhysSpeedDiff = PhysSpeedSlut - PhysSpeedStart) %>%
  group_by(FOD) %>%
  dplyr::mutate(TotalNumberOfPhysAlert = max(1:n()))

PhysSlowedDown <- PhysSpeedAtandAfterAlert %>%
  filter(PhysSpeedDiff < 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedDown = mean(PhysSpeedDiff),
                   sdSpeedDown = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedDown = max(NumberOfPhysAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfPhysAlertSpeedDown / TotalNumberOfPhysAlert * 100) 

PhysSlowedDown

PhysSpeedUp <- PhysSpeedAtandAfterAlert %>%
  dplyr::group_by(FOD) %>%
  dplyr::filter(PhysSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedUp = mean(PhysSpeedDiff),
                   medianSpeedUp  = median(PhysSpeedDiff),
                   sdSpeedUp = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedUp = max(NumberOfPhysAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfPhysAlertSpeedUp/TotalNumberOfPhysAlert*100)

PhysSpeedUp

PhysSlowedDown1 <- PhysSpeedAtandAfterAlert %>%
  dplyr::mutate(BigGap = ifelse(TimeSinceLastAlert > 2, "Big Gap", "Small Gap")) %>%
  filter(PhysSpeedDiff < 0) %>%
  group_by(FOD, BigGap) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedDown = mean(PhysSpeedDiff),
                   sdSpeedDown = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedDown = max(NumberOfPhysAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfPhysAlertSpeedDown / TotalNumberOfPhysAlert * 100) 

PhysSlowedDown1

PhysSpeedUp1 <- PhysSpeedAtandAfterAlert %>%
  dplyr::mutate(BigGap = ifelse(TimeSinceLastAlert > 2, "Big Gap", "Small Gap")) %>%
  dplyr::group_by(FOD, BigGap) %>%
  dplyr::filter(PhysSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfPhysAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(PhysSpeedStart = mean(PhysSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   PhysSpeedSlut = mean(PhysSpeedSlut),
                   PhysAvgSpeedUp = mean(PhysSpeedDiff),
                   medianSpeedUp  = median(PhysSpeedDiff),
                   sdSpeedUp = sd(PhysSpeedDiff),
                   TotalNumberOfPhysAlert = max(TotalNumberOfPhysAlert),
                   NumberOfPhysAlertSpeedUp = max(NumberOfPhysAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfPhysAlertSpeedUp/TotalNumberOfPhysAlert*100)

PhysSpeedUp1

#Speed at Augmented alert start
AugSpeedStart <- PhysTest2 %>%
  dplyr::filter(TimeSinceAugDetStart > 0 & TimeSinceAugDetStart < 0.1 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(testID, ParticipantID, FOD, Range, objDetInTestID) %>%
  dplyr::summarise(AugSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE),
                   TimeSinceLastAlert = max(LagTimeSinceStart))

#mean(AugSpeedStart$AugSpeedStart)

#Speed at augmented alert after 1.2s
AugSpeedSlut <- dfp %>%
  filter(TimeSinceAugDetStart > 1.2 & TimeSinceAugDetStart < 1.3 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(AugSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE))

#mean(AugSpeedSlut$AugSpeedSlut)

# difference in speed at and after augmented alerts 
AugSpeedAtandAfterAlert <- merge(AugSpeedStart, AugSpeedSlut)

AugSpeedAtandAfterAlert <- AugSpeedAtandAfterAlert %>%
  dplyr::mutate(AugSpeedDiff = AugSpeedSlut - AugSpeedStart) %>%
  group_by(FOD) %>%
  dplyr::mutate(TotalNumberOfAugAlert = max(1:n()))

AugSlowedDown <- AugSpeedAtandAfterAlert %>%
  filter(AugSpeedDiff < 0) %>%
  group_by(FOD) %>%
  dplyr::mutate(NumberOfAugAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedDown = mean(AugSpeedDiff),
                   sdSpeedDown = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedDown = max(NumberOfAugAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfAugAlertSpeedDown / TotalNumberOfAugAlert * 100) 

AugSlowedDown

AugSpeedUp <- AugSpeedAtandAfterAlert %>%
  dplyr::group_by(FOD) %>%
  dplyr::filter(AugSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfAugAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   MedianGap = median(TimeSinceLastAlert, na.rm=TRUE),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedUp = mean(AugSpeedDiff),
                   sdSpeedUp = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedUp = max(NumberOfAugAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfAugAlertSpeedUp/TotalNumberOfAugAlert*100)

AugSpeedUp

AugSlowedDown1 <- AugSpeedAtandAfterAlert %>%
  dplyr::mutate(BigGap = ifelse(TimeSinceLastAlert > 2, "Big Gap", "Small Gap")) %>%
  filter(AugSpeedDiff < 0) %>%
  group_by(FOD, BigGap) %>%
  dplyr::mutate(NumberOfAugAlertSpeedDown = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedDown = mean(AugSpeedDiff),
                   sdSpeedDown = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedDown = max(NumberOfAugAlertSpeedDown)) %>%
  dplyr::mutate(ProcentageSpeedDrop = NumberOfAugAlertSpeedDown / TotalNumberOfAugAlert * 100) 

AugSlowedDown1

AugSpeedUp1 <- AugSpeedAtandAfterAlert %>%
  dplyr::mutate(BigGap = ifelse(TimeSinceLastAlert > 2, "Big Gap", "Small Gap")) %>%
  dplyr::group_by(FOD, BigGap) %>%
  dplyr::filter(AugSpeedDiff > 0) %>%
  dplyr::mutate(NumberOfAugAlertSpeedUp = 1:n())  %>%
  dplyr::summarise(AugSpeedStart = mean(AugSpeedStart),
                   AugSpeedSlut = mean(AugSpeedSlut),
                   AvgSpeedUp = mean(AugSpeedDiff),
                   sdSpeedUp = sd(AugSpeedDiff),
                   TotalNumberOfAugAlert = max(TotalNumberOfAugAlert),
                   NumberOfAugAlertSpeedUp = max(NumberOfAugAlertSpeedUp)) %>%
  dplyr::mutate(ProcentageSpeedUp = NumberOfAugAlertSpeedUp/TotalNumberOfAugAlert*100)

AugSpeedUp1

#----- Plot Alert Cost: Phys vs Aug Split increase and decrease  (cleaned for paper) -----

#_________________________________________________________________________________________________________________
# Physical Alerts
#_________________________________________________________________________________________________________________

test <- dfp %>%
  dplyr::filter(TimeSincePhysDetStart > 0 & TimeSincePhysDetStart <= 0.1 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE))  %>%
  right_join(dfp) %>%
  arrange(rowNum)

test <- test %>%
  filter(TimeSincePhysDetStart > 0.1 & TimeSincePhysDetStart < 1.3 &!is.na(TimeSincePhysDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(PhysSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #AlertType = "Physical Alert"
                   ) %>%
  right_join(test) %>%
  arrange(rowNum)

test <- test %>%
  dplyr::mutate(PhysSpeedDiff = PhysSpeedSlut - PhysSpeedStart) %>%
  mutate(PhysSpeedChange = ifelse(PhysSpeedDiff < 0, "Slowed Down After Physical Alert",
                              ifelse(PhysSpeedDiff > 0, "Speeded Up After Physical Alert", NA)))

test <- test %>%
  dplyr::filter(TimeSinceAugDetStart > 0 & TimeSinceAugDetStart <= 0.1 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(AugSpeedStart = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #AlertType = "Augmented Alert"
                   )  %>%
  right_join(test) %>%
  arrange(rowNum)

test <- test %>%
  filter(TimeSinceAugDetStart > 0.1 & TimeSinceAugDetStart < 1.3 &!is.na(TimeSinceAugDetStart)) %>%
  dplyr::group_by(FOD, testID, objDetInTestID) %>%
  dplyr::summarise(AugSpeedSlut = mean(rollingSpeedMedian, na.rm=TRUE)#,
                   #AlertType = "Augmented Alert"
                   ) %>%
  right_join(test) %>%
  arrange(rowNum)

test <- test %>%
  dplyr::mutate(AugSpeedDiff = AugSpeedSlut - AugSpeedStart) %>%
  mutate(AugSpeedChange = ifelse(AugSpeedDiff < 0, "Slowed Down After Augmented Alert",
                                 ifelse(AugSpeedDiff > 0, "Speeded Up After Augmented Alert", NA)))

test %>%
  filter(TimeSinceObjDetStart < 5 & TimeSinceObjDetStart > 0 & !is.na(AugSpeedChange)& !is.na(PhysSpeedChange)) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_smooth(aes(colour = AugSpeedChange, linetype = PhysSpeedChange), se = F) +
  theme_bw() +
  ylab("Walking Speed (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")


#### --------------------------------------------------------------------


## New analysis

daggByScenario <- dfp %>%
  filter(PersonSpeed < 3) %>%
  select(ParticipantID, Scenario, FOD, Range, PersonSpeed, objDet, objColl, TimeSeconds) %>%
  pivot_longer(
    cols = PersonSpeed:TimeSeconds,
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(ParticipantID, Scenario, FOD, Range, measure) %>%
  dplyr::summarize(
    avg = mean(value),
    median = median(value),
    max = max(value),
    min = min(value),
    sum = sum(value, na.rm = TRUE)
  ) %>%
  mutate_each(funs(replace(., is.na(.), 0)), avg:sum) %>%
  pivot_longer(
    cols = avg:sum,
    names_to = "ScenarioAgg",
    values_to = "value"
  )

daggByScenPart <- daggByScenario %>%
  group_by(ParticipantID, FOD, Range, measure, ScenarioAgg) %>%
  dplyr::summarize(
    avg = mean(value),
    median = median(value)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = avg:median,
    names_to = "PersonAgg",
    values_to = "Value"
  )

PersonBaselines <- daggByScenPart %>%
  ungroup() %>%
  filter(Range == 1) %>%
  select(
    ParticipantID,
    measure,
    PersonAgg,
    ScenarioAgg,
    Value
  ) %>%
  rename(Baseline = Value)

daggByPerson <- daggByScenPart %>%
  merge(PersonBaselines) %>%
  mutate(diffFromBL = Value - Baseline)

# The shapiro test does not work
daggByScenPart %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  select(Value) %>%
  shapiro.test(Value)


# Test significance of Walking Speed
SpeedModel <- daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  do(data.frame(tidy(lm(diffFromBL ~ FOD, data = .))))

SpeedModel

SpeedModelByDet <-

  dfx <- daggByPerson %>%
  filter(PersonAgg == "avg") %>%
  filter((measure == "PersonSpeed" & ScenarioAgg == "avg") | (measure == "objDet" & ScenarioAgg == "sum") | (measure == "objColl" & ScenarioAgg == "sum")) %>%
  select(ParticipantID, Value, diffFromBL, Range, FOD, measure) %>%
  pivot_wider(names_from = measure, values_from = c(Value, diffFromBL), names_glue = "{measure}_{.value}") %>%
  do(data.frame(tidy(lm(PersonSpeed_Value ~ objDet_Value, data = .))))

summary(lm(PersonSpeed_Value ~ objDet_Value, data = dfx))
summary(lm(objColl_Value ~ objDet_Value, data = dfx))

summary(lm(PersonSpeed_Value ~ objColl_Value, data = dfx))
summary(lm(PersonSpeed_Value ~ objDet_Value * objColl_Value, data = dfx))

summary(lm(objDet_Value ~ Range, data = dfx))

summary(lm(objColl_Value ~ FOD, family = "poisson", data = dfx))

summary(lm(PersonSpeed_Value ~ objColl_Value, data = dfx))
summary(lm(PersonSpeed_Value ~ objDet_Value * objColl_Value, data = dfx))

summary(lm(PersonSpeed_diffFromBL ~ objDet_diffFromBL * objColl_diffFromBL, data = dfx))
summary(lm(PersonSpeed_diffFromBL ~ objDet_diffFromBL * objColl_diffFromBL, data = dfx))

SpeedModelByDet


# Test significance of Detections
ObjDetModel <- daggByPerson %>%
  filter(measure == "objDet", PersonAgg == "avg", ScenarioAgg == "sum") %>%
  do(data.frame(tidy(lm(diffFromBL ~ Range, family = "poisson", data = .))))
ObjDetModel

# Test significance of Collisions
ObjCollModel <- daggByPerson %>%
  filter(measure == "objColl", PersonAgg == "avg", ScenarioAgg == "sum") %>%
  do(data.frame(tidy(lm(diffFromBL ~ Range, family = "poisson", data = .))))
ObjCollModel


daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  ggplot(aes(x = Range, y = Value, group = ParticipantID, colour = factor(ParticipantID))) +
  geom_line() +
  facet_grid(rows = vars(FOD))

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  ggplot(aes(x = Range, y = diffFromBL, group = ParticipantID, colour = factor(ParticipantID))) +
  geom_line() +
  facet_grid(rows = vars(FOD))

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  select(Value) %>%
  ggplot(aes(x = Value)) +
  geom_density()

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  filter(FOD == "Corridor") %>%
  select(Value) %>%
  summarise(sd = sd(Value), mean = mean(Value), min = min(Value), max = max(Value))

options(scipen = 999)

dfxColl <- dfx %>%
  group_by(ParticipantID) %>%
  summarise(avgColl = mean(objColl_Value))

dfx %>%
  summarise(sd = sd(objColl_Value), mean = mean(objColl_Value), min = min(objColl_Value), max = max(objColl_Value))


sd(dfxColl$avgColl)

dfxDet <- dfx %>%
  group_by(ParticipantID) %>%
  summarise(avgDet = mean(objDet_Value))

dfx %>% summarise(sd = sd(objDet_Value), mean = mean(objDet_Value), min = min(objDet_Value), max = max(objDet_Value))

sd(dfxDet$avgDet)

dfxSpeed <- dfx %>%
  group_by(ParticipantID) %>%
  filter(FOD == "WholeRoom") %>%
  summarise(avgSpeed = mean(Person_Speed_Value))

dfx %>%
  filter(FOD == "WholeRoom") %>%
  summarise(sd = sd(Person_Speed_Value), mean = mean(Person_Speed_Value), min = min(Person_Speed_Value), max = max(Person_Speed_Value))

sd(dfxSpeed$avgSpeed)

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95) {
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95) {
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}



dfFOD <- dfx %>%
  group_by(ParticipantID, FOD) %>%
  dplyr::summarize(avgColl = mean(objColl_Value))
do(data.frame(tidy(lm(avgColl ~ FOD, data = .))))

dfFODbyPerson <- dfx %>%
  group_by(ParticipantID, FOD) %>%
  dplyr::summarize(avgColl = mean(objColl_Value), avgWS = mean(Person_Speed_Value)) %>%
  ungroup()

dfFODbyPerson %>%
  group_by(FOD) %>%
  dplyr::summarize(medColl = median(avgColl), sdColl = sd(avgColl), avgColl = mean(avgColl)) %>%
  ungroup()

dfFODbyPerson$FOD <- recode(dfFODbyPerson$FOD, Baseline = "white cane", Corridor = "body preview", WholeRoom = "open range")
dfFODbyPerson$FOD <- factor(dfFODbyPerson$FOD, levels = c("white cane", "open range", "body preview"))
ggline(dfFODbyPerson,
  x = "FOD", y = "avgColl",
  add = c("mean_se", "jitter"),
  order = c("white cane", "open range", "body preview"),
  ylab = "number of collisions", xlab = ""
)

dfFODForPlotColl <- summarySE(dfFODbyPerson, measurevar = "avgColl", groupvars = c("FOD"))
dfFODForPlotWS <- summarySE(dfFODbyPerson, measurevar = "avgWS", groupvars = c("FOD"))

ggplot(dfFODForPlotColl, aes(x = FOD, y = avgColl, fill = FOD, colour = FOD, shape = FOD)) +
  geom_point(data = dfFODbyPerson, fill = "grey", color = "grey", width = .1) +
  geom_line(data = dfFODbyPerson, aes(group = ParticipantID), color = "grey", width = .1) +
  geom_errorbar(aes(ymin = avgColl - se, ymax = avgColl + se), color = "black", width = .1) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(20, 25, 22)) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title = element_text(size = 22, face = "bold")) +
  ylab("number of collisions") +
  xlab("")
ggsave(here("..", "..", "S1-CollisionsByFOD.png"), width = 9, height = 6.3)


ggplot(dfFODForPlotWS, aes(x = FOD, y = avgWS, fill = FOD, colour = FOD, shape = FOD)) +
  geom_point(data = dfFODbyPerson, fill = "grey", color = "grey", width = .1) +
  geom_line(data = dfFODbyPerson, aes(group = ParticipantID), color = "grey", width = .1) +
  geom_errorbar(aes(ymin = avgWS - se, ymax = avgWS + se), color = "black", width = .1) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(20, 25, 22)) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title = element_text(size = 22, face = "bold")) +
  ylab("walking speed (m/s)") +
  xlab("")
ggsave(here("..", "..", "Study1WalkingSpeed.png"), width = 9, height = 6.3)

# "Dropbox" ,"Apps","Overleaf","CHI Blind Tunnel Vision Limiting the Field of Detection and Range of Electronic Mobility Aids","figures"
set_here(path = "~")

summary(lm(avgColl ~ FOD, dfFOD))

boxplot(objColl_Value ~ Range,
  col = c("white", "lightgray"), dfx
)
boxplot(avgWS ~ FOD,
  col = c("white", "lightgray"), dfFODbyPerson
)

summary(aov(avgWS ~ FOD, dfFODbyPerson))
summary(aov(avgColl ~ FOD, dfFODbyPerson))

# Plot total average Walking speed ----------------
daggAvgSpeed <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "Person_Speed",
    PersonAgg == "avg",
    ScenarioAgg == "avg"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgSpeed = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgSpeed, aes(
  x = Range,
  y = newAvgSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgSpeed, 2)),
    size = 7,
    alpha = 1,
    position = position_dodge(0.8),
    vjust = -0.9
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Open Range aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# Plot Diff in Walking speed ----------------
daggPersonSpeed <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "Person_Speed",
    PersonAgg == "avg",
    ScenarioAgg == "avg"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newDiffSpeed = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggPersonSpeed, aes(
  x = Range,
  y = newDiffSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newDiffSpeed, 2)),
    size = 7,
    alpha = 1,
    position = position_dodge(0.8),
    vjust = -0.9
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Open Range aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# Plot total average collisions ----------------
daggAvgColl <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objColl",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgColl = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgColl, aes(
  x = Range,
  y = newAvgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of collisions per Range and Condition") +
  ylab("Number of collisions") +
  scale_y_continuous() +
  theme_bw()

# Plot difference in average collisions ----------------

daggDiffColl <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objColl",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newAvgColl = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggDiffColl, aes(
  x = Range,
  y = newAvgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of collisions per Range and Condition") +
  ylab("Number of collisions") +
  scale_y_continuous() +
  theme_bw()

# Plot total average detections ----------------
daggAvgDet <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objDet",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgDet = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgDet, aes(
  x = Range,
  y = newAvgDet,
  group = FOD,
  color = FOD,
  shape = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1,
    size = 5
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1,
    size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  # geom_text(aes(label = round(newAvgDet, 2)),
  #  size = 6,
  #  alpha = 1,
  #  position = position_dodge(0.6),
  #  vjust = -0.5
  # ) +
  # scale_fill_hue(
  #  name = "Condition",
  #  labels = c(
  #    "White Cane",
  #    "Body-preview aEMA",
  #    "Normal aEMA"
  #  )
  # ) +
  ylab("Averge Number of Alerts") +
  xlab("Preview Range in Meters") +
  scale_y_continuous() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

# Plot difference in average collisions ----------------

daggDiffDet <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objDet",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newAvgDet = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggDiffDet, aes(
  x = Range,
  y = newAvgDet,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgDet, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Detections per Range and Condition") +
  ylab("Number of Detections") +
  scale_y_continuous() +
  theme_bw()


# Old code -------------------------------

daggByScen <- dfp %>%
  filter(Person_Speed < 3) %>%
  group_by(testID, day, Scenario, FOD, Range) %>%
  dplyr::summarize(
    avgSpeed = mean(Person_Speed),
    medianSpeed = median(Person_Speed),
    maxSpeed = max(Person_Speed),
    minSpeed = min(Person_Speed),
    objectDetected = sum(objDet, na.rm = TRUE),
    objectCollisions = sum(objColl, na.rm = TRUE),
    Time = max(Time_in_S)
  ) %>%
  arrange(testID)

# add Coloum with sum of total time spent
daggByScenPart$totalTimeTraining <- round(cumsum(daggByScenPart$Time))

# add Coloum with total time spent for a given FOD with a given Range
daggByScenPart <- daggByScenPart %>%
  group_by(FOD, day, Range) %>%
  mutate(timeFDRtrain = round(cumsum(Time)))

# add Coloum with total time spent for a given FOD
daggByScenPart <- daggByScenPart %>%
  group_by(FOD, day) %>%
  mutate(timeFDtrain = round(cumsum(Time)))

# add Coloum with total time spent for a given Day
daggByScenPart <- daggByScenPart %>%
  group_by(day) %>%
  mutate(timeDtrain = round(cumsum(Time)), totalTimeTrainingHrs = totalTimeTraining / 3600)



# Below, a summary of our data. In total, 420 tests were completed over three days (140 per day), using three different Field Of Detections (FOD - Baseline, WholeRoom and Corridor). The WholeRoom and Corridor differ between three ranges (two, three and four meters), while the Baseline represents the original white cane (one meter range). Scenarios describe the obstacle courses the system was tested on (20 different scenarios). In addition, each test logged the walking speed of the participant, the amount of objects detected by the cane/EMA, the amount of collisions by the user and the completion time of the individual obstacle courses.

PartSpeed <- dfpSumTestID %>%
  group_by(ParticipantID) %>%
  dplyr::summarize(
    avgSpeed = mean(avgSpeed),
    maxSpeed = max(avgSpeed),
    minSpeed = min(avgSpeed)
  )

mean(dfpSumTestID$avgSpeed)
sd(dfpSumTestID$avgSpeed)

mean(dfp$ObjDetDuration, na.rm = TRUE)

summary(dfpSumTestID)

PartVibDur <- dfp %>%
  group_by(ParticipantID) %>%
  dplyr::summarize(
    avgVibDur = mean(ObjDetDuration, na.rm = TRUE),
    maxVibDur = max(ObjDetDuration, na.rm = TRUE),
    minVibDur = min(ObjDetDuration, na.rm = TRUE)
  )


PartAlert <- dfpSumTestID %>%
  group_by(FOD) %>%
  dplyr::summarize(
    avgAlert = mean(objectDetected),
    maxAlert = max(objectDetected),
    minAlert = min(objectDetected)
  )

# Below, a summary of our data.
summary(dfpSumTestID)

# Training time on walking speed
summary(lm(avgSpeed ~ RunNumber, data = dfpSumTestID))

# Training time and range on walking speed (all data)
summary(lm(avgSpeed ~ Range + RunNumber, data = dfpSumTestID))

# split data fram based on FOD
WCDat <- dfpSumTestID[dfpSumTestID$FOD == "White Cane", ]
wrDat <- dfpSumTestID[dfpSumTestID$FOD == "Conical View AWC", ]
corrDat <- dfpSumTestID[dfpSumTestID$FOD == "Tunnel View AWC", ]



TunnelDat <- dfpSumTestID[dfpSumTestID$FOD != "Conical View AWC", ]
ConicalDat <- dfpSumTestID[dfpSumTestID$FOD != "Tunnel View AWC", ]

# Training time and range on alerts (whole room data)
summary(lm(avgSpeed ~ Range + RunNumber, data = ConicalDat))

# Training time and range on alerts (corridor data)
summary(lm(avgSpeed ~ Range + RunNumber, data = TunnelDat))

# Training time and range on alerts (corridor data)
summary(lm(avgSpeed ~ Range + RunNumber, family = "poisson", data = TunnelDat))

# Training time and range on alerts (whole room data)
summary(glm(objectDetected ~ Range + RunNumber, family = "poisson", data = ConicalDat))

# Training time and range on alerts (corridor data)
summary(lm(objectDetected ~ Range + RunNumber, family = "poisson", data = TunnelDat))

# Training time and range on walking speed (whole room data)
summary(lm(avgSpeed ~ Range + RunNumber, data = wrDat))

# Training time and range on walking speed (corridor data)
summary(lm(avgSpeed ~ Range + RunNumber, data = corrDat))

# Training time and range on alerts (whole room data)
summary(lm(objectDetected ~ Range + RunNumber, data = wrDat))

# Training time and range on alerts (corridor data)
summary(lm(objectDetected ~ Range + RunNumber, data = corrDat))

# Training time and FOD on walking speed (all data)
summary(lm(avgSpeed ~ FOD + RunNumber, data = dfpSumTestID))

# Training time and FOD on walking speed (all data)
summary(lm(objectDetected ~ FOD + RunNumber, data = dfpSumTestID))

# Training time and Collisions on walking speed (all data)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = dfpSumTestID))

# Training time and Collisions on walking speed (baseline)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = WCDat))

# Training time and Collisions on walking speed (corridor)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = corrDat))

# Training time and Collisions on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = wrDat))

# Training time and Detections on walking speed (all data)
summary(lm(avgSpeed ~ objectDetected * objectCollisions + RunNumber, data = dfpSumTestID))

# Training time and Detections on walking speed (Baseline)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = WCDat))

# Training time and Detections on walking speed (Corridor)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = corrDat))

# Training time and Detections on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = wrDat))



summary(aov(avgSpeed ~ Range * FOD + Error(ParticipantID), data = dfpSumTestID))

## Walking speed =================================

# In this section we analysis how walking speed is effected by training time, range, FOD, collisions and detections.

### Overview #############################

# To get an overview of the walking speed we first made a histogram with a density curve to see how our data is distributed

# Histogram and curve of avgSpeed
ggplot(dfpSumTestID, aes(x = avgSpeed)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(avgSpeed)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

# As we can see the data is close to by not quite normally distributed, a Shapiro Wilks test confirms this as the p-values show a significant difference and, thereby rejects the nullhypothesis of the data following a normal distributed.

shapiro.test(dfpSumTestID$objectCollisions)

# A qq-plots also shows that the date is close to normal distributed with only a few outliers that was a lot faster than the rest.

qqPlot(dfpSumTestID$avgSpeed)

# To get a better overview of the outliers we made a heatmap of the 420 test to see if we could locate the outliers.

# Data Frame for heatmap table
daggHeat <- daggByScenPart %>%
  group_by(Scenario, FOD, Range, day)

# Heatmap table over avgSpeed
ggplot(daggHeat, aes(
  x = Range,
  y = Scenario
)) +
  geom_tile(aes(fill = avgSpeed)) +
  geom_text(aes(label = round(avgSpeed, 2))) +
  scale_fill_gradient2(
    low = muted("red"),
    mid = "yellow",
    high = muted("green"),
    midpoint = 0.63
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over avgSpeed per Scenario") +
  theme(legend.title = element_text(
    face = "bold",
    size = 14
  )) +
  scale_y_continuous(trans = "reverse") +
  # labs(fill = "avgSpeed") +
  facet_grid(
    cols = vars(FOD),
    row = vars(day)
  )

# As we can see scenario one seems to be a lot faster that the other scenarios. To illustrate this further we plot the walking speed for each scenario

# AvgSpeed per senario
ggplot(daggByScenPart, aes(
  x = Scenario,
  y = avgSpeed,
  color = FOD,
  group = c(Scenario)
)) +
  geom_jitter(width = .2) +
  geom_boxplot(aes(alpha = .1)) +
  facet_grid(rows = vars(FOD))

# Here we see that the first senario indeed is faster that the others but only for two of the conditions: Baseline and corridor.

# If we try to remove scenario one and then test for normality we see that it is closer, but still not quite there. However, it is so close that for the rest of the analysis we will assume Guassian distribution, even when including scenario one.

daggNoScen1 <- daggByScenPart %>%
  filter(Scenario != 1)

shapiro.test(daggNoScen1$avgSpeed)

qqPlot(daggNoScen1$avgSpeed)

### Training Time #############################

# We except training time to have a big influences on performance of the individual as when they get more experience they will start to walk faster.

# If we plot the total training time on the x-axis and then the walking speed on the y-axis for each condition what we see is that the user walks faster the more experiences he gets.

# Setting up a dataset for avg. speed for each day.
daggByDFR <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  dplyr::summarize(
    totalTimeTraining = max(totalTimeTraining),
    newAvgSpeed = mean(avgSpeed),
    smean = mean(avgSpeed, na.rm = TRUE),
    ssd = sd(avgSpeed, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

# Plot over the avgSpeed over each condition and the three days.
ggplot(daggByScenPart, aes(
  x = totalTimeTraining,
  y = avgSpeed,
  color = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = "red"),
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()

# Graph over the avg.Speed over the three day for each range
ggplot(daggByDFR, aes(
  x = totalTimeTraining,
  y = newAvgSpeed,
  color = factor(Range),
  shape = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  )) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()

# Plot over avg.Speed for each range and each FOD
ggplot(daggByScenPart, aes(
  x = totalTimeTraining,
  y = avgSpeed,
  color = factor(Range)
)) +
  geom_point(aes(alpha = .1)) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(a = 1, b = 1)),
    se = FALSE
  ) +
  # stat_regline_equation(aes(x=totalTimeTraining, y=avgSpeed))+
  facet_grid(cols = vars(FOD)) +
  theme_bw()

# Graph over avg.Speed for each range and each FOD
ggplot(daggByDFR, aes(
  x = totalTimeTraining,
  y = newAvgSpeed,
  color = factor(Range),
  shape = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  )) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# In fact, the training time is a significant predictor of walking speed.

summary(lm(avgSpeed ~ totalTimeTrainingHrs, data = daggByScenPart))

### Range #############################

# As we can see on training time there seems to be a clear different between the different ranges and how they effect walking speed.
#
# Based on the plots of there seems to be a clear difference between corridor and wholeroom.

daggSpeed <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  dplyr::summarize(
    newAvgSpeed = mean(avgSpeed),
    smean = mean(avgSpeed, na.rm = TRUE),
    ssd = sd(avgSpeed, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(daggByScenPart, aes(
  x = Range,
  y = avgSpeed,
  group = FOD,
  color = factor(FOD)
)) +
  geom_jitter(position = position_dodge(0.1)) +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = factor(FOD)),
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()


ggplot(data = daggSpeed, aes(
  x = Range,
  y = newAvgSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgSpeed, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# However, when we try to predict the users walking speed based on range on all the data we do not find the range to be a significat predictor.

summary(lm(avgSpeed ~ Range, data = daggByScenPart))

# Thus, we split the data up looking at the different conditions individual. What we find is that range is a significant predictor for wholeroom, but not for corridor.

WCDat <- daggByScenPart[daggByScenPart$FOD == "Baseline", ]
wrDat <- daggByScenPart[daggByScenPart$FOD == "WholeRoom", ]
corrDat <- daggByScenPart[daggByScenPart$FOD == "Corridor", ]

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data = wrDat))

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data = corrDat))

### FOD effect on walking speed #############################

# Based on the figure in the previous section it seems that the FOD influences the walking speed of the user. What we find is that wholeroom does negatively predict walking speed, while corridor only show a positively tendency to effect walking speed.


### Collisions effect on walking speed #############################

# Logically colliding with an object should slow down the person walking. Based on the two plots it looks like that is the case as the more collisions the slower the person walk.

# Number of Collisions effect on avgSpeed
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, color = factor(Range))) +
  geom_jitter(aes(alpha = .1)) +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = "red"), method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw()

# Number of Collisions effect on avgSpeed split per FOD
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, color = factor(Range))) +
  geom_jitter(aes(alpha = .1)) +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# To test this we analyzed how collisions effected the whole data set and what we found was that collisions was a significant negative predictor of walking speed

summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = daggByScenPart))


# To make sure it was the same case for the individual conditions we checked of each of them and found that in all cases the number of collisions was a significant predictor of walking speed.

summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = wrDat))

### Detections effect on Walking speed #############################

#  we would expect based on collisions so does more of detections lower the walking speed of the user.

# Number of detections effect on avgSpeed
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectDetected, color = factor(Range))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw()

# Number of detections effect on avgSpeed split per FOD
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectDetected, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# To test if this was a significant negative predictor of walking speed of the whole data set we found that it was.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = daggByScenPart))

# The same was also the case for the individual conditions where detections was a significant predictor of walking speed.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = wrDat))

# Now the question is how does the number of detections slow down a persons walking speed to find our we started out by making a plot where we show what happens the first 2 second after a detection.


### Detections and collisions effect on Walking speed #############################

# Number of detections and collisions based on avgSpeed split by FOD
ggplot(daggByScenPart, aes(y = objectDetected, x = objectCollisions, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))



ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, colour = factor(FOD))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0, color = "blue")+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  geom_point(aes(x = objectDetected, colour = factor(FOD))) +
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), colour = Range, method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  facet_grid(cols = vars(Range))


# Number of detections and collisions based on avgSpeed split by Range
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, colour = factor(Range))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0, color = "blue")+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  geom_point(aes(x = objectDetected, colour = factor(Range))) +
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  facet_grid(cols = vars(FOD))

## Collisions =================================

# Some text


ggplot(daggHeat, aes(x = Range, y = Scenario)) +
  geom_tile(aes(fill = objectCollisions)) +
  geom_text(aes(fill = daggHeat$objectCollisions, label = round(daggHeat$objectCollisions, 2))) +
  scale_fill_gradient2(
    low = muted("green"),
    mid = "yellow",
    high = muted("red"),
    midpoint = 4
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over Collisions per Scenario") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(trans = "reverse") +
  labs(fill = "Collisions") +
  facet_grid(cols = vars(FOD), row = vars(day))

daggColl <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  summarise(
    avgColl = mean(objectCollisions),
    smean = mean(objectCollisions, na.rm = TRUE),
    ssd = sd(objectCollisions, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggColl, aes(
  x = Range,
  y = avgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(avgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Objects Collisions per Range and Condition") +
  ylab("Mean Number of Collisions") +
  scale_y_continuous() +
  theme_bw()


## Detections =================================

# Some text...

# Heatmap table over Detections
ggplot(daggHeat, aes(x = Range, y = Scenario)) +
  geom_tile(aes(fill = objectDetected)) +
  geom_text(aes(fill = daggHeat$objectDetected, label = round(daggHeat$objectDetected, 2))) +
  scale_fill_gradient2(
    low = muted("green"),
    mid = "yellow",
    high = muted("red"),
    midpoint = 30
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over Detections per Scenario") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(trans = "reverse") +
  labs(fill = "Detections") +
  facet_grid(cols = vars(FOD), row = vars(day))


# Make functions
daggDetect <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  summarise(
    avgObjDet = mean(objectDetected),
    smean = mean(objectDetected, na.rm = TRUE),
    ssd = sd(objectDetected, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )


ggplot(data = daggDetect, aes(
  x = Range,
  y = avgObjDet,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(avgObjDet, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Objects Detected per Range and Condition") +
  ylab("Mean Obstacles Detected") +
  scale_y_continuous() +
  theme_bw()



objectsTime <- daggByScenPart$objectDetected / daggByScenPart$Time

ggplot(daggByScenPart, aes(y = avgSpeed, x = objectsTime, color = factor(Range), alpha = 0.9)) +
  geom_point() +
  geom_jitter() +
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  # geom_smooth(size=0, color = "blue")+
  # stat_smooth(color="red", method="glm", family="poisson", se=TRUE)+
  # geom_point(aes(x = objectDetected,  color=I("#56B4E9")))+
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  # stat_smooth(aes(x = objectDetected), color="red", method="glm", family="poisson", se=TRUE)+
  # scale_color_manual(labels=c("White Cane", "Body-preview aEMA", "Normal aEMA"))+
  # facet_grid(cols=vars(FOD))+
  # scale_colour_manual(values=c("#E69F00","#56B4E9"), labels = c("Detections", "Collisions"))+
  # labs(colour = "Collisions")+
  # ggtitle("The effect of Detections on Walking Speed")+
  labs(
    y = "Mean Walking Speed in Meters per Second",
    x = "Number of obstacle Detections"
  ) +
  theme(
    axis.title.x = element_text(
      vjust = 10,
      size = 44
    ),
    axis.title.y = element_text(size = 15)
  ) +
  scale_color_manual(
    name = "Range",
    values = c("#E69F00", "#56B4E9", "#143D59", "#6DD47E"),
    labels = c("1 meter", "2 meter", "3 meter", "4 meter")
  ) +
  # ylab("Mean Walking Speed in Meters per Second") +
  # xlab("Number of obstacle Detections") +
  facet_grid(cols = vars(FOD)) +
  theme_bw()


# analysis on how detections affect speed
onsets <- dfp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
    VibStartTime = RunningTime,
    SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- dfp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
    VibStopTime = RunningTime,
    SpeedAtVibStop = rollingSpeedMedian
  )

dfv <- merge(dfp, onsets)
dfv <- merge(dfv, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfv$TimeSinceObjDetStart <- dfv$RunningTime - dfv$VibStartTime
dfv$TimeSinceVibStop <- dfv$RunningTime - dfv$VibStopTime


dfv$SpeedChangeFromDetStart <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStart
dfv$SpeedDiffFromStop <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStop


dfp$ObjDetChangeHlp <- lag(dfp$Object_detected)


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_line(alpha = .04) +
  facet_grid(rows = vars(FOD)) +
  theme_bw()


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  )) +
  geom_point(
    alpha = .1,
    size = .5
  ) +
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = SpeedChangeFromDetStart
  )) +
  geom_point(
    alpha = .1,
    size = .5
  ) +
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = SpeedChangeFromDetStart,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(cols = vars(Range))


dfv %>%
  filter(TimeSinceObjDetStart < 2 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(cols = vars(FOD))


dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = factor(day)
  )) +
  geom_smooth() +
  theme_bw()



dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD))

dfv %>%
  filter(TimeSinceVibStop < 2.5 & TimeSinceVibStop > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStop,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD))



dfvbaseDat <- dfv[dfv$FOD == "Baseline", ]
dfvwrDat <- dfv[dfv$FOD == "WholeRoom", ]
dfvcorrDat <- dfv[dfv$FOD == "Corridor", ]

vibDuration <- median(dfv$VibStopTime - dfv$VibStartTime)
vibDurationBase <- median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime)
vibDurationWR <- median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime)
vibDurationCorr <- median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime)


dm <- data.frame(
  "FOD" = c("Baseline", "WholeRoom", "Corridor"),
  "medianDuration" = c(
    median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime),
    median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime),
    median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime)
  )
)

dfv %>%
  filter(TimeSinceObjDetStart < 2.5 & TimeSinceObjDetStart > 0) %>%
  ggplot(
    data = dfv,
    aes(
      x = TimeSinceObjDetStart,
      y = rollingSpeedMedian,
      colour = FOD
    )
  ) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD)) +
  geom_rect(
    data = dm,
    mapping = aes(
      xmin = medianDuration,
      xmax = medianDuration,
      ymin = 0,
      ymax = 1,
      fill = FOD
    ),
    color = "black",
    alpha = 0.5
  )


geom_point(
  data = dm,
  aes(x = median)
)


geom_point(aes(
  x = vibDurationWR,
  colour = "WholeRoom"
)) +
  geom_point(aes(
    x = vibDurationCorr,
    colour = "Corridor"
  ))


###### Analysis of differences consistency in terms  of collisions ######
col4ICC <- dfx %>%
  select(FOD, Range, ParticipantID, colls = objColl_Value) %>%
  pivot_wider(names_from = ParticipantID, values_from = colls) %>%
  ungroup() %>%
  select(-FOD, -Range)
psych::ICC(col4ICC)

Yes

# Make trail ID
# PIDTrialID <- dfp %>%
#  select(ParticipantID, testID) %>%
#  group_by(ParticipantID, testID) %>%
#  summarise(number = 1) %>%
#  mutate(runningNum = cumsum(number)) %>%
#  select(-number)
# dfp <- merge(dfp, PIDTrialID)

plot(density(dfp$TimeSinceObjDetStart))
plot(density(dfp$TimeSinceVibStop))

# analysis on how detections affect speed
onsets <- dfp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
    VibStartTime = RunningTime,
    SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- dfp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
    VibStopTime = RunningTime,
    SpeedAtVibStop = rollingSpeedMedian
  )

dfp <- merge(dfp, onsets)
dfp <- merge(dfp, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfp$TimeSinceObjDetStart <- dfp$RunningTime - dfp$VibStartTime
dfp$TimeSinceVibStop <- dfp$RunningTime - dfp$VibStopTime


dfp$SpeedChangeFromDetStart <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStart
dfp$SpeedDiffFromStop <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStop
