# Analysis 1 Blind Participants Study ---------------------------------

#----- Libraries -----
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(Rmisc)
library("car")
library(lme4)
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

#----- GetData -----

# Load the data which was imported and cleaned by the "ImportBlindFoldedParticipants" script. 
load("data_3StudyBlindParticipant_All.rda")
load("data_Participants_bpSumTestID.rda")

#----- Data Overview: Means -----
df_bpSumTestID124 <- df_bpSumTestID %>%
  filter(Range != 3)

meanDF <- df_bpSumTestID124 %>%
  dplyr::group_by(FOD) %>%
  dplyr::summarise(avgSpeed = mean(avgSpeed),
                   avgMedianSpeed = mean(medianSpeed),
                   avgColl = mean(objectCollisions),
                   avgDet = mean(objectDetected),
                   avgPhysDet = mean(physObjectDetected),
                   avgAugDet = mean(augObjectDetected))

meanDF

summary(lm(avgSpeed ~ objectDetected + testID, data = df_bpSumTestID))
summary(lm(avgSpeed ~ physObjectDetected + testID, data = df_bpSumTestID))
summary(lm(avgSpeed ~ augObjectDetected + testID, data = df_bpSumTestID))
summary(lm(avgSpeed ~ objectCollisions + testID, data = df_bpSumTestID))
summary(lm(avgSpeed ~ physObjectDetected + augObjectDetected + objectCollisions + FOD + Range + testID, data = df_bpSumTestID))


summary(lm(avgSpeed ~ FOD + testID, data = df_bpSumTestID))

summary(lm(avgSpeed ~ Range + testID, data = df_bpSumTestID))

#----- Data Overview: Detection Distance -----

# Density plot of the alert distance for each FOD.
df_bp %>%
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

# Density plot of the alert duration for each FOD.
df_bp %>%
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
ggplot(df_bpSumTestID, aes(x=factor(Scenario), y=avgSpeed)) + 
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

#----- Data Overview: Variance between Run Number -----
ggplot(df_bpSumTestID, aes(x=RunNumber, y=avgSpeed)) +
  geom_point() +
  geom_smooth()+
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
mean(df_bpSumTestID$objectDetected)
sd(df_bpSumTestID$objectDetected)

# Calculate the mean of number of physical Detections per run
mean(df_bpSumTestID$physObjectDetected)
sd(df_bpSumTestID$physObjectDetected)

# Calculate the mean of number of augmented Detections per run
mean(df_bpSumTestID$augObjectDetected)
sd(df_bpSumTestID$augObjectDetected)

#----- Mean: Walking speed -----

# Total Mean Walking Speed
mean(df_bpSumTestID$avgSpeed)
sd(df_bpSumTestID$avgSpeed)

#----- Mean: Collisions -----

mean(df_bpSumTestID$objectCollisions)
sd(df_bpSumTestID$objectCollisions)

#----- Testing for normality -----

### Check if logging the data will have any effect on the data

# How is the data if we just add a linear function
summary(lm(avgSpeed ~ objectDetected + FOD, data = df_bpSumTestID)) # R-squared:  0.20

# How is the data if we make a log transformation on the walking speed. Making it Exponential function
summary(lm(log(avgSpeed) ~ objectDetected, data = df_bpSumTestID)) # R-squared:  0.27

# How is the data if we make a log transformation on the walking speed and alerts. Making it a power function 
summary(lm(log(avgSpeed) ~ log(objectDetected+1), data = df_bpSumTestID)) #R-squared:  0.25

### Summary : Based on the above comparison logging the walking speed seems to be the best fit

# /////////////////////////////////////////////////////////////////////////////////////////////////

# Finding the residuals - Linear Exponential function
lmExponential <- lm(log(avgSpeed) ~ objectDetected, data = df_bpSumTestID)

lmExponentialResiduals <- resid(lmExponential)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(df_bpSumTestID$objectDetected+1), lmExponentialResiduals) + abline(0,0) # looks good

# Test the Residuals on a qqplot to look for normality
qqPlot(lmExponentialResiduals) # looks good - a bit towards the end

# Test for normality
shapiro.test(lmExponentialResiduals) # close to not be rejected so not too good not too bad

### Summery : A logged linear function is close to normally distributed

# /////////////////////////////////////////////////////////////////////////////////////////////////

#----- Mixed Linear Models -----

# Mixed Exponential function

# Finding the residuals - Mixed Exponential function
WalkingSpeed.Exponential <- lmer(log(avgSpeed) ~ objectDetected + 
                                   (1 + objectDetected | ParticipantID) +
                                   (1 + objectDetected | RunNumber),
                                 data = df_bpSumTestID, REML = FALSE
)

WalkingSpeed.Exponential.Residuals <- resid(WalkingSpeed.Exponential)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(df_bpSumTestID$objectDetected+1), WalkingSpeed.Exponential.Residuals) + abline(0,0) # looks good

# Test the Residuals on a qqplot to look for normality
qqPlot(WalkingSpeed.Exponential.Residuals) # looks good - a bit towards the end

# Test for normality
shapiro.test(WalkingSpeed.Exponential.Residuals) # Very good - data should be normal distributed

### Summery : A logged mixed linear function should be normally distributed

# /////////////////////////////////////////////////////////////////////////////////////////////////

### Linear power function

# Finding the residuals - linear power function
lmPower <- lm(log(avgSpeed) ~ log(objectDetected+1), data = df_bpSumTestID)

lmPowerResiduals <- resid(lmPower)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(df_bpSumTestID$objectDetected+1), lmPowerResiduals) + abline(0,0) # Could be a lot better the bottom of the line is ot heavy

# Test the Residuals on a qqplot to look for normality
qqPlot(lmPowerResiduals) # Does not look too good the end is off

# Test for normality
shapiro.test(lmPowerResiduals) # not good - data does not look normally distributed

### Summery : A Linear power function is not a good transformation of the data.

# /////////////////////////////////////////////////////////////////////////////////////////////////

# mixed linear Power function

# Finding the residuals mixed Power function
WalkingSpeed.Power <- lmer(log(avgSpeed) ~ log(objectDetected+1) + 
                             (1+ log(1 + objectDetected) | ParticipantID) +
                             (1 + log(1 + objectDetected) | RunNumber) +
                             (1 + log(1 + objectDetected) | Scenario),
                           data = df_bpSumTestID, REML = FALSE
)

WalkingSpeed.Power.Residuals <- resid(WalkingSpeed.Power)

# Plot the residuals - should (ish) have the same amount above and below the line without any clear pattern
plot(log(df_bpSumTestID$objectDetected+1), WalkingSpeed.Power.Residuals) + abline(0,0) # looks good, a bit heavy

# Test the Residuals on a qqplot to look for normality
qqPlot(WalkingSpeed.Power.Residuals) # looks good - a bit towards each end 

# Test for normality
shapiro.test(WalkingSpeed.Power.Residuals) # Very good - data should be normal distributed

### After testing four different transformation the Mixed Linear Function (Exponential only one log) seemst to explain the data the most

#----- LMEM: Walking Speed vs alerts -----

# Making the NULL model, only including participants
Log.WS.1.PCPs <- lmer(log(avgSpeed) ~ 1 +
                        (1 | ParticipantID),
                      data = df_bpSumTestID, REML = FALSE )

summary(Log.WS.1.PCPs)
confint(Log.WS.1.PCPs)
plot(Log.WS.1.PCPs, type = c("p", "smooth"))
qqmath(Log.WS.1.PCPs, id = 0.05)

# adding scenario
Log.WS.Alerts.PCPs <- lmer(log(avgSpeed) ~ objectDetected +
                             (1 | ParticipantID),
                           data = df_bpSumTestID, REML = FALSE
)

summary(Log.WS.Alerts.PCPs)
confint(Log.WS.Alerts.PCPs)
plot(Log.WS.Alerts.PCPs, type = c("p", "smooth"))


# significant - yes
anova(Log.WS.1.PCPs, Log.WS.Alerts.PCPs)

# Add number of alerts 
Log.WS.Alerts.PCPs.Run <- lmer(log(avgSpeed) ~ objectDetected + 
                                 (1 | ParticipantID) +
                                 (1 | RunNumber),
                               data = df_bpSumTestID, REML = FALSE
)

summary(Log.WS.Alerts.PCPs.Run)

# significant - yes
anova(Log.WS.Alerts.PCPs, Log.WS.Alerts.PCPs.Run)

# add slope to participants
Log.WS.Alerts.PCPs.Run.Scen <- lmer(log(avgSpeed) ~ objectDetected + 
                                      (1 | ParticipantID) +
                                      (1 | RunNumber) +
                                      (1 | Scenario),
                                    data = df_bpSumTestID, REML = FALSE
)

summary(Log.WS.Alerts.PCPs.Run.Scen)

# significant - yes
anova(Log.WS.Alerts.PCPs.Run, Log.WS.Alerts.PCPs.Run.Scen)

# add slope to scenario
Log.WS.Alerts.PCPs.Slope.Scen.Run <- lmer(log(avgSpeed) ~ objectDetected + 
                                            (1 + objectDetected | ParticipantID) +
                                            (1 | Scenario) +
                                            (1 | RunNumber),
                                          data = df_bpSumTestID, REML = FALSE
)
summary(Log.WS.Alerts.PCPs.Slope.Scen.Run)

# significant - Yes
anova(Log.WS.Alerts.PCPs.Scen.Run, Log.WS.Alerts.PCPs.Slope.Scen.Run)

# add run number
Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope <- lmer(log(avgSpeed) ~ objectDetected + 
                                                  (1 + objectDetected | ParticipantID) +
                                                  (1 | Scenario) +
                                                  (1 + objectDetected | RunNumber),
                                                data = df_bpSumTestID, REML = FALSE
)
summary(Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope)

# significant - Yes 
anova(Log.WS.Alerts.PCPs.Slope.Scen.Run, Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope)

# add run number
Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope <- lmer(log(avgSpeed) ~ objectDetected + 
                                                        (1 + objectDetected | ParticipantID) +
                                                        (1 + objectDetected | Scenario) +
                                                        (1 + objectDetected | RunNumber),
                                                      data = df_bpSumTestID, REML = FALSE
)
summary(Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope)

# significant - No 
anova(Log.WS.Alerts.PCPs.Slope.Scen.Run.Slope, Log.WS.Alerts.PCPs.Slope.Scen.Slope.Run.Slope)

#----- LMEM: Walking Speed vs Phy-Alerts -----

# Making the NULL model, only including participants
WS.Power.Part <- lmer(log(avgSpeed) ~ 1 +
                        (1 | ParticipantID),
                      data = df_bpSumTestID, REML = FALSE )

# adding scenario
WS.Power.Part.Scen <- lmer(log(avgSpeed) ~ 1 +
                             (1 | ParticipantID) +
                             (1 | Scenario),
                           data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part, WS.Power.Part.Scen)

# Add number of alerts 
WS.Power.Part.Scen.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                      (1 | ParticipantID) +
                                      (1 | Scenario),
                                    data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen, WS.Power.Part.Scen.phyAlert)

# add slope to participants
WS.Power.Part.Slope.Scen.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                            (1 + log(1 + physObjectDetected) | ParticipantID) +
                                            (1 | Scenario),
                                          data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen.phyAlert, WS.Power.Part.Slope.Scen.phyAlert)

# add slope to scenario
WS.Power.Part.Slope.Scen.Slope.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                  (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                  (1 + log(1 + physObjectDetected) | Scenario),
                                                data = df_bpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.phyAlert, WS.Power.Part.Slope.Scen.Slope.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                      (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                      (1 + log(1 + physObjectDetected) | Scenario) +
                                                      (1 | RunNumber),
                                                    data = df_bpSumTestID, REML = FALSE
)

# significant - Yes 
anova(WS.Power.Part.Slope.Scen.Slope.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                            (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                            (1 + log(1 + physObjectDetected) | Scenario) +
                                                            (1 + log(1 + physObjectDetected) | RunNumber),
                                                          data = df_bpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert.augAlert <- lmer(log(avgSpeed) ~ 1 + log(physObjectDetected + 1) + 
                                                                     (1 + log(1 + physObjectDetected) | ParticipantID) +
                                                                     (1 | Scenario) +
                                                                     (1 | RunNumber),
                                                                   data = df_bpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.phyAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.phyAlert)







WS.Power.Part.Scen.Run <- lmer(log(avgSpeed) ~ 1 + 
                                 (1 | ParticipantID) +
                                 (1 | Scenario) +
                                 (1 | RunNumber),
                               data = df_bpSumTestID, REML = FALSE
)

# add run number
WS.Power.Part.Scen.Run.phyAlert <- lmer(log(avgSpeed) ~ 1 + augObjectDetected +
                                          (1 | ParticipantID) +
                                          (1 | Scenario) +
                                          (1 | RunNumber),
                                        data = df_bpSumTestID, REML = FALSE
)

anova(WS.Power.Part.Scen.Run, WS.Power.Part.Scen.Run.phyAlert)

# add run number
WS.Power.Part.Scen.Run.phyAlert.augAlert <- lmer(log(avgSpeed) ~ 1 + augObjectDetected + physObjectDetected +  + 
                                                   (1 | ParticipantID) +
                                                   (1 | Scenario) +
                                                   (1 | RunNumber),
                                                 data = df_bpSumTestID, REML = FALSE
)

anova(WS.Power.Part.Scen.Run.phyAlert.augAlert, WS.Power.Part.Scen.Run.phyAlert)

#----- LMEM: Walking Speed vs Aug-Alerts -----

# Making the NULL model, only including participants
WS.Power.Part <- lmer(log(avgSpeed) ~ 1 +
                        (1 | ParticipantID),
                      data = df_bpSumTestID, REML = FALSE )

# add scenario
WS.Power.Part.Scen <- lmer(log(avgSpeed) ~ 1 +
                             (1 | ParticipantID) +
                             (1 | Scenario),
                           data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part, WS.Power.Part.Scen)

# Add number of alerts 
WS.Power.Part.Scen.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                      (1 | ParticipantID) +
                                      (1 | Scenario),
                                    data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen, WS.Power.Part.Scen.augAlert)

# add slope to participants
WS.Power.Part.Slope.Scen.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                            (1 + log(1 + augObjectDetected) | ParticipantID) +
                                            (1 | Scenario),
                                          data = df_bpSumTestID, REML = FALSE
)

# significant - yes
anova(WS.Power.Part.Scen.augAlert, WS.Power.Part.Slope.Scen.augAlert)

# add slope to scenario
WS.Power.Part.Slope.Scen.Slope.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                  (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                  (1 + log(1 + augObjectDetected) | Scenario),
                                                data = df_bpSumTestID, REML = FALSE
)

# significant - yes 
anova(WS.Power.Part.Slope.Scen.augAlert, WS.Power.Part.Slope.Scen.Slope.augAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                      (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                      (1 + log(1 + augObjectDetected) | Scenario) +
                                                      (1 | RunNumber),
                                                    data = df_bpSumTestID, REML = FALSE
)

# significant - Yes 
anova(WS.Power.Part.Slope.Scen.Slope.augAlert, WS.Power.Part.Slope.Scen.Slope.Run.augAlert)

# add run number
WS.Power.Part.Slope.Scen.Slope.Run.Slope.augAlert <- lmer(log(avgSpeed) ~ 1 + log(augObjectDetected + 1) + 
                                                            (1 + log(1 + augObjectDetected) | ParticipantID) +
                                                            (1 + log(1 + augObjectDetected) | Scenario) +
                                                            (1 + log(1 + augObjectDetected) | RunNumber),
                                                          data = df_bpSumTestID, REML = FALSE
)

# significant - No 
anova(WS.Power.Part.Slope.Scen.Slope.Run.augAlert, WS.Power.Part.Slope.Scen.Slope.Run.Slope.augAlert)

# LMER model - linear mixed effect models ----------------


lme1 <- lmer(objColl_Value ~ FOD + (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
lmeNull <- lmer(objColl_Value ~ (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
anova(lme1, lmeNull)


#----- Plot Alert vs speed -----
df_bpSumTestID %>%
  ggplot(aes(
    x = objectDetected ,
    #x = log(physObjectDetected + 1),
    #x = log(augObjectDetected + 1),
    y = avgSpeed#,
    #color = factor(ParticipantID)
  )) +
  geom_point() +
  #geom_smooth(se = F) +
  geom_smooth(method=lm, se = F) +
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

df_bpSumTestID %>%
  ggplot(aes(
    #x = log(objectDetected+1),
    x = physObjectDetected,
    y = augObjectDetected,
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
df_bp %>%
  filter(TimeSinceObjDetStart < 3 & TimeSinceObjDetStart > 0) %>%
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
df_bp %>%
  filter(TimeSinceObjDetStart < 3 & TimeSinceObjDetStart > 0) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = FOD
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
    legend.text = element_text(size = 14)
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Plot Alert Cost: Per Range (cleaned for paper) -----
df_bp %>%
  filter(TimeSinceObjDetStart < 3 & TimeSinceObjDetStart > 0 & !is.na(TimeSinceObjDetStart)) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian,
    colour = factor(Range)
  )) +
  # geom_hline(yintercept=0) +
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
    legend.text = element_text(size = 14)
  ) +
  scale_color_discrete("") +
  scale_shape_discrete("")


#----- Plot Alert Cost: Phys vs Aug (cleaned for paper) -----

df_bp %>%
  filter(TimeSincePhysDetStart < 3 & TimeSincePhysDetStart > 0 & TimeSinceAugDetStart < 3 & TimeSinceAugDetStart > 0) %>%
  ggplot() +
  # geom_hline(yintercept=0, linetype = "dashed") +
  geom_smooth(aes(
    x = TimeSincePhysDetStart,
    y = rollingSpeedMedian,
    # linetype = factor(day),
    color = "PhysicalAlerts"
  ),
  se = F
  ) +
  geom_smooth(aes(
    x = TimeSinceAugDetStart,
    y = rollingSpeedMedian,
    # linetype = factor(day),
    color = "AugmentedAlerts"
  ),
  se = F
  ) +
  # geom_smooth(data = augDetDataTW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#D16103", se = F) +
  # geom_smooth(data = augDetDataCW, aes(x = TimeSinceVibStart, y = SpeedDiffFromStart), color = "#52854C", se = F) +
  theme_bw() +
  ylab("Cehange in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
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
  ) #+
# scale_fill_identity(guide = 'legend') +
# scale_colour_manual(name = NULL,
#                    values =c('PhysicalAlerts'='#00AFBB','AugmentedAlerts'='#D16103'),
#                    labels = c('Augmented Alerts','Physical Alerts')) #+
# scale_color_discrete("") +
# scale_shape_discrete("")

#----- Number of Objects Detected per Range and Condition -----

df_bpSumTestID %>%
  dplyr::group_by(Range, FOD)%>%
  dplyr::summarise(avgObjDet = mean(objectDetected)) %>%
  ggplot(  aes(x = Range, 
           y = avgObjDet, 
           group = FOD, 
           color = FOD,
           shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size=5)+
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1)+
  ylab("Average number of Alerts") +
  xlab("Preview Range in Meter") +
  scale_y_continuous()+
  theme_bw() + 
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Walking speed per Range and Condition -----

df_bpSumTestID %>%
  dplyr::group_by(Range, FOD)%>%
  dplyr::summarise(avgWalkingSpeed = mean(avgSpeed)) %>%
  ggplot(  aes(x = Range, 
               y = avgWalkingSpeed, 
               group = FOD, 
               color = FOD,
               shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size=5)+
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1)+
  ylab("Average Walking speed (m/s)") +
  xlab("Preview Range in Meter") +
  scale_y_continuous()+
  theme_bw() + 
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Number of Collisions per Range and Condition -----

df_bpSumTestID %>%
  dplyr::group_by(Range, FOD)%>%
  dplyr::summarise(avgCollisions = mean(objectCollisions)) %>%
  ggplot(  aes(x = Range, 
               y = avgCollisions, 
               group = FOD, 
               color = FOD,
               shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size=5)+
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1)+
  ylab("Average Number of Object Collisions") +
  xlab("Preview Range in Meter") +
  scale_y_continuous()+
  theme_bw() + 
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") +
  scale_shape_discrete("")

#----- Percentage of Alert: Reduce walking speed -----

SpeedAtDetStart

df_bp %>%
  filter(TimeSinceObjDetStart < 0.51 & TimeSinceObjDetStart > 0.5) %>%
  ggplot(aes(
    x = TimeSinceObjDetStart,
    y = rollingSpeedMedian
  ))

summary(lm(avgSpeed ~ objectDetected * FOD, data = df_bpSumTestID))

mean(df_bp$ObjectDistance)
sum(df_bp$objDetStart)





## New analysis

daggByScenario <- df_bp %>%
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

daggByScen <- df_bp %>%
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

PartSpeed <- df_bpSumTestID %>%
  group_by(ParticipantID) %>%
  dplyr::summarize(
    avgSpeed = mean(avgSpeed),
    maxSpeed = max(avgSpeed),
    minSpeed = min(avgSpeed)
  )

mean(df_bpSumTestID$avgSpeed)
sd(df_bpSumTestID$avgSpeed)

mean(df_bp$ObjDetDuration, na.rm = TRUE)

summary(df_bpSumTestID)

PartVibDur <- df_bp %>%
  group_by(ParticipantID) %>%
  dplyr::summarize(
    avgVibDur = mean(ObjDetDuration, na.rm = TRUE),
    maxVibDur = max(ObjDetDuration, na.rm = TRUE),
    minVibDur = min(ObjDetDuration, na.rm = TRUE)
  )


PartAlert <- df_bpSumTestID %>%
  group_by(FOD) %>%
  dplyr::summarize(
    avgAlert = mean(objectDetected),
    maxAlert = max(objectDetected),
    minAlert = min(objectDetected)
  )

# Below, a summary of our data.
summary(df_bpSumTestID)

# Training time on walking speed
summary(lm(avgSpeed ~ RunNumber, data = df_bpSumTestID))

# Training time and range on walking speed (all data)
summary(lm(avgSpeed ~ Range + RunNumber, data = df_bpSumTestID))

# split data fram based on FOD
WCDat <- df_bpSumTestID[df_bpSumTestID$FOD == "White Cane", ]
wrDat <- df_bpSumTestID[df_bpSumTestID$FOD == "Conical View AWC", ]
corrDat <- df_bpSumTestID[df_bpSumTestID$FOD == "Tunnel View AWC", ]



TunnelDat <- df_bpSumTestID[df_bpSumTestID$FOD != "Conical View AWC", ]
ConicalDat <- df_bpSumTestID[df_bpSumTestID$FOD != "Tunnel View AWC", ]

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
summary(lm(avgSpeed ~ FOD + RunNumber, data = df_bpSumTestID))

# Training time and FOD on walking speed (all data)
summary(lm(objectDetected ~ FOD + RunNumber, data = df_bpSumTestID))

# Training time and Collisions on walking speed (all data)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = df_bpSumTestID))

# Training time and Collisions on walking speed (baseline)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = WCDat))

# Training time and Collisions on walking speed (corridor)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = corrDat))

# Training time and Collisions on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectCollisions + RunNumber, data = wrDat))

# Training time and Detections on walking speed (all data)
summary(lm(avgSpeed ~ objectDetected * objectCollisions + RunNumber, data = df_bpSumTestID))

# Training time and Detections on walking speed (Baseline)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = WCDat))

# Training time and Detections on walking speed (Corridor)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = corrDat))

# Training time and Detections on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectDetected + RunNumber, data = wrDat))



summary(aov(avgSpeed ~ Range * FOD + Error(ParticipantID), data = df_bpSumTestID))

## Walking speed =================================

# In this section we analysis how walking speed is effected by training time, range, FOD, collisions and detections.

### Overview #############################

# To get an overview of the walking speed we first made a histogram with a density curve to see how our data is distributed

# Histogram and curve of avgSpeed
ggplot(df_bpSumTestID, aes(x = avgSpeed)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(avgSpeed)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

# As we can see the data is close to by not quite normally distributed, a Shapiro Wilks test confirms this as the p-values show a significant difference and, thereby rejects the nullhypothesis of the data following a normal distributed.

shapiro.test(df_bpSumTestID$objectCollisions)

# A qq-plots also shows that the date is close to normal distributed with only a few outliers that was a lot faster than the rest.

qqPlot(df_bpSumTestID$avgSpeed)

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
onsets <- df_bp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
         VibStartTime = RunningTime,
         SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- df_bp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
         VibStopTime = RunningTime,
         SpeedAtVibStop = rollingSpeedMedian
  )

dfv <- merge(df_bp, onsets)
dfv <- merge(dfv, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfv$TimeSinceObjDetStart <- dfv$RunningTime - dfv$VibStartTime
dfv$TimeSinceVibStop <- dfv$RunningTime - dfv$VibStopTime


dfv$SpeedChangeFromDetStart <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStart
dfv$SpeedDiffFromStop <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStop


df_bp$ObjDetChangeHlp <- lag(df_bp$Object_detected)


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
# PIDTrialID <- df_bp %>%
#  select(ParticipantID, testID) %>%
#  group_by(ParticipantID, testID) %>%
#  summarise(number = 1) %>%
#  mutate(runningNum = cumsum(number)) %>%
#  select(-number)
# df_bp <- merge(df_bp, PIDTrialID)

plot(density(df_bp$TimeSinceObjDetStart))
plot(density(df_bp$TimeSinceVibStop))

# analysis on how detections affect speed
onsets <- df_bp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
         VibStartTime = RunningTime,
         SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- df_bp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
         VibStopTime = RunningTime,
         SpeedAtVibStop = rollingSpeedMedian
  )

df_bp <- merge(df_bp, onsets)
df_bp <- merge(df_bp, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


df_bp$TimeSinceObjDetStart <- df_bp$RunningTime - df_bp$VibStartTime
df_bp$TimeSinceVibStop <- df_bp$RunningTime - df_bp$VibStopTime


df_bp$SpeedChangeFromDetStart <- df_bp$rollingSpeedMedian - df_bp$SpeedAtVibStart
df_bp$SpeedDiffFromStop <- df_bp$rollingSpeedMedian - df_bp$SpeedAtVibStop
