# Analysis Original Self Evaluation Study ---------------------------------

# The presented results are based on measures from 420 test runs. 

# Initialize the libraries
library(readbulk)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library("car")
library(reshape2)
library(tidyverse)
library(here)
library(gsheet)
library(ggforce)
library(ggpubr)
library(grid)
library(png)
library(ggimage)
library(zoo)

# GetData
load("data_Training_All.rda")
load("data_Training_SumTestID.rda")

## The effect of vibration alerts ---------
# Change in Rolling Median based on FODs when vibration starts
dft %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_smooth(aes(
    colour = factor(FOD)
  ),
  #colour = "gray80",
  alpha = 0.5,
  se = FALSE
  ) +
  #geom_smooth(
  #  colour = "magenta1",
  #  se = FALSE
  #) +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") #+
#scale_shape_discrete("FOA")  

# Change in Speed Difference based on FODs when vibration starts
dft %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_hline(yintercept=0) +
  geom_smooth() +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") #+

# Change based on FODs and range when vibration starts
dft %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(cols = vars(Range))

# Change based on FODs and range when vibration starts
dft %>%
  filter(TimeSinceVibStart < 2 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_hline(yintercept=0) +
  geom_smooth(aes(
    colour = factor(FOD)
  ),
  #colour = "gray80",
  alpha = 0.5,
  se = FALSE
  ) +
  #geom_smooth(
  #  colour = "magenta1",
  #  se = FALSE
  #) +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") #+
#scale_shape_discrete("FOA") 

# Change based on day when vibration starts
dft %>%
  filter(TimeSinceVibStart < 2 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_hline(yintercept=0) +
  geom_smooth(aes(
    colour = factor(day)
  ),
  #colour = "gray80",
  alpha = 0.5,
  se = FALSE
  ) +
  #geom_smooth(
  #  colour = "magenta1",
  #  se = FALSE
  #) +
  theme_bw() +
  ylab("Change in Walking Speed From Alert Onset (m/s)") +
  xlab("Time Since Alert Onset (seconds)") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("Day") #+
#scale_shape_discrete("FOA") 

# Change in Rolling Median based on FODs when vibration stops
dft %>%
  filter(TimeSinceVibStop < 2.5 & TimeSinceVibStop > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStop,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD))


## Analysis of groupped data for paper -----------

# Get Data
load("data_Training_SumTestID.rda")

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

#Training Time
daggByDFR <- dftSumTestID %>%
  group_by(Range, day, FOD) %>%
  summarize(totalTimeTraining = max(totalTimeTraining),
            newAvgSpeed = mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(daggByDFR, aes(x = totalTimeTraining,
                      y = newAvgSpeed,
                      color = factor(Range),
                      shape = factor(Range)))+
  geom_point()+  
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci))+
  stat_smooth(method = 'nls', 
              formula = 'y~a*x^b', 
              method.args = list(start = c(a = 1,
                                           b = 1)),
              se = FALSE)+
  theme_bw()+
  facet_grid(cols = vars(FOD), labeller=labeller(daggByDFR$FOD)) +
  theme(legend.position="bottom", 
        panel.spacing = unit(2, "lines"), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + 
        ylab("Average Walking Speed (m/s)") +
        xlab("Training Time in Seconds") +
        scale_color_discrete("Preview Range in Meters") +
        scale_shape_discrete("Preview Range in Meters")


#Number of Alerts
daggDetectTrain <- dftSumTestID %>%
  group_by(Range, FOD)%>%
  summarise(avgObjDet = mean(objectDetected),
            smean = mean(objectDetected, na.rm = TRUE),
            ssd = sd(objectDetected, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(daggDetectTrain, 
       aes(x = Range, 
           y = avgObjDet, 
           group = FOD, 
           color = FOD,
           shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size=5)+
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1)+
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0.1)) +
  #scale_fill_hue(name="Condition", 
                 #labels=c("White Cane", 
                #          "Body-preview A-ema", 
                #          "Normal A-ema"))+
  #ggtitle("Number of Objects Detected per Range and Condition")+
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
  


# Average Walking speed
daggSpeedTrain <- dftSumTestID %>%
  group_by(Range, FOD) %>%
  summarise(newAvgSpeed = mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = daggSpeedTrain, aes(x = Range, 
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
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") +
  scale_shape_discrete("")


# Number of Collisions
daggCollTrain <- dftSumTestID %>%
  group_by(Range, FOD) %>%
  summarise(newObjColl = mean(objectCollisions),
            smean = mean(objectCollisions, na.rm = TRUE),
            ssd = sd(objectCollisions, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = daggCollTrain, aes(x = Range, 
                                  y = newObjColl, 
                                  group = FOD, 
                                  color = FOD,
                                  shape = FOD)) +
  geom_point(position = position_dodge(0.1), alpha=1, size=5) +
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
  ylab("Average Number of collisions") +
  xlab("Preview Range in Meter") +
  scale_y_continuous() +
  theme_bw() + 
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_color_discrete("") +
  scale_shape_discrete("")


## Statistical analysis -------

### Summary -------




# Below, a summary of our data.
summary(dftSumTestID)

# Training time on walking speed 
summary(lm(avgSpeed ~ totalTimeTrainingHrs, data=dftSumTestID))
        
# Training time and range on walking speed (all data)
summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=dftSumTestID))

# split data fram based on FOD
WCDat <- dftSumTestID[dftSumTestID$FOD=="Baseline",]
wrDat <- dftSumTestID[dftSumTestID$FOD=="WholeRoom",]
corrDat <- dftSumTestID[dftSumTestID$FOD=="Corridor",]

# Training time and range on walking speed (whole room data)
summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=wrDat))

# Training time and range on walking speed (corridor data)
summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=corrDat))

# Training time and FOD on walking speed (all data)
summary(lm(avgSpeed ~ FOD + totalTimeTrainingHrs, data=dftSumTestID))

# Training time and Collisions on walking speed (all data)
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=dftSumTestID))

# Training time and Collisions on walking speed (baseline)
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=WCDat))

# Training time and Collisions on walking speed (corridor)
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=corrDat))

# Training time and Collisions on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=wrDat))

# Training time and Detections on walking speed (all data)
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = dftSumTestID))

# Training time and Detections on walking speed (Baseline)
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = WCDat))

# Training time and Detections on walking speed (Corridor)
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = corrDat))

# Training time and Detections on walking speed (Whole Room)
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = wrDat))

### Walking speed ---------

# In this section we analysis how walking speed is effected by training time, range, FOD, collisions and detections. 

### Overview -------------

# To get an overview of the walking speed we first made a histogram with a density curve to see how our data is distributed

# Histogram and curve of avgSpeed
ggplot(dftSumTestID, aes(x = avgSpeed)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(avgSpeed)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

# As we can see the data is close to by not quite normally distributed, 
# a Shapiro Wilks test confirms this as the p-values show a significant difference and, 
# thereby rejects the nullhypothesis of the data following a normal distributed.

shapiro.test(dftSumTestID$avgSpeed)

# A qq-plots also shows that the date is close to normal distributed with only a few outliers that was a lot faster than the rest.

qqPlot(dftSumTestID$avgSpeed)

#To get a better overview of the outliers we made a heatmap of the 420 test to see if we could locate the outliers.

# Data Frame for heatmap table
daggHeat <- dftSumTestID %>%
  group_by(Scenario, FOD, Range, day)

# Heatmap table over avgSpeed
ggplot(daggHeat, aes(x = Range, 
                     y = Scenario)
) +
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
  theme(legend.title = element_text(face = "bold", 
                                    size = 14)
  ) +
  scale_y_continuous(trans = "reverse") +
  #labs(fill = "avgSpeed") +
  facet_grid(cols = vars(FOD), 
             row = vars(day))

# As we can see scenario one seems to be a lot faster that the other scenarios. To illustrate this further we plot the walking speed for each scenario

# AvgSpeed per senario
ggplot(dftSumTestID, aes(x = Scenario, 
                       y = avgSpeed, 
                       color = FOD, 
                       group = c(Scenario))) +
  geom_jitter(width = .2)+
  geom_boxplot(aes(alpha = .1)) +
  facet_grid(rows=vars(FOD)) +
  theme_bw()

# Here we see that the first senario indeed is faster that the others but only for two of the conditions: Baseline and corridor.

# If we try to remove scenario one and then test for normality we see that it is closer, but still not quite there. 
# However, it is so close that for the rest of the analysis we will assume Guassian distribution, even when including scenario one. 

daggNoScen1 <- dftSumTestID %>%
  filter(Scenario != 1)

shapiro.test(daggNoScen1$avgSpeed)

qqPlot(daggNoScen1$avgSpeed)

### Training Time --------

# We except training time to have a big influences on performance of the individual as when they get more experience they will start to walk faster. 

# If we plot the total training time on the x-axis and then the walking speed on the y-axis for each condition what we see is that the user walks 
# faster the more experiences he gets. 

# Setting up a dataset for avg. speed for each day. 
daggByDFR <- dftSumTestID %>%
  group_by(Range, day, FOD) %>%
  summarize(totalTimeTraining = max(totalTimeTraining),
            newAvgSpeed = mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

# Plot over the avgSpeed over each condition and the three days.
ggplot(dftSumTestID,aes(x = totalTimeTraining, 
                      y = avgSpeed, 
                      color = factor(Range)))+
  geom_point()+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color = "red"),
              method = 'nls', 
              formula = 'y~a*x^b', 
              method.args = list(start= c(a = 1,
                                          b = 1)),
              se = FALSE)+
  theme_bw()

# Graph over the avg.Speed over the three day for each range 
ggplot(daggByDFR, aes(x = totalTimeTraining,
                      y = newAvgSpeed,
                      color = factor(Range),
                      shape = factor(Range)))+
  geom_point()+ 
  #geom_smooth(size=0)+ 
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci))+
  stat_smooth(method = 'nls', 
              formula = 'y~a*x^b', 
              method.args = list(start = c(a = 1,
                                           b = 1)),
              se = FALSE)+
  theme_bw()

# Plot over avg.Speed for each range and each FOD
ggplot(dftSumTestID,aes(x = totalTimeTraining,
                      y = avgSpeed,
                      color = factor(Range))) +
  geom_point(aes(alpha=.1)) + 
  stat_smooth(method = 'nls', 
              formula = 'y~a*x^b', 
              method.args = list(start= c(a = 1, b = 1)),
              se = FALSE) +
  #stat_regline_equation(aes(x=totalTimeTraining, y=avgSpeed))+
  facet_grid(cols = vars(FOD)) +
  theme_bw()

# Graph over avg.Speed for each range and each FOD


# In fact, the training time is a significant predictor of walking speed.

summary(lm(avgSpeed ~ totalTimeTrainingHrs, data=dftSumTestID))

### Range -------

# As we can see on training time there seems to be a clear different between the different ranges and how they effect walking speed. 
# 
# Based on the plots of there seems to be a clear difference between corridor and wholeroom.

daggSpeed <- dftSumTestID %>%
  group_by(Range, FOD) %>%
  summarize(newAvgSpeed=mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(dftSumTestID,aes(x = Range, 
                      y = avgSpeed, 
                      group = FOD,
                      color = factor(FOD))) +
  geom_jitter(position = position_dodge(0.1)) +
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color = factor(FOD)),
              method = 'nls', 
              formula = 'y~a*x^b', 
              method.args = list(start= c(a = 1,
                                          b = 1)),
              se = FALSE) +
  theme_bw()


ggplot(data = daggSpeed, aes(x = Range, 
                             y = newAvgSpeed, 
                             group = FOD, 
                             color = FOD))+
  geom_point(position = position_dodge(0.1), 
             alpha=1)+
  geom_line(position = position_dodge(0.1), 
            alpha=1, size=1)+
  #geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0.1)) +
  geom_text(aes(label = round(newAvgSpeed, 2)), 
            size = 6, 
            alpha=1, 
            position = position_dodge(0.6), 
            vjust = -0.5) +
  scale_fill_hue(name="Condition", 
                 labels=c("White Cane", 
                          "Body-preview aEMA", 
                          "Normal aEMA"))+
  ggtitle("Walking Speed per Range and Condition")+
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous()+
  theme_bw()

# However, when we try to predict the users walking speed based on range on all the data we do not find the range to be a significat predictor.

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=dftSumTestID))

# Thus, we split the data up looking at the different conditions individual. 
# What we find is that range is a significant predictor for wholeroom, but not for corridor. 

WCDat <- dftSumTestID[dftSumTestID$FOD=="Baseline",]
wrDat <- dftSumTestID[dftSumTestID$FOD=="WholeRoom",]
corrDat <- dftSumTestID[dftSumTestID$FOD=="Corridor",]

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=wrDat))

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=corrDat))

### FOD effect on walking speed ----------

#Based on the figure in the previous section it seems that the FOD influences the walking speed of the user. 
#What we find is that wholeroom does negatively predict walking speed, while corridor only show a positively tendency to effect walking speed. 

summary(lm(avgSpeed ~ FOD + totalTimeTrainingHrs, data=dftSumTestID))

### Collisions effect on walking speed ############################# 

#Logically colliding with an object should slow down the person walking. Based on the two plots it looks like that is the case as the more collisions the slower the person walk.  

#Number of Collisions effect on avgSpeed
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_jitter(aes(alpha=.1))+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE) +
  theme_bw()

#Number of Collisions effect on avgSpeed split per FOD
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_jitter(aes(alpha=.1))+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

# To test this we analyzed how collisions effected the whole data set and what we found was that collisions was a significant negative predictor of walking speed

summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=dftSumTestID))


# To make sure it was the same case for the individual conditions we checked of each of them and found that in all cases the number of collisions was a significant predictor of walking speed. 

summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=WCDat))
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=corrDat))
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=wrDat))

### Detections effect on Walking speed ############################# 

#  we would expect based on collisions so does more of detections lower the walking speed of the user. 

#Number of detections effect on avgSpeed 
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE) +
  theme_bw()

#Number of detections effect on avgSpeed split per FOD
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point(aes(alpha=.1))+
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

# To test if this was a significant negative predictor of walking speed of the whole data set we found that it was.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = dftSumTestID))

# The same was also the case for the individual conditions where detections was a significant predictor of walking speed. 

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = wrDat))

# Now the question is how does the number of detections slow down a persons walking speed to find our we started out by making a plot where we show what happens the first 2 second after a detection. 


### Detections and collisions effect on Walking speed ############################# 

#Number of detections and collisions based on avgSpeed split by FOD
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectCollisions, colour = factor(FOD)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(FOD)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(Range))


#Number of detections and collisions based on avgSpeed split by Range
ggplot(dftSumTestID,aes(y = avgSpeed, x = objectCollisions, colour = factor(Range)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(Range)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(FOD))

### Collisions =================================

# Number of Collisions effect on time
ggplot(dftSumTestID, aes(x = Time, y = avgSpeed, color = factor(Range))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  stat_smooth(color = "blue", method = "nls", formula = "y~a*x^b", method.args = list(start = c(a = 1, b = 1)), se = FALSE)

# Number of Collisions effect on time split per FOD
ggplot(dftSumTestID, aes(x = Time, y = avgSpeed, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# Number of Collisions effect on time split per FOD and per day
ggplot(dftSumTestID, aes(x = Time, y = avgSpeed, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD), row = vars(day))



ggplot(daggHeat, aes(x = Range, y = Scenario)) + 
  geom_tile(aes(fill = objectCollisions)) + 
  geom_text(aes(fill = daggHeat$objectCollisions, label = round(daggHeat$objectCollisions, 2))) + 
  scale_fill_gradient2(low = muted("green"), 
                       mid = "yellow", 
                       high = muted("red"), 
                       midpoint = 4) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.text.x = element_text(angle=0, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("HeatMap Over Collisions per Scenario") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_y_continuous(trans = "reverse")+
  labs(fill="Collisions") +
  facet_grid(cols=vars(FOD), row=vars(day))

daggColl <- dftSumTestID %>%
  group_by(Range, FOD)%>%
  summarise(avgColl=mean(objectCollisions),
            smean = mean(objectCollisions, na.rm = TRUE),
            ssd = sd(objectCollisions, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = daggColl, aes(x = Range, 
                            y = avgColl, 
                            group = FOD, 
                            color = FOD)) +
  geom_point(position = position_dodge(0.1), 
             alpha = 1) +
  geom_line(position = position_dodge(0.1), 
            alpha = 1, 
            size = 1) +
  #geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(ymin = lowerci, 
                    ymax = upperci), 
                width = 0.2, 
                color = "Black", 
                position = position_dodge(0.1)) +
  geom_text(aes(label = round(avgColl, 1)), 
            size = 6, 
            alpha = 1, 
            position = position_dodge(0.4), 
            vjust = -0.5) +
  scale_color_manual(labels=c("White Cane", 
                              "Body-preview aEMA", 
                              "Regular aEMA")) +
  ggtitle("Number of Objects Collisions per Range and Condition")+
  ylab("Mean Number of Collisions") +
  scale_y_continuous()+
  theme_bw()

### Detections =================================

# Some text...

#Heatmap table over Detections
ggplot(daggHeat, aes(x = Range, y = Scenario)) + 
  geom_tile(aes(fill = objectDetected)) + 
  geom_text(aes(fill = daggHeat$objectDetected, label = round(daggHeat$objectDetected, 2))) + 
  scale_fill_gradient2(low = muted("green"), 
                       mid = "yellow", 
                       high = muted("red"), 
                       midpoint = 30) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.text.x = element_text(angle=0, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("HeatMap Over Detections per Scenario") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_y_continuous(trans = "reverse")+
  labs(fill="Detections") +
  facet_grid(cols=vars(FOD), row=vars(day))


#Make functions
daggDetect <- dftSumTestID %>%
  group_by(Range, FOD)%>%
  summarise(avgObjDet=mean(objectDetected),
            smean = mean(objectDetected, na.rm = TRUE),
            ssd = sd(objectDetected, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(data = daggDetect, aes(x=Range, y=avgObjDet, group=FOD, color=FOD))+
  geom_point(position = position_dodge(0.1), alpha=1)+
  geom_line(position = position_dodge(0.1), alpha=1, size=1)+
  #geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(ymin = lowerci, ymax = upperci), width = 0.2, color = "Black", position = position_dodge(0.1)) +
  geom_text(aes(label = round(avgObjDet, 1)), size = 6, alpha=1, position = position_dodge(0.45), vjust = -0.5) +
  scale_fill_hue(name="Condition", labels=c("White Cane", "Body-preview aEMA", "Normal aEMA"))+
  ggtitle("Number of Objects Detected per Range and Condition")+
  ylab("Mean Obstacles Detected") +
  scale_y_continuous()+
  theme_bw()

objectsTime <- dftSumTestID$objectDetected / dftSumTestID$Time

ggplot(dftSumTestID,aes(y = avgSpeed, x = objectsTime, color=factor(Range), alpha=0.9))+
  geom_point()+
  geom_jitter()+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  #geom_smooth(size=0, color = "blue")+ 
  #stat_smooth(color="red", method="glm", family="poisson", se=TRUE)+
  #geom_point(aes(x = objectDetected,  color=I("#56B4E9")))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  #stat_smooth(aes(x = objectDetected), color="red", method="glm", family="poisson", se=TRUE)+
  #scale_color_manual(labels=c("White Cane", "Body-preview aEMA", "Normal aEMA"))+
  #facet_grid(cols=vars(FOD))+
  #scale_colour_manual(values=c("#E69F00","#56B4E9"), labels = c("Detections", "Collisions"))+
  #labs(colour = "Collisions")+
  #ggtitle("The effect of Detections on Walking Speed")+
  labs(y = "Mean Walking Speed in Meters per Second", 
       x="Number of obstacle Detections")+
  theme(axis.title.x=element_text(vjust=10,  
                                  size=44), 
        axis.title.y=element_text(size=15))+
  scale_color_manual(name="Range", 
                     values=c("#E69F00","#56B4E9","#143D59","#6DD47E"), 
                     labels=c("1 meter", "2 meter", "3 meter", "4 meter"))+
  #ylab("Mean Walking Speed in Meters per Second") +
  #xlab("Number of obstacle Detections") +
  facet_grid(cols=vars(FOD))+
  theme_bw()


### Detections and Collisions ------

# Amount of collisions and detections over training time
ggplot(dftSumTestID, aes(x = day, y = objectCollisions, color = factor(Range))) +
  # geom_point()+
  geom_smooth(aes(x = day, y = objectDetected)) +
  stat_smooth() +
  facet_grid(cols = vars(FOD, Range)) +
  ylab("objectCollisions (Bottom) & objectsDetected (Top)")