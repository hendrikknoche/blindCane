# Analysis Original Self Evaluation Study ---------------------------------

# The presented results are based on measures from each of the 420 test runs (140 per day). The number of collisions and detections are the total for the given run. Walking speed is the mean walking speed of the entire run (unless something else is specified), thus if the participant stopped walking, it would lower the mean walking speed for that specific run.

# Initialize the libraries
library(readbulk)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library("car")

# not sure if we use these libraries
#library(ggpubr)
#library(grid)
#library(png)
#library(ggimage)

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
load("data_all_Training.rda")

#Data Grouped by Snario
daggByScenTrain <- dft %>% 
  filter(Person_Speed<3)%>%
  group_by(testID, day, Scenario, FOD, Range)%>%
  summarize(avgSpeed = mean(Person_Speed),
            medianSpeed = median(Person_Speed),
            maxSpeed = max(Person_Speed),
            minSpeed = min(Person_Speed),
            objectDetected = sum(objDet,na.rm = TRUE),
            objectCollisions = sum(objColl,na.rm = TRUE),
            Time = max(Time_in_S))%>% 
  arrange(testID)

#add Coloum with sum of total time spent 
daggByScenTrain$totalTimeTraining<-round(cumsum(daggByScenTrain$Time))

#add Coloum with total time spent for a given FOD with a given Range
daggByScenTrain <- daggByScenTrain%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
daggByScenTrain <- daggByScenTrain%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
daggByScenTrain <- daggByScenTrain%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)),totalTimeTrainingHrs=totalTimeTraining/3600)

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# Below, a summary of our data. In total, 420 tests were completed over three days (140 per day), using three different Field Of Detections (FOD - Baseline, WholeRoom and Corridor). The WholeRoom and Corridor differ between three ranges (two, three and four meters), while the Baseline represents the original white cane (one meter range). Scenarios describe the obstacle courses the system was tested on (20 different scenarios). In addition, each test logged the walking speed of the participant, the amount of objects detected by the cane/EMA, the amount of collisions by the user and the completion time of the individual obstacle courses.

summary(daggByScenTrain)

## Walking speed =================================

# In this section we analysis how walking speed is effected by training time, range, FOD, collisions and detections. 

### Overview ############################# 

# To get an overview of the walking speed we first made a histogram with a density curve to see how our data is distributed

# Histogram and curve of avgSpeed
ggplot(daggByScenTrain, aes(x = avgSpeed)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(avgSpeed)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

# As we can see the data is close to by not quite normally distributed, a Shapiro Wilks test confirms this as the p-values show a significant difference and, thereby rejects the nullhypothesis of the data following a normal distributed.

shapiro.test(daggByScenTrain$avgSpeed)

# A qq-plots also shows that the date is close to normal distributed with only a few outliers that was a lot faster than the rest.

qqPlot(daggByScenTrain$avgSpeed)

#To get a better overview of the outliers we made a heatmap of the 420 test to see if we could locate the outliers.

# Data Frame for heatmap table
daggHeat <- daggByScenTrain %>%
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
ggplot(daggByScenTrain, aes(x = Scenario, 
                       y = avgSpeed, 
                       color = FOD, 
                       group = c(Scenario))) +
  geom_jitter(width = .2)+
  geom_boxplot(aes(alpha = .1)) +
  facet_grid(rows=vars(FOD))

# Here we see that the first senario indeed is faster that the others but only for two of the conditions: Baseline and corridor.

# If we try to remove scenario one and then test for normality we see that it is closer, but still not quite there. However, it is so close that for the rest of the analysis we will assume Guassian distribution, even when including scenario one. 

daggNoScen1 <- daggByScenTrain %>%
  filter(Scenario != 1)

shapiro.test(daggNoScen1$avgSpeed)

qqPlot(daggNoScen1$avgSpeed)

### Training Time ############################# 

# We except training time to have a big influences on performance of the individual as when they get more experience they will start to walk faster. 

# If we plot the total training time on the x-axis and then the walking speed on the y-axis for each condition what we see is that the user walks faster the more experiences he gets. 

# Setting up a dataset for avg. speed for each day. 
daggByDFR <- daggByScenTrain %>%
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
ggplot(daggByScenTrain,aes(x = totalTimeTraining, 
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
ggplot(daggByScenTrain,aes(x = totalTimeTraining,
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
  theme_bw()+
  facet_grid(cols = vars(FOD))

# In fact, the training time is a significant predictor of walking speed.

summary(lm(avgSpeed ~ totalTimeTrainingHrs, data=daggByScenTrain))

### Range

# As we can see on training time there seems to be a clear different between the different ranges and how they effect walking speed. 
# 
# Based on the plots of there seems to be a clear difference between corridor and wholeroom.

daggSpeed <- daggByScenTrain %>%
  group_by(Range, FOD) %>%
  summarize(newAvgSpeed=mean(avgSpeed),
            smean = mean(avgSpeed, na.rm = TRUE),
            ssd = sd(avgSpeed, na.rm = TRUE),
            count = n()) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count))

ggplot(daggByScenTrain,aes(x = Range, 
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

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=daggByScenTrain))

# Thus, we split the data up looking at the different conditions individual. What we find is that range is a significant predictor for wholeroom, but not for corridor. 

WCDat <- daggByScenTrain[daggByScenTrain$FOD=="Baseline",]
wrDat <- daggByScenTrain[daggByScenTrain$FOD=="WholeRoom",]
corrDat <- daggByScenTrain[daggByScenTrain$FOD=="Corridor",]

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=wrDat))

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data=corrDat))

### FOD effect on walking speed ############################# 

#Based on the figure in the previous section it seems that the FOD influences the walking speed of the user. What we find is that wholeroom does negatively predict walking speed, while corridor only show a positively tendency to effect walking speed. 

summary(lm(avgSpeed ~ FOD + totalTimeTrainingHrs, data=daggByScenTrain))

### Collisions effect on walking speed ############################# 

#Logically colliding with an object should slow down the person walking. Based on the two plots it looks like that is the case as the more collisions the slower the person walk.  

#Number of Collisions effect on avgSpeed
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_jitter(aes(alpha=.1))+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE) +
  theme_bw()

#Number of Collisions effect on avgSpeed split per FOD
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_jitter(aes(alpha=.1))+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

# To test this we analyzed how collisions effected the whole data set and what we found was that collisions was a significant negative predictor of walking speed

summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=daggByScenTrain))


# To make sure it was the same case for the individual conditions we checked of each of them and found that in all cases the number of collisions was a significant predictor of walking speed. 

summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=WCDat))
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=corrDat))
summary(lm(avgSpeed ~ objectCollisions+totalTimeTrainingHrs,data=wrDat))

### Detections effect on Walking speed ############################# 

#  we would expect based on collisions so does more of detections lower the walking speed of the user. 

#Number of detections effect on avgSpeed 
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE) +
  theme_bw()

#Number of detections effect on avgSpeed split per FOD
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point(aes(alpha=.1))+
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

# To test if this was a significant negative predictor of walking speed of the whole data set we found that it was.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = daggByScenTrain))

# The same was also the case for the individual conditions where detections was a significant predictor of walking speed. 

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = wrDat))

# Now the question is how does the number of detections slow down a persons walking speed to find our we started out by making a plot where we show what happens the first 2 second after a detection. 


### Detections and collisions effect on Walking speed ############################# 

#Number of detections and collisions based on avgSpeed split by FOD
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectCollisions, colour = factor(FOD)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(FOD)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(Range))


#Number of detections and collisions based on avgSpeed split by Range
ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectCollisions, colour = factor(Range)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(Range)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(FOD))

## Collisions =================================

# Some text


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

daggColl <- daggByScenTrain %>%
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

## Detections =================================

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
daggDetect <- daggByScenTrain %>%
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

objectsTime <- daggByScenTrain$objectDetected / daggByScenTrain$Time

ggplot(daggByScenTrain,aes(y = avgSpeed, x = objectsTime, color=factor(Range), alpha=0.9))+
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


# analysis on how detections affect speed
onsets <- dft %>% 
  filter(objDet == 1) %>% 
  select(ObjDetID,
         VibStartTime = RunningTime, 
         SpeedAtVibStart = rollingSpeedMedian)

offsets <- dft %>% 
  filter(objDetStop == 1) %>% 
  select(ObjDetID,
         VibStopTime = RunningTime, 
         SpeedAtVibStop = rollingSpeedMedian)

dfv<-merge(dft,onsets)
dfv<-merge(dfv,offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfv$TimeSinceVibStart <- dfv$RunningTime - dfv$VibStartTime
dfv$TimeSinceVibStop <- dfv$RunningTime - dfv$VibStopTime


dfv$SpeedDiffFromStart <- dfv$rollingSpeedMedian-dfv$SpeedAtVibStart
dfv$SpeedDiffFromStop <- dfv$rollingSpeedMedian-dfv$SpeedAtVibStop


dft$ObjDetChangeHlp <- lag(dft$Object_detected)


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian))+
  geom_line(alpha = .04)+
  facet_grid(rows = vars(FOD))+
  theme_bw()


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian)) + 
  geom_point(alpha = .1, 
             size = .5) + 
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = SpeedDiffFromStart)) + 
  geom_point(alpha = .1,
             size = .5) + 
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = SpeedDiffFromStart,
             colour = FOD)) +
  geom_smooth() + 
  theme_bw()


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian,
             colour = FOD)) + 
  geom_smooth() + 
  theme_bw() + 
  facet_grid(cols = vars(Range))


dfv %>% 
  filter(TimeSinceVibStart < 2 & TimeSinceVibStart>0) %>% 
  ggplot(aes(x = TimeSinceVibStart, 
             y = rollingSpeedMedian, 
             colour = FOD)) + 
  geom_smooth() + 
  theme_bw() +
  facet_grid(cols = vars(FOD))


dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian,
             colour = factor(day))) + 
  geom_smooth() + 
  theme_bw()



dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian,
             colour = FOD)) + 
  geom_smooth() + 
  theme_bw() + 
  facet_grid(rows = vars(FOD))

dfv %>% 
  filter(TimeSinceVibStop < 2.5 & TimeSinceVibStop > 0) %>% 
  ggplot(aes(x = TimeSinceVibStop,
             y = rollingSpeedMedian,
             colour = FOD)) + 
  geom_smooth() + 
  theme_bw() + 
  facet_grid(rows = vars(FOD))



dfvbaseDat <- dfv[dfv$FOD=="Baseline",]
dfvwrDat <- dfv[dfv$FOD=="WholeRoom",]
dfvcorrDat <- dfv[dfv$FOD=="Corridor",]

vibDuration <- median(dfv$VibStopTime - dfv$VibStartTime)
vibDurationBase <- median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime)
vibDurationWR <- median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime)
vibDurationCorr <- median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime)


dm <- data.frame("FOD" = c("Baseline", "WholeRoom", "Corridor"),
                 "medianDuration" = c(median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime), 
                                      median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime), 
                                      median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime))
)

dfv %>% 
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>% 
  ggplot(data = dfv,
         aes(x = TimeSinceVibStart,
             y = rollingSpeedMedian,
             colour = FOD)) +
  geom_smooth() + 
  theme_bw() + 
  facet_grid(rows = vars(FOD)) +
  geom_rect(data = dm, 
            mapping = aes(xmin = medianDuration, 
                          xmax = medianDuration, 
                          ymin = 0, 
                          ymax = 1, 
                          fill = FOD), 
            color="black", 
            alpha=0.5)


geom_point(data = dm, 
           aes(x = median)) 


geom_point(aes(x = vibDurationWR,
               colour = "WholeRoom")) +
  geom_point(aes(x = vibDurationCorr,
                 colour = "Corridor"))







