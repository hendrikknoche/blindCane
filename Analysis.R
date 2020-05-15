library(tidyverse)
library(readbulk)
library(lubridate)

#Load all data files
load('data_all.rda')

#Data Grouped by Snario
daggByScen <- df %>% filter(Person_Speed<3)%>%group_by(testID,day,Scenario,FOD,Range)%>%summarize(avgSpeed=mean(Person_Speed),medianSpeed=median(Person_Speed),maxSpeed=max(Person_Speed),minSpeed=min(Person_Speed),objectDetected=sum(objDet,na.rm = TRUE),objectCollisions=sum(objColl,na.rm = TRUE),Time=max(Time_in_MS*1000))%>% arrange(testID)

#add Coloum with sum of total time spent 
daggByScen$totalTimeTraining<-round(cumsum(daggByScen$Time))

#add Coloum with total time spent for a given FOD with a given Range
daggByScen <- daggByScen%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given FOD
daggByScen <- daggByScen%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))

#add Coloum with total time spent for a given Day
daggByScen <- daggByScen%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)))


# helper= read.csv('DataRepair.csv',sep=";")
# daggByScen<-merge(daggByScen,helper)

#----------------------------   The effect of speed

#data needs filtering on Speed

#Histogram of avgSpeed
ggplot(daggByScen,aes(avgSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#AvgSpeed per senario
ggplot(daggByScen,aes(x=Scenario,y=avgSpeed,color=FOD))+
  geom_point(aes(alpha=.1)) 

#Histogram of medianSpeed
ggplot(daggByScen,aes(medianSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#medianSpeed per senario
ggplot(daggByScen,aes(x=Scenario,y=medianSpeed,color=Range))+
  geom_point(aes(alpha=.1))

#Histogram of MaxSpeed
ggplot(daggByScen,aes(maxSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#MaxSpeed per senario
ggplot(daggByScen,aes(x=Scenario,y=maxSpeed,color=Range))+
  geom_point(aes(alpha=.1))

#----------------------------   The effect of detections

#Number of detections effect on time
ggplot(daggByScen,aes(x = Time, y = objectDetected, color = FOD))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of detections effect on time split per FOD
ggplot(daggByScen,aes(x = Time, y = objectDetected, color = Range))+
  geom_point(aes(alpha=.1))+ 
  geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#Number of detections effect on avgSpeed 
ggplot(daggByScen,aes(x = avgSpeed, y = objectDetected, color = FOD))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of detections effect on avgSpeed split per FOD
ggplot(daggByScen,aes(x = avgSpeed, y = objectDetected, color = Range))+
  geom_point(aes(alpha=.1))+ 
  geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#----------------------------   The effect of Collisions

#Number of Collisions effect on time
ggplot(daggByScen,aes(x = Time, y = objectCollisions, color = FOD))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of Collisions effect on time split per FOD
ggplot(daggByScen,aes(x = Time, y = objectCollisions, color = Range))+
  geom_point(aes(alpha=.1))+ 
  geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#Number of Collisions effect on avgSpeed
ggplot(daggByScen,aes(x = avgSpeed, y = objectCollisions, color = FOD))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of Collisions effect on avgSpeed split per FOD
ggplot(daggByScen,aes(x = avgSpeed, y = objectCollisions, color = Range))+
  geom_point(aes(alpha=.1))+ 
  geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))


#----------------------------   The effect of Training Time

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}


#Data grouped by Day, FOD and Range
daggByDFR<-daggByScen %>%group_by(FOD,day,Range)%>%summarize(totalTimeTraining=max(totalTimeTraining),avgTime=mean(Time),ssd = sd(Time),count=n(),se = ssd / sqrt(count))%>%mutate(lower_ci=lower_ci(avgTime,se,count),upper_ci=upper_ci(avgTime,se,count))


doa<-daggByScen %>%group_by(day,FOD,Range)%>%summarize(Time=max(Time),speedSD=sd(avgSpeed),avgSpeed=mean(avgSpeed),maxSpeed=max(maxSpeed))


ggplot(daggByScen,aes(x=FOD,y=Time,color=Range))+
  geom_point(aes(alpha=.1))

ggplot(daggByScen,aes(x=totalTimeTraining,y=Time))+
  geom_point()


ggplot(daggByScen,aes(x=totalTimeTraining,y=Time,color=FOD))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

                                                                                         
ggplot(daggByDFR,aes(x=totalTimeTraining,y=avgTime,color=factor(Range),shape=factor(Range)))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci))+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))
