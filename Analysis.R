library(tidyverse)
library(readbulk)
library(lubridate)
library(ggplot2)
library(scales) 

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
ggplot(daggByScen,aes(x=Scenario,y=avgSpeed,color=FOD,size=Range))+
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
ggplot(daggByScen,aes(y = Time, x = objectDetected, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of detections effect on time split per FOD
ggplot(daggByScen,aes(y = Time, x = objectDetected, color = factor(Range)))+
  geom_point(aes(alpha=.1))+
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#Number of detections effect on avgSpeed 
ggplot(daggByScen,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of detections effect on avgSpeed split per FOD
ggplot(daggByScen,aes(y = avgSpeed, x = objectDetected, color = factor(Range)))+
  geom_point(aes(alpha=.1))+
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#----------------------------   The effect of Collisions

#Number of Collisions effect on time
ggplot(daggByScen,aes(y = Time, x = objectCollisions, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of Collisions effect on time split per FOD
ggplot(daggByScen,aes(y = Time, x = objectCollisions, color = factor(Range)))+
  geom_point(aes(alpha=.1))+ 
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#Number of Collisions effect on avgSpeed
ggplot(daggByScen,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of Collisions effect on avgSpeed split per FOD
ggplot(daggByScen,aes(y = avgSpeed, x = objectCollisions, color = factor(Range)))+
  geom_point(aes(alpha=.1))+ 
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#----------------------------   The effect of Collisions and detection

#Number of detections and collisions based on avgSpeed split by FOD
ggplot(daggByScen,aes(y = avgSpeed, x = objectCollisions, colour = factor(FOD)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(FOD)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(Range))

#Number of detections and collisions based on avgSpeed split by Range
ggplot(daggByScen,aes(y = avgSpeed, x = objectCollisions, colour = factor(Range)))+
  geom_point()+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected, colour = factor(Range)))+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(FOD))

#Number of detections and collisions based on avgSpeed split by FOD and Range
ggplot(daggByScen,aes(y = avgSpeed, x = objectCollisions))+
  geom_point(color="blue")+
  geom_jitter()+
  #geom_smooth(size=0, color = "blue")+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  geom_point(aes(x = objectDetected), color = "green")+
  #geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(FOD), rows=vars(Range))

#----------------------------   Time of compleation vs avgSpeed

#Number of Collisions effect on time
ggplot(daggByScen,aes(x = Time, y = avgSpeed, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(color="red",method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  stat_smooth(color="blue",method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)

#Number of Collisions effect on time split per FOD
ggplot(daggByScen,aes(x = Time, y = avgSpeed, color = factor(Range)))+
  geom_point(aes(alpha=.1))+ 
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#Number of Collisions effect on time split per FOD and per day
ggplot(daggByScen,aes(x = Time, y = avgSpeed, color = factor(Range)))+
  geom_point(aes(alpha=.1))+ 
  geom_jitter()+
  #geom_smooth(size=0)+
  stat_smooth(method = 'nls', formula = 'y~a+x*b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD), row=vars(day))

#----------------------------   Time of compleation vs total time

#time vs total time split by FOD
ggplot(daggByScen,aes(x = totalTimeTraining, y = Time, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()

#time vs total time split by FOD
ggplot(daggByScen,aes(x = totalTimeTraining, y = Time, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD))

#time vs total time split by FOD and day
ggplot(daggByScen,aes(x = totalTimeTraining, y = Time, color = factor(Range)))+
  geom_point()+ 
  geom_jitter()+
  #geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  facet_grid(cols=vars(FOD), row = vars(day))

#----------------------------   The effect of range

ggplot(daggByScen,aes(x=FOD,y=Time,color=Range))+
  geom_point(aes(alpha=.1))

#----------------------------   Heatmap Table

#Data Frame for heatmap table
daggHeat <- df %>% filter(Person_Speed<3)%>%group_by(Scenario,FOD,Range,day)%>%summarize(avgSpeed=mean(Person_Speed),medianSpeed=median(Person_Speed),maxSpeed=max(Person_Speed),minSpeed=min(Person_Speed),objectDetected=sum(objDet,na.rm = TRUE),objectCollisions=sum(objColl,na.rm = TRUE),Time=max(Time_in_MS*1000))

#Heatmap table over avgSpeed
ggplot(daggHeat, aes(x = Range, y = Scenario)) + 
  geom_tile(aes(fill = avgSpeed)) + 
  geom_text(aes(fill = daggHeat$avgSpeed, label = round(daggHeat$avgSpeed, 2))) + 
  scale_fill_gradient2(low = muted("red"), 
                       mid = "yellow", 
                       high = muted("green"), 
                       midpoint = 0.75) + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.text.x = element_text(angle=0, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("HeatMap Over avgSpeed per Scenario") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_y_continuous(trans = "reverse")+
  labs(fill="avgSpeed") +
  facet_grid(cols=vars(FOD), row=vars(day))

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

#Heatmap table over Collisions
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

#----------------------------   The effect of Training Time

#Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

#Data grouped by Day, FOD and Range
daggByDFR<-daggByScen %>%group_by(FOD,day,Range)%>%summarize(totalTimeTraining=max(totalTimeTraining),avgTime=mean(Time),ssd = sd(Time),count=n(),se = ssd / sqrt(count))%>%mutate(lower_ci=lower_ci(avgTime,se,count),upper_ci=upper_ci(avgTime,se,count))

#Learning over the three days
ggplot(daggByDFR,aes(x=totalTimeTraining,y=avgTime,color=factor(Range),shape=factor(Range)))+
  geom_point()+ 
  geom_smooth(size=0)+ 
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci))+
  stat_smooth(method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  theme_bw()+
  #geom_point(aes(y = objectDetected), color = "green")+
  #stat_smooth(aes(y = objectDetected), color="green",method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+
  facet_grid(cols=vars(FOD))



doa<-daggByScen %>%group_by(day,FOD,Range)%>%summarize(Time=max(Time),speedSD=sd(avgSpeed),avgSpeed=mean(avgSpeed),maxSpeed=max(maxSpeed))
