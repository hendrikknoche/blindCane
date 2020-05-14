library(tidyverse)
library(readbulk)
library(lubridate)


load('data_all.rda') 
# helper= read.csv('DataRepair.csv',sep=";")
# daggByScen<-merge(daggByScen,helper)


ggplot(dagg,aes(maxSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#data needs filtering on Speed
# testID, day, 
daggByScen<-df %>% filter(Person_Speed<5)%>%group_by(day,FOD,testID, Scenario,Range)%>%summarize(Time=max(Time_in_MS*1000),avgSpeed=mean(Person_Speed),objectCollisions=sum(objColl,na.rm = TRUE),maxSpeed=max(Person_Speed))%>% arrange(testID)
daggByScen$totalTimeTraining<-round(cumsum(daggByScen$Time))

daggByScen <-daggByScen%>%group_by(FOD,day,Range)%>%mutate(timeFDRtrain=round(cumsum(Time)))
daggByScen <-daggByScen%>%group_by(FOD,day)%>%mutate(timeFDtrain=round(cumsum(Time)))
daggByScen <-daggByScen%>%group_by(day)%>%mutate(timeDtrain=round(cumsum(Time)))

doa<-daggByScen %>%group_by(day,FOD,Range)%>%summarize(Time=max(Time),speedSD=sd(avgSpeed),avgSpeed=mean(avgSpeed),maxSpeed=max(maxSpeed))

ggplot(daggByScen,aes(x=FOD,y=Time,color=Range))+geom_point(aes(alpha=.1))

ggplot(daggByScen,aes(x=totalTimeTraining,y=Time))+geom_point()
ggplot(daggByScen,aes(x=totalTimeTraining,y=Time,color=FOD))+geom_point()+ geom_smooth(size=0)+ 
  stat_smooth(aes(color="red"),method = 'nls', formula = 'y~a*x^b', method.args = list(start= c(a = 1,b=1)),se=FALSE)+theme_bw()+facet_grid(cols=vars(FOD))

                                                                                         