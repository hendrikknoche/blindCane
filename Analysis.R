library(tidyverse)
library(readbulk)


load('data_all.rda') 
ggplot(dagg,aes(maxSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#data needs filtering on Speed

dagg<-df %>% filter(Person_Speed<5)%>%group_by(day,FOD,Scenario,Range)%>%summarize(Time=max(Time_in_MS*1000),avgSpeed=mean(Person_Speed),objectCollisions=sum(objColl,na.rm = TRUE),maxSpeed=max(Person_Speed))
doa<-dagg %>%group_by(day,FOD,Range)%>%summarize(Time=max(Time),speedSD=sd(avgSpeed),avgSpeed=mean(avgSpeed),maxSpeed=max(maxSpeed))

ggplot(dagg,aes(x=FOD,y=Time,color=Range))+geom_point(aes(alpha=.1))

