library(tidyverse)
library(readbulk)
df = readbulk::read_bulk('data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)
df$testID<-as.numeric(gsub("[^0-9.-]", "", substr(df$File,7,10)))
df$timeSinceStart<-as.double(substr(df$Timer,9,16))*1000
df<-df[,c(18:21,23,1:17,22)]
df$FOD<-as.factor(df$FOD)
df<-rename(df, Range = Detection_range_in_Meters) 
df<-rename(df, day = Day_nr.) 
df$Object_collision<-gsub('null', '', df$Object_collision)
df$objBefore<-c('',df[1:(nrow(df)-1),]$Object_collision)
df$objColl<-ifelse(substr(df$Object_collision,1,1)=="B" & df$objBefore=='',1,0)
ggplot(dagg,aes(maxSpeed)) + 
  geom_histogram()+
  scale_y_continuous(trans='log2')

#data needs filtering on Speed

dagg<-df %>% filter(Person_Speed<5)%>%group_by(day,FOD,Scenario,Range)%>%summarize(Time=max(Time_in_MS*1000),avgSpeed=mean(Person_Speed),objectCollisions=sum(objColl,na.rm = TRUE),maxSpeed=max(Person_Speed))
doa<-dagg %>%group_by(day,FOD,Range)%>%summarize(Time=max(Time),speedSD=sd(avgSpeed),avgSpeed=mean(avgSpeed),maxSpeed=max(maxSpeed))

ggplot(dagg,aes(x=FOD,y=Time,color=Range))+geom_point(aes(alpha=.1))

