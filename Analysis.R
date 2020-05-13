library(tidyverse)
library(readbulk)
df = readbulk::read_bulk('data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)
df$testID<-as.numeric(gsub("[^0-9.-]", "", substr(df$File,7,10)))
df$timeSinceStart<-as.double(substr(df$Timer,9,16))*1000
df<-df[,c(18:21,23,1:17,22)]
df %>% group_by(Test.ID,FOD,Scenario)%>%summarize(sumOfTime=sum(timeSinceStart))