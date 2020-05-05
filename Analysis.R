library(tidyverse)
library(readbulk)
df = readbulk::read_bulk('results_final', sep=';', na.strings = 'none', stringsAsFactors=FALSE)
df$timeSinceStart<-as.double(substr(df$Timer,9,16))*1000

df %>% group_by(Test.ID,FOD,Scenario)%>%summarize(sumOfTime=sum(timeSinceStart))
