library(tidyverse)
library(ggplot2)
library(scales)
library(magrittr)
library(here)
library(gsheet)
library(ggforce)
# dfSOTA <- read.csv(here("SOTA","SOTA.csv"), sep=';', na.strings = 'none', stringsAsFactors=FALSE)

df<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1uthbYKNO5X-lwCO1uuJM-04sPn-rInwL8-gEusCmoqc/edit?usp=sharing")


df %<>%
  group_by(Author) %>%
  mutate(sn=1:n())

# Tidy the dataset
dataBL <- df %>%
  group_by(Author) %>%
  pivot_longer(
    names_sep = "_",
    cols = c(13:33),
    names_to = c("DV", "aggMeasure", "augType","EMASupp","WCsupp"),
    values_to = "value"
  ) %>% 
  filter(!is.na(value))


# Re-size the range of the white cane 
dataBL <- dataBL %>%
  mutate(Range = ifelse(augType == "n", 1, Range))
  
#This is the one we are working on
dataBL %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = TrainingTimeHours, y = value, color = factor(augType), group = factor(augType), size = factor(Range), shape = factor(subjectType))) +
  geom_point(aes(alpha = .1)) +
  geom_mark_ellipse(aes(group = factor(Author), fill = Author, label = Author), size=1) +
  geom_jitter(width=1) +
  #geom_smooth() +
  #stat_smooth(color="red",
  #method = 'loess',
  # formula = 'y~a+x*b',
  # method.args = list(start= c(a = 1,b=1)),
  # se=FALSE)+
  theme_bw()



### Test ---
dataBL %>% 
  filter(aggMeasure =="avg") %>%
  ggplot(aes(x = Range, y = value, color = factor(augType),  size = factor(TrainingTimeHours))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter(width=.2) +
  geom_smooth() +
  #stat_smooth(color="red",
              #method = 'loess',
              # formula = 'y~a+x*b',
              # method.args = list(start= c(a = 1,b=1)),
              #se=FALSE)+
  theme_bw()

dataBL %>% 
  filter(aggMeasure == "sum" & DV == "Collisions") %>%
  ggplot(aes(x = TrainingTimeHours, y = value, color = factor(augType), group=factor(augType))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter(width=.2) +
  #geom_smooth(method = 'loess') +
  #stat_smooth(color="red",
  #method = 'loess',
  # formula = 'y~a+x*b',
  # method.args = list(start= c(a = 1,b=1)),
  #se=FALSE) +
  theme_bw()

dataBL %>% 
  filter(aggMeasure == "sum" & DV == "Collisions") %>%
  ggplot(aes(x = Range, y = value, color = factor(augType), group=factor(augType))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter(width=.2) +
  #geom_smooth(method = 'loess') +
  #stat_smooth(color="red",
  #method = 'loess',
  # formula = 'y~a+x*b',
  # method.args = list(start= c(a = 1,b=1)),
  #se=FALSE) +
  theme_bw()

