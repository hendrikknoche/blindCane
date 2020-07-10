library(tidyverse)
library(ggplot2)
library(scales)
library(magrittr)
library(here)
library(gsheet)
# dfSOTA <- read.csv(here("SOTA","SOTA.csv"), sep=';', na.strings = 'none', stringsAsFactors=FALSE)

df<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1uthbYKNO5X-lwCO1uuJM-04sPn-rInwL8-gEusCmoqc/edit?usp=sharing")


df %<>%
  group_by(Author) %>%
  mutate(sn=1:n())

dataBL <- df %>%
  group_by(Author) %>%
  pivot_longer(
    names_sep = "_",
    cols = c(12:32),
    names_to = c("DV", "aggMeasure", "augType","EMASupp","WCsupp"),
    values_to = "value"
  ) %>% 
  filter(!is.na(value))
 
dataBL %>% 
  filter(aggMeasure =="avg" & DV=="WalkingSpeed") %>%
  ggplot(aes(x = Range, y = value, color = factor(augType),group=factor(augType))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter(width=.2) +
  geom_smooth() +
  stat_smooth(color="red",
              method = 'loess',
              # formula = 'y~a+x*b',
              # method.args = list(start= c(a = 1,b=1)),
              se=FALSE)+
  theme_bw()
  
  geom_point(aes(y = WalkingSpeed_WC), color="green") +
  geom_jitter(aes(y = WalkingSpeed_WC), width=.2) +
  geom_smooth(aes(y = WalkingSpeed_WC)) +
  stat_smooth(aes(y = WalkingSpeed_WC),
              color="green",
              method = 'nls',
              formula = 'y~a+x*b',
              method.args = list(start= c(a = 1,b=1)),
              se=FALSE) +
  geom_point(aes(y = WalkingSpeed_EMAWC), color="blue") +
  geom_jitter(aes(y = WalkingSpeed_EMAWC), width=.2)+
  geom_smooth(aes(y = WalkingSpeed_EMAWC)) +
  stat_smooth(aes(y = WalkingSpeed_EMAWC),
              color = "blue",
              method = 'nls',
              formula = 'y~a+x*b',
              method.args = list(start= c(a = 1,b=1)),
              se = FALSE)+
  theme_bw()