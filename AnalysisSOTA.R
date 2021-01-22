# State of the art Analysis ---------------------------------


# In this chapter we analysis the state of the art studies in terms of safety (**Number of Collisions**) and efficiency (**Walking Speed**) to find out how they are affected by the detection **Range** of the EMA and the **Training Time** of user. 


## Loading and setting up data =================================

# First we need to load and Tidy the data. The naming convention for the Tidy is: DependentVariable_Measurement_TypeOfAid_UseEMA_UseWhiteCane
# 
# Dependent Variable:
#   - Walking Speed
# - Collisions 
# 
# Measurements:
#   - Average
# - Sum
# - SD
# 
# Type of Aid:
#   - p
# - s
# - a
# - r
# - n (none)
# 
# Use EMA:
#   - EMA
# -   (blank)
# 
# Use White Cane:
#   - WC
# -   (blank)


# Initialize the libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(magrittr)
library(here)
library(gsheet)
library(ggforce)

# Load data from Google Sheets
dfSOTA<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1uthbYKNO5X-lwCO1uuJM-04sPn-rInwL8-gEusCmoqc/edit?usp=sharing")

# Group of the data based on Author
dfSOTA %<>%
  group_by(Author) %>%
  mutate(sn=1:n())

# Tidy the dataset
dataSOTA <- dfSOTA %>%
  group_by(Author) %>%
  pivot_longer(
    names_sep = "_",
    cols = c(14:33),
    names_to = c("DV", "aggMeasure", "augType","EMASupp","WCsupp"),
    values_to = "value"
  ) %>% 
  filter(!is.na(value))


# Re-size the range of the white cane 
dataSOTA <- dataSOTA %>%
  mutate(Range = ifelse(augType == "n", 1, Range))



## Walking Speed =================================

# We star out by analyzing how walking speed is effected by **Training Time**.
# 
# Thus, by having training time out of the x-axis and Walking Speed up the y-axis we clearly see that the longer training time the participants has have the faster the user walks.

# AvgSpeed over training time
dataSOTA %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = TrainingTimeHours, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = 5)) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()

# To get a better overview we separate the individual studies. This shows us that for the studies that only had five minutes to train were almost separated into two groups 
# 
# Group 1:
#   - Kim
# - Kuchenbecker
# - Santos
# 
# Group 2:
#   - Roentgen
# - Lee
# - Gao
# 
# What is interesting about that is that all the studies in group 1 (which is the slowest group) uses aEMAs. While group 2 uses uses pEMA and sEMAs (with the exception of Roentgen who use both pEMA and an aEMA). This could indicate that there is a difference between what type of EMA you use and how fast you are walking. However, the walking speed when using of the white cane also changes based on the study. So in group 1 they are slower when using the white cane compared to group 2. This could indicate that it is something else about the experimental setup in these studies that makes them slower and the fact that they all use aEMAs might just be a coincident.

# AvgSpeed over training time grouped by Author 
dataSOTA %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = TrainingTimeHours, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = 5)) +
  geom_jitter(aes(alpha = .1)) +
  geom_mark_ellipse(aes(group = factor(Author), 
                        fill = Author, 
                        label = Author), 
                    size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()

# Separating based on type of aid shows that aEMA does appear to result in lower walking speed. However, it also shows that the avg walking speed using the white cane differ a lot despite training time. It seems that the type of EMA might have an influence on walking speed, but it is very difficult to say something with certainty.   

# AvgSpeed over training time grouped by EMA type 
dataSOTA %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = TrainingTimeHours, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = 5)) +
  geom_jitter(aes(alpha = .1)) +
  geom_mark_ellipse(aes(group = factor(augType), 
                        fill = augType, 
                        label = augType), 
                    size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()


# Some of the independent variable that changes from study to study is the course length and number of obstacles giving each study a different obstacles density. These two variables may explain why some studies have slower walking speed than others. What we see is a small down wards slop which indicate that the higher density of obstacles the slower the person walks. However, here it is also clear just how much training time influences the results. 

# AvgSpeed by Obstacle density 
dataSOTA %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = Density, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  geom_jitter(aes(alpha = .1)) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size=1) +
  geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()

dataSOTA %>% 
  filter(aggMeasure == "avg", TrainingTimeHours < 1) %>%
  ggplot(aes(x = Density, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  geom_jitter(aes(alpha = .1)) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size=1) +
  geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()

dataSOTA %>% 
  filter(aggMeasure == "avg", TrainingTimeHours > 1) %>%
  ggplot(aes(x = Density, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  geom_jitter(aes(alpha = .1)) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size=1) +
  geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) +
  ylab("Avg. Walking Speed") +
  theme_bw()


# Density isn't the only independent variable much more interesting is is the detection range of the EMA. Here it is difficult to say just how big of an effect the range has on walking speed as training time completely dominates the small ranges.

dataSOTA %>% 
  filter(aggMeasure == "avg") %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = 1),
              #formula = value ~ TrainingTimeHours*Range,
              method = 'lm', 
  ) +
  ylab("Avg. Walking Speed") +
  theme_bw()

#To see just how much range influenced walking speed not taking into account walking speed we tried to split up all studies where the participant got more than one hour of training.

dataSOTA %>% 
  filter(aggMeasure == "avg", TrainingTimeHours < 1) %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "n"), 
              method = 'lm', ) +
  ylab("Avg. Walking Speed") +
  theme_bw()

dataSOTA %>% 
  filter(aggMeasure == "avg", TrainingTimeHours > 1) %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "n"), 
              method = 'lm', ) +
  ylab("Avg. Walking Speed") +
  theme_bw()

# What we see is that both groups have a small increase in walking speed the longer the detection range. This could indicate a positive correlation between walking speed and range- while being very slim.


## Collisions =================================

# Like before we star out by analyzing how **Training Time** affect our dependent variable.
# 
# Thus, by having training time out of the x-axis and collisions up the y-axis we clearly see that the longer training time the participants has the less collisions does they have.


# AvgSpeed per Author
dataSOTA %>% 
  filter(aggMeasure == "sum") %>%
  ggplot(aes(x = TrainingTimeHours, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = 5)) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) + 
  ylab("Number of Collisions") +
  theme_bw()

# Grouping based on aid type also shows that training time reduces the number of collisions. 

dataSOTA %>% 
  filter(aggMeasure == "sum") %>%
  ggplot(aes(x = TrainingTimeHours, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = 5)) +
  geom_jitter(aes(alpha = .1)) +
  geom_mark_ellipse(aes(group = factor(augType), 
                        fill = augType, 
                        label = augType), 
                    size=1) +
  ylab("Number of Collisions") +
  theme_bw()

# At the same time the density of the obstacle course results in more collisions. So the fewer obstacles the less collisions.   

dataSOTA %>% 
  filter(aggMeasure == "sum") %>%
  ggplot(aes(x = Density, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  geom_jitter(aes(alpha = .1)) +
    geom_smooth(aes(group = "1"), 
              method = 'lm',
              size=1) +
  ylab("Number of Collisions") +
  theme_bw()

# It is also very clear that the longer range the less collisions the user experiences. 

dataSOTA %>% 
  filter(aggMeasure == "sum") %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(Density))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "n"), 
              method = 'lm', ) +
  ylab("Number of Collisions") +
  theme_bw()

# Funny, this decrease in collision the longer range is mostly seen in studies whith less than one hour of training, as studies with more than one hour of training increases the number of collisions based on the range. 

dataSOTA %>% 
  filter(aggMeasure == "sum", TrainingTimeHours < 1) %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(TrainingTimeHours))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "n"), 
              method = 'lm', ) +
  ylab("Number of Collisions") +
  theme_bw()

dataSOTA %>% 
  filter(aggMeasure == "avg", TrainingTimeHours > 1) %>%
  ggplot(aes(x = Range, 
             y = value, 
             color = factor(augType), 
             group = factor(augType), 
             size = factor(Density))) +
  #geom_mark_ellipse(aes(group = factor(augType), 
  #                      fill = augType, 
  #                      label = augType), 
  #                  size = 1) +
  geom_jitter(aes(alpha = .1)) +
  geom_smooth(aes(group = "n"), 
              method = 'lm', ) +
  ylab("Number of Collisions") +
  theme_bw()













