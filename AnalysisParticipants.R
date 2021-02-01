# Analysis 10 Participants Study ---------------------------------

# Initialize the libraries
library(readbulk)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(Rmisc)
library("car")

# not sure if we use these libraries
# library(ggpubr)
# library(grid)
# library(png)
# library(ggimage)

library(tidyverse)
library(broom)
library(here)
library(gsheet)
library(ggforce)
library(ggpubr)
library(grid)
library(png)
library(ggimage)
library(zoo)
library(afex)
library(emmeans)

# GetData
load("data_Participants_All.rda")

## The effect of vibration alerts ---------
# Change in Rolling Median based on FODs when vibration starts
dfp %>%
  filter(TimeSinceVibStart < 4 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_smooth() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

dfp %>%
  filter(TimeSinceVibStart < 5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart,
    colour = FOD
  )) +
  geom_smooth() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

dfp %>%
  filter(TimeSinceVibStart < 5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  facet_grid(rows = vars(Range))+
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

dfp %>%
  filter(TimeSinceVibStart < 5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart,
    colour = factor(Range)
  )) +
  geom_smooth(se = F) +
  theme_bw() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  facet_grid(rows = vars(FOD))+
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) +
  scale_color_discrete("FOA") +
  scale_shape_discrete("FOA")

dfp %>%
  filter(TimeSinceVibStart < 4 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_smooth(aes(
    shape = factor(ParticipantID)
  ),
  colour = "gray80",
  se = FALSE
  ) +
  #scale_color_manual(values="#999999") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) #+
#scale_color_discrete("FOA") +
#scale_shape_discrete("FOA") 

dfp %>%
  filter(TimeSinceVibStart < 4 & TimeSinceVibStart > 0 & ParticipantID != 2 & ParticipantID != 9 & ParticipantID != 10) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_smooth(aes(
                  shape = factor(ParticipantID)
                  ),
              colour = "gray80",
              se = FALSE
              ) +
  #scale_color_manual(values="#999999") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ylab("Change in Walking Speed") +
  xlab("Time Since Alert") +
  theme(legend.position="bottom", 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14)) #+
  #scale_color_discrete("FOA") +
  #scale_shape_discrete("FOA") 

## New analysis

daggByScenario <- dfp %>%
  filter(PersonSpeed < 3) %>%
  select(ParticipantID, Scenario, FOD, Range, PersonSpeed, objDet, objColl, TimeSeconds) %>%
  pivot_longer(
    cols = PersonSpeed:TimeSeconds,
    names_to = "measure",
    values_to = "value"
  )  %>%
  group_by(ParticipantID, Scenario, FOD, Range, measure) %>%
  dplyr::summarize(
    avg = mean(value),
    median = median(value),
    max = max(value),
    min = min(value),
    sum = sum(value, na.rm = TRUE)
  ) %>% 
  mutate_each(funs(replace(., is.na(.), 0)), avg:sum) %>%
  pivot_longer(
    cols = avg:sum,
    names_to = "ScenarioAgg",
    values_to = "value"
  )

daggByScenPart <- daggByScenario %>%
  group_by(ParticipantID, FOD, Range, measure, ScenarioAgg) %>%
  dplyr::summarize(
    avg = mean(value),
    median = median(value)
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = avg:median,
    names_to = "PersonAgg",
    values_to = "Value"
  )

PersonBaselines <- daggByScenPart %>%
  ungroup() %>%
  filter(Range == 1) %>%
  select(
    ParticipantID,
    measure,
    PersonAgg,
    ScenarioAgg,
    Value
  ) %>%
  rename(Baseline = Value)

daggByPerson <- daggByScenPart %>%
  merge(PersonBaselines) %>%
  mutate(diffFromBL = Value - Baseline)

#The shapiro test does not work
daggByScenPart %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  select(Value) %>%
  shapiro.test(Value)


# Test significance of Walking Speed
SpeedModel <- daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  do(data.frame(tidy(lm(diffFromBL ~ FOD, data = .))))

SpeedModel

SpeedModelByDet <-
  
dfx <- daggByPerson %>%
  filter(PersonAgg == "avg") %>%
  filter((measure == "PersonSpeed" & ScenarioAgg == "avg") | (measure == "objDet" & ScenarioAgg == "sum") | (measure == "objColl" & ScenarioAgg == "sum")) %>%
  select(ParticipantID, Value, diffFromBL, Range, FOD, measure) %>%
  pivot_wider(names_from = measure, values_from = c(Value, diffFromBL), names_glue = "{measure}_{.value}") %>%
  do(data.frame(tidy(lm(PersonSpeed_Value ~ objDet_Value, data = .))))

summary(lm(PersonSpeed_Value ~ objDet_Value, data = dfx))
summary(lm(objColl_Value ~ objDet_Value, data = dfx))

summary(lm(PersonSpeed_Value ~ objColl_Value, data = dfx))
summary(lm(PersonSpeed_Value ~ objDet_Value * objColl_Value, data = dfx))

summary(lm(objDet_Value ~ Range, data = dfx))

summary(lm(objColl_Value ~ FOD, family = "poisson", data = dfx))

summary(lm(PersonSpeed_Value ~ objColl_Value, data = dfx))
summary(lm(PersonSpeed_Value ~ objDet_Value * objColl_Value, data = dfx))

summary(lm(PersonSpeed_diffFromBL ~ objDet_diffFromBL * objColl_diffFromBL, data = dfx))
summary(lm(PersonSpeed_diffFromBL ~ objDet_diffFromBL * objColl_diffFromBL, data = dfx))

SpeedModelByDet


# Test significance of Detections
ObjDetModel <- daggByPerson %>%
  filter(measure == "objDet", PersonAgg == "avg", ScenarioAgg == "sum") %>%
  do(data.frame(tidy(lm(diffFromBL ~ Range, family = "poisson", data = .))))
ObjDetModel

# Test significance of Collisions
ObjCollModel <- daggByPerson %>%
  filter(measure == "objColl", PersonAgg == "avg", ScenarioAgg == "sum") %>%
  do(data.frame(tidy(lm(diffFromBL ~ Range, family = "poisson", data = .))))
ObjCollModel


daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  ggplot(aes(x = Range, y = Value, group = ParticipantID, colour = factor(ParticipantID))) +
  geom_line() +
  facet_grid(rows = vars(FOD))

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  ggplot(aes(x = Range, y = diffFromBL, group = ParticipantID, colour = factor(ParticipantID))) +
  geom_line() +
  facet_grid(rows = vars(FOD))

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  select(Value) %>%
  ggplot(aes(x = Value)) +
  geom_density()

daggByPerson %>%
  filter(measure == "PersonSpeed", PersonAgg == "avg", ScenarioAgg == "avg") %>%
  filter(FOD == "Corridor") %>%
  select(Value) %>%
  summarise(sd = sd(Value), mean = mean(Value), min = min(Value), max = max(Value))

options(scipen = 999)

dfxColl <- dfx %>%
  group_by(ParticipantID) %>%
  summarise(avgColl = mean(objColl_Value))

dfx %>% 
  summarise(sd = sd(objColl_Value), mean = mean(objColl_Value), min = min(objColl_Value), max = max(objColl_Value))


sd(dfxColl$avgColl)

dfxDet <- dfx %>%
  group_by(ParticipantID) %>%
  summarise(avgDet = mean(objDet_Value))

dfx %>% summarise(sd = sd(objDet_Value), mean = mean(objDet_Value), min = min(objDet_Value), max = max(objDet_Value))

sd(dfxDet$avgDet)

dfxSpeed <- dfx %>%
  group_by(ParticipantID) %>%
  filter(FOD == "WholeRoom") %>%
  summarise(avgSpeed = mean(Person_Speed_Value))

dfx %>%
  filter(FOD == "WholeRoom") %>%
  summarise(sd = sd(Person_Speed_Value), mean = mean(Person_Speed_Value), min = min(Person_Speed_Value), max = max(Person_Speed_Value))

sd(dfxSpeed$avgSpeed)

# Make functions
lower_ci <- function(mean, se, n, conf_level = 0.95) {
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95) {
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# LMER model - linear mixed effect models ----------------
library(lme4)
library("ggpubr")

lme1 <- lmer(objColl_Value ~ FOD + (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
lmeNull <- lmer(objColl_Value ~ (1 | ParticipantID), dfx[dfx$Range > 0, ], REML = FALSE)
anova(lme1, lmeNull)

dfFOD <- dfx %>%
  group_by(ParticipantID, FOD) %>%
  dplyr::summarize(avgColl = mean(objColl_Value))
do(data.frame(tidy(lm(avgColl ~ FOD, data = .))))

dfFODbyPerson <- dfx %>%
  group_by(ParticipantID, FOD) %>%
  dplyr::summarize(avgColl = mean(objColl_Value), avgWS = mean(Person_Speed_Value)) %>%
  ungroup()

dfFODbyPerson %>%
  group_by(FOD) %>%
  dplyr::summarize(medColl = median(avgColl), sdColl = sd(avgColl), avgColl = mean(avgColl)) %>%
  ungroup()

dfFODbyPerson$FOD <- recode(dfFODbyPerson$FOD, Baseline = "white cane", Corridor = "body preview", WholeRoom = "open range")
dfFODbyPerson$FOD <- factor(dfFODbyPerson$FOD, levels = c("white cane", "open range", "body preview"))
ggline(dfFODbyPerson,
  x = "FOD", y = "avgColl",
  add = c("mean_se", "jitter"),
  order = c("white cane", "open range", "body preview"),
  ylab = "number of collisions", xlab = ""
)

dfFODForPlotColl <- summarySE(dfFODbyPerson, measurevar = "avgColl", groupvars = c("FOD"))
dfFODForPlotWS <- summarySE(dfFODbyPerson, measurevar = "avgWS", groupvars = c("FOD"))

ggplot(dfFODForPlotColl, aes(x = FOD, y = avgColl, fill = FOD, colour = FOD, shape = FOD)) +
  geom_point(data = dfFODbyPerson, fill = "grey", color = "grey", width = .1) +
  geom_line(data = dfFODbyPerson, aes(group = ParticipantID), color = "grey", width = .1) +
  geom_errorbar(aes(ymin = avgColl - se, ymax = avgColl + se), color = "black", width = .1) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(20, 25, 22)) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title = element_text(size = 22, face = "bold")) +
  ylab("number of collisions") +
  xlab("")
ggsave(here("..", "..", "S1-CollisionsByFOD.png"), width = 9, height = 6.3)


ggplot(dfFODForPlotWS, aes(x = FOD, y = avgWS, fill = FOD, colour = FOD, shape = FOD)) +
  geom_point(data = dfFODbyPerson, fill = "grey", color = "grey", width = .1) +
  geom_line(data = dfFODbyPerson, aes(group = ParticipantID), color = "grey", width = .1) +
  geom_errorbar(aes(ymin = avgWS - se, ymax = avgWS + se), color = "black", width = .1) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(20, 25, 22)) +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), axis.title = element_text(size = 22, face = "bold")) +
  ylab("walking speed (m/s)") +
  xlab("")
ggsave(here("..", "..", "Study1WalkingSpeed.png"), width = 9, height = 6.3)

# "Dropbox" ,"Apps","Overleaf","CHI Blind Tunnel Vision Limiting the Field of Detection and Range of Electronic Mobility Aids","figures"
set_here(path = "~")

summary(lm(avgColl ~ FOD, dfFOD))

boxplot(objColl_Value ~ Range,
  col = c("white", "lightgray"), dfx
)
boxplot(avgWS ~ FOD,
  col = c("white", "lightgray"), dfFODbyPerson
)

summary(aov(avgWS ~ FOD, dfFODbyPerson))
summary(aov(avgColl ~ FOD, dfFODbyPerson))

# Plot total average Walking speed ----------------
daggAvgSpeed <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "Person_Speed",
    PersonAgg == "avg",
    ScenarioAgg == "avg"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgSpeed = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgSpeed, aes(
  x = Range,
  y = newAvgSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgSpeed, 2)),
    size = 7,
    alpha = 1,
    position = position_dodge(0.8),
    vjust = -0.9
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Open Range aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# Plot Diff in Walking speed ----------------
daggPersonSpeed <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "Person_Speed",
    PersonAgg == "avg",
    ScenarioAgg == "avg"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newDiffSpeed = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggPersonSpeed, aes(
  x = Range,
  y = newDiffSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newDiffSpeed, 2)),
    size = 7,
    alpha = 1,
    position = position_dodge(0.8),
    vjust = -0.9
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Open Range aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# Plot total average collisions ----------------
daggAvgColl <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objColl",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgColl = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgColl, aes(
  x = Range,
  y = newAvgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of collisions per Range and Condition") +
  ylab("Number of collisions") +
  scale_y_continuous() +
  theme_bw()

# Plot difference in average collisions ----------------

daggDiffColl <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objColl",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newAvgColl = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggDiffColl, aes(
  x = Range,
  y = newAvgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of collisions per Range and Condition") +
  ylab("Number of collisions") +
  scale_y_continuous() +
  theme_bw()

# Plot total average detections ----------------
daggAvgDet <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objDet",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(Value) %>%
  dplyr::summarize(
    newAvgDet = mean(Value),
    smean = mean(Value, na.rm = TRUE),
    ssd = sd(Value, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggAvgDet, aes(
  x = Range,
  y = newAvgDet,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgDet, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Detections per Range and Condition") +
  ylab("Number of Detections") +
  scale_y_continuous() +
  theme_bw()

# Plot difference in average collisions ----------------

daggDiffDet <- daggByPerson %>%
  group_by(Range, FOD) %>%
  filter(
    measure == "objDet",
    PersonAgg == "avg",
    ScenarioAgg == "sum"
  ) %>%
  select(diffFromBL) %>%
  dplyr::summarize(
    newAvgDet = mean(diffFromBL),
    smean = mean(diffFromBL, na.rm = TRUE),
    ssd = sd(diffFromBL, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggDiffDet, aes(
  x = Range,
  y = newAvgDet,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgDet, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Detections per Range and Condition") +
  ylab("Number of Detections") +
  scale_y_continuous() +
  theme_bw()


# Old code -------------------------------

daggByScen <- dfp %>%
  filter(Person_Speed < 3) %>%
  group_by(testID, day, Scenario, FOD, Range) %>%
  dplyr::summarize(
    avgSpeed = mean(Person_Speed),
    medianSpeed = median(Person_Speed),
    maxSpeed = max(Person_Speed),
    minSpeed = min(Person_Speed),
    objectDetected = sum(objDet, na.rm = TRUE),
    objectCollisions = sum(objColl, na.rm = TRUE),
    Time = max(Time_in_S)
  ) %>%
  arrange(testID)

# add Coloum with sum of total time spent
daggByScenPart$totalTimeTraining <- round(cumsum(daggByScenPart$Time))

# add Coloum with total time spent for a given FOD with a given Range
daggByScenPart <- daggByScenPart %>%
  group_by(FOD, day, Range) %>%
  mutate(timeFDRtrain = round(cumsum(Time)))

# add Coloum with total time spent for a given FOD
daggByScenPart <- daggByScenPart %>%
  group_by(FOD, day) %>%
  mutate(timeFDtrain = round(cumsum(Time)))

# add Coloum with total time spent for a given Day
daggByScenPart <- daggByScenPart %>%
  group_by(day) %>%
  mutate(timeDtrain = round(cumsum(Time)), totalTimeTrainingHrs = totalTimeTraining / 3600)



# Below, a summary of our data. In total, 420 tests were completed over three days (140 per day), using three different Field Of Detections (FOD - Baseline, WholeRoom and Corridor). The WholeRoom and Corridor differ between three ranges (two, three and four meters), while the Baseline represents the original white cane (one meter range). Scenarios describe the obstacle courses the system was tested on (20 different scenarios). In addition, each test logged the walking speed of the participant, the amount of objects detected by the cane/EMA, the amount of collisions by the user and the completion time of the individual obstacle courses.

summary(daggByScenPart)

summary(aov(avgSpeed ~ Range * FOD + Error(ParticipantID), data = daggByScenPart))

## Walking speed =================================

# In this section we analysis how walking speed is effected by training time, range, FOD, collisions and detections.

### Overview #############################

# To get an overview of the walking speed we first made a histogram with a density curve to see how our data is distributed

# Histogram and curve of avgSpeed
ggplot(daggByScenPart, aes(x = avgSpeed)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(avgSpeed)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

# As we can see the data is close to by not quite normally distributed, a Shapiro Wilks test confirms this as the p-values show a significant difference and, thereby rejects the nullhypothesis of the data following a normal distributed.

shapiro.test(daggByScen$objectCollisions)

# A qq-plots also shows that the date is close to normal distributed with only a few outliers that was a lot faster than the rest.

qqPlot(daggByScenPart$avgSpeed)

# To get a better overview of the outliers we made a heatmap of the 420 test to see if we could locate the outliers.

# Data Frame for heatmap table
daggHeat <- daggByScenPart %>%
  group_by(Scenario, FOD, Range, day)

# Heatmap table over avgSpeed
ggplot(daggHeat, aes(
  x = Range,
  y = Scenario
)) +
  geom_tile(aes(fill = avgSpeed)) +
  geom_text(aes(label = round(avgSpeed, 2))) +
  scale_fill_gradient2(
    low = muted("red"),
    mid = "yellow",
    high = muted("green"),
    midpoint = 0.63
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over avgSpeed per Scenario") +
  theme(legend.title = element_text(
    face = "bold",
    size = 14
  )) +
  scale_y_continuous(trans = "reverse") +
  # labs(fill = "avgSpeed") +
  facet_grid(
    cols = vars(FOD),
    row = vars(day)
  )

# As we can see scenario one seems to be a lot faster that the other scenarios. To illustrate this further we plot the walking speed for each scenario

# AvgSpeed per senario
ggplot(daggByScenPart, aes(
  x = Scenario,
  y = avgSpeed,
  color = FOD,
  group = c(Scenario)
)) +
  geom_jitter(width = .2) +
  geom_boxplot(aes(alpha = .1)) +
  facet_grid(rows = vars(FOD))

# Here we see that the first senario indeed is faster that the others but only for two of the conditions: Baseline and corridor.

# If we try to remove scenario one and then test for normality we see that it is closer, but still not quite there. However, it is so close that for the rest of the analysis we will assume Guassian distribution, even when including scenario one.

daggNoScen1 <- daggByScenPart %>%
  filter(Scenario != 1)

shapiro.test(daggNoScen1$avgSpeed)

qqPlot(daggNoScen1$avgSpeed)

### Training Time #############################

# We except training time to have a big influences on performance of the individual as when they get more experience they will start to walk faster.

# If we plot the total training time on the x-axis and then the walking speed on the y-axis for each condition what we see is that the user walks faster the more experiences he gets.

# Setting up a dataset for avg. speed for each day.
daggByDFR <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  dplyr::summarize(
    totalTimeTraining = max(totalTimeTraining),
    newAvgSpeed = mean(avgSpeed),
    smean = mean(avgSpeed, na.rm = TRUE),
    ssd = sd(avgSpeed, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

# Plot over the avgSpeed over each condition and the three days.
ggplot(daggByScenPart, aes(
  x = totalTimeTraining,
  y = avgSpeed,
  color = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = "red"),
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()

# Graph over the avg.Speed over the three day for each range
ggplot(daggByDFR, aes(
  x = totalTimeTraining,
  y = newAvgSpeed,
  color = factor(Range),
  shape = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  )) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()

# Plot over avg.Speed for each range and each FOD
ggplot(daggByScenPart, aes(
  x = totalTimeTraining,
  y = avgSpeed,
  color = factor(Range)
)) +
  geom_point(aes(alpha = .1)) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(a = 1, b = 1)),
    se = FALSE
  ) +
  # stat_regline_equation(aes(x=totalTimeTraining, y=avgSpeed))+
  facet_grid(cols = vars(FOD)) +
  theme_bw()

# Graph over avg.Speed for each range and each FOD
ggplot(daggByDFR, aes(
  x = totalTimeTraining,
  y = newAvgSpeed,
  color = factor(Range),
  shape = factor(Range)
)) +
  geom_point() +
  # geom_smooth(size=0)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  )) +
  stat_smooth(
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# In fact, the training time is a significant predictor of walking speed.

summary(lm(avgSpeed ~ totalTimeTrainingHrs, data = daggByScenPart))

### Range #############################

# As we can see on training time there seems to be a clear different between the different ranges and how they effect walking speed.
#
# Based on the plots of there seems to be a clear difference between corridor and wholeroom.

daggSpeed <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  dplyr::summarize(
    newAvgSpeed = mean(avgSpeed),
    smean = mean(avgSpeed, na.rm = TRUE),
    ssd = sd(avgSpeed, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(daggByScenPart, aes(
  x = Range,
  y = avgSpeed,
  group = FOD,
  color = factor(FOD)
)) +
  geom_jitter(position = position_dodge(0.1)) +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = factor(FOD)),
    method = "nls",
    formula = "y~a*x^b",
    method.args = list(start = c(
      a = 1,
      b = 1
    )),
    se = FALSE
  ) +
  theme_bw()


ggplot(data = daggSpeed, aes(
  x = Range,
  y = newAvgSpeed,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(newAvgSpeed, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Walking Speed per Range and Condition") +
  ylab("Mean walking speed in meters per Second") +
  scale_y_continuous() +
  theme_bw()

# However, when we try to predict the users walking speed based on range on all the data we do not find the range to be a significat predictor.

summary(lm(avgSpeed ~ Range, data = daggByScenPart))

# Thus, we split the data up looking at the different conditions individual. What we find is that range is a significant predictor for wholeroom, but not for corridor.

WCDat <- daggByScenPart[daggByScenPart$FOD == "Baseline", ]
wrDat <- daggByScenPart[daggByScenPart$FOD == "WholeRoom", ]
corrDat <- daggByScenPart[daggByScenPart$FOD == "Corridor", ]

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data = wrDat))

summary(lm(avgSpeed ~ Range + totalTimeTrainingHrs, data = corrDat))

### FOD effect on walking speed #############################

# Based on the figure in the previous section it seems that the FOD influences the walking speed of the user. What we find is that wholeroom does negatively predict walking speed, while corridor only show a positively tendency to effect walking speed.


### Collisions effect on walking speed #############################

# Logically colliding with an object should slow down the person walking. Based on the two plots it looks like that is the case as the more collisions the slower the person walk.

# Number of Collisions effect on avgSpeed
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, color = factor(Range))) +
  geom_jitter(aes(alpha = .1)) +
  # geom_smooth(size=0)+
  stat_smooth(aes(color = "red"), method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw()

# Number of Collisions effect on avgSpeed split per FOD
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, color = factor(Range))) +
  geom_jitter(aes(alpha = .1)) +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# To test this we analyzed how collisions effected the whole data set and what we found was that collisions was a significant negative predictor of walking speed

summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = daggByScenPart))


# To make sure it was the same case for the individual conditions we checked of each of them and found that in all cases the number of collisions was a significant predictor of walking speed.

summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectCollisions + totalTimeTrainingHrs, data = wrDat))

### Detections effect on Walking speed #############################

#  we would expect based on collisions so does more of detections lower the walking speed of the user.

# Number of detections effect on avgSpeed
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectDetected, color = factor(Range))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw()

# Number of detections effect on avgSpeed split per FOD
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectDetected, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))

# To test if this was a significant negative predictor of walking speed of the whole data set we found that it was.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = daggByScenPart))

# The same was also the case for the individual conditions where detections was a significant predictor of walking speed.

summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = WCDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = corrDat))
summary(lm(avgSpeed ~ objectDetected + totalTimeTrainingHrs, data = wrDat))

# Now the question is how does the number of detections slow down a persons walking speed to find our we started out by making a plot where we show what happens the first 2 second after a detection.


### Detections and collisions effect on Walking speed #############################

# Number of detections and collisions based on avgSpeed split by FOD
ggplot(daggByScenPart, aes(y = objectDetected, x = objectCollisions, color = factor(Range))) +
  geom_point(aes(alpha = .1)) +
  geom_jitter() +
  # geom_smooth(size=0)+
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  theme_bw() +
  facet_grid(cols = vars(FOD))



ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, colour = factor(FOD))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0, color = "blue")+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  geom_point(aes(x = objectDetected, colour = factor(FOD))) +
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), colour = Range, method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  facet_grid(cols = vars(Range))


# Number of detections and collisions based on avgSpeed split by Range
ggplot(daggByScenPart, aes(y = avgSpeed, x = objectCollisions, colour = factor(Range))) +
  geom_point() +
  geom_jitter() +
  # geom_smooth(size=0, color = "blue")+
  stat_smooth(color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  geom_point(aes(x = objectDetected, colour = factor(Range))) +
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  stat_smooth(aes(x = objectDetected), color = "red", method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  facet_grid(cols = vars(FOD))

## Collisions =================================

# Some text


ggplot(daggHeat, aes(x = Range, y = Scenario)) +
  geom_tile(aes(fill = objectCollisions)) +
  geom_text(aes(fill = daggHeat$objectCollisions, label = round(daggHeat$objectCollisions, 2))) +
  scale_fill_gradient2(
    low = muted("green"),
    mid = "yellow",
    high = muted("red"),
    midpoint = 4
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over Collisions per Scenario") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(trans = "reverse") +
  labs(fill = "Collisions") +
  facet_grid(cols = vars(FOD), row = vars(day))

daggColl <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  summarise(
    avgColl = mean(objectCollisions),
    smean = mean(objectCollisions, na.rm = TRUE),
    ssd = sd(objectCollisions, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )

ggplot(data = daggColl, aes(
  x = Range,
  y = avgColl,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(avgColl, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Objects Collisions per Range and Condition") +
  ylab("Mean Number of Collisions") +
  scale_y_continuous() +
  theme_bw()


## Detections =================================

# Some text...

# Heatmap table over Detections
ggplot(daggHeat, aes(x = Range, y = Scenario)) +
  geom_tile(aes(fill = objectDetected)) +
  geom_text(aes(fill = daggHeat$objectDetected, label = round(daggHeat$objectDetected, 2))) +
  scale_fill_gradient2(
    low = muted("green"),
    mid = "yellow",
    high = muted("red"),
    midpoint = 30
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1, size = 12, face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  ) +
  ggtitle("HeatMap Over Detections per Scenario") +
  theme(legend.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(trans = "reverse") +
  labs(fill = "Detections") +
  facet_grid(cols = vars(FOD), row = vars(day))


# Make functions
daggDetect <- daggByScenPart %>%
  group_by(Range, FOD) %>%
  summarise(
    avgObjDet = mean(objectDetected),
    smean = mean(objectDetected, na.rm = TRUE),
    ssd = sd(objectDetected, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lowerci = lower_ci(smean, se, count),
    upperci = upper_ci(smean, se, count)
  )


ggplot(data = daggDetect, aes(
  x = Range,
  y = avgObjDet,
  group = FOD,
  color = FOD
)) +
  geom_point(
    position = position_dodge(0.1),
    alpha = 1
  ) +
  geom_line(
    position = position_dodge(0.1),
    alpha = 1, size = 1
  ) +
  # geom_bar(position="dodge", stat = "identity", size=.3)+
  geom_errorbar(aes(
    ymin = lowerci,
    ymax = upperci
  ),
  width = 0.2,
  color = "Black",
  position = position_dodge(0.1)
  ) +
  geom_text(aes(label = round(avgObjDet, 2)),
    size = 6,
    alpha = 1,
    position = position_dodge(0.6),
    vjust = -0.5
  ) +
  scale_fill_hue(
    name = "Condition",
    labels = c(
      "White Cane",
      "Body-preview aEMA",
      "Normal aEMA"
    )
  ) +
  ggtitle("Number of Objects Detected per Range and Condition") +
  ylab("Mean Obstacles Detected") +
  scale_y_continuous() +
  theme_bw()



objectsTime <- daggByScenPart$objectDetected / daggByScenPart$Time

ggplot(daggByScenPart, aes(y = avgSpeed, x = objectsTime, color = factor(Range), alpha = 0.9)) +
  geom_point() +
  geom_jitter() +
  stat_smooth(method = "nls", formula = "y~a+x*b", method.args = list(start = c(a = 1, b = 1)), se = FALSE) +
  # geom_smooth(size=0, color = "blue")+
  # stat_smooth(color="red", method="glm", family="poisson", se=TRUE)+
  # geom_point(aes(x = objectDetected,  color=I("#56B4E9")))+
  # geom_smooth(aes(x = avgSpeed, y = objectDetected))+
  # stat_smooth(aes(x = objectDetected), color="red", method="glm", family="poisson", se=TRUE)+
  # scale_color_manual(labels=c("White Cane", "Body-preview aEMA", "Normal aEMA"))+
  # facet_grid(cols=vars(FOD))+
  # scale_colour_manual(values=c("#E69F00","#56B4E9"), labels = c("Detections", "Collisions"))+
  # labs(colour = "Collisions")+
  # ggtitle("The effect of Detections on Walking Speed")+
  labs(
    y = "Mean Walking Speed in Meters per Second",
    x = "Number of obstacle Detections"
  ) +
  theme(
    axis.title.x = element_text(
      vjust = 10,
      size = 44
    ),
    axis.title.y = element_text(size = 15)
  ) +
  scale_color_manual(
    name = "Range",
    values = c("#E69F00", "#56B4E9", "#143D59", "#6DD47E"),
    labels = c("1 meter", "2 meter", "3 meter", "4 meter")
  ) +
  # ylab("Mean Walking Speed in Meters per Second") +
  # xlab("Number of obstacle Detections") +
  facet_grid(cols = vars(FOD)) +
  theme_bw()


# analysis on how detections affect speed
onsets <- dfp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
    VibStartTime = RunningTime,
    SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- dfp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
    VibStopTime = RunningTime,
    SpeedAtVibStop = rollingSpeedMedian
  )

dfv <- merge(dfp, onsets)
dfv <- merge(dfv, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfv$TimeSinceVibStart <- dfv$RunningTime - dfv$VibStartTime
dfv$TimeSinceVibStop <- dfv$RunningTime - dfv$VibStopTime


dfv$SpeedDiffFromStart <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStart
dfv$SpeedDiffFromStop <- dfv$rollingSpeedMedian - dfv$SpeedAtVibStop


dfp$ObjDetChangeHlp <- lag(dfp$Object_detected)


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian
  )) +
  geom_line(alpha = .04) +
  facet_grid(rows = vars(FOD)) +
  theme_bw()


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian
  )) +
  geom_point(
    alpha = .1,
    size = .5
  ) +
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart
  )) +
  geom_point(
    alpha = .1,
    size = .5
  ) +
  facet_grid(rows = vars(FOD)) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = SpeedDiffFromStart,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw()


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(cols = vars(Range))


dfv %>%
  filter(TimeSinceVibStart < 2 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(cols = vars(FOD))


dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian,
    colour = factor(day)
  )) +
  geom_smooth() +
  theme_bw()



dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStart,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD))

dfv %>%
  filter(TimeSinceVibStop < 2.5 & TimeSinceVibStop > 0) %>%
  ggplot(aes(
    x = TimeSinceVibStop,
    y = rollingSpeedMedian,
    colour = FOD
  )) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD))



dfvbaseDat <- dfv[dfv$FOD == "Baseline", ]
dfvwrDat <- dfv[dfv$FOD == "WholeRoom", ]
dfvcorrDat <- dfv[dfv$FOD == "Corridor", ]

vibDuration <- median(dfv$VibStopTime - dfv$VibStartTime)
vibDurationBase <- median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime)
vibDurationWR <- median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime)
vibDurationCorr <- median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime)


dm <- data.frame(
  "FOD" = c("Baseline", "WholeRoom", "Corridor"),
  "medianDuration" = c(
    median(dfvbaseDat$VibStopTime - dfvbaseDat$VibStartTime),
    median(dfvwrDat$VibStopTime - dfvwrDat$VibStartTime),
    median(dfvcorrDat$VibStopTime - dfvcorrDat$VibStartTime)
  )
)

dfv %>%
  filter(TimeSinceVibStart < 2.5 & TimeSinceVibStart > 0) %>%
  ggplot(
    data = dfv,
    aes(
      x = TimeSinceVibStart,
      y = rollingSpeedMedian,
      colour = FOD
    )
  ) +
  geom_smooth() +
  theme_bw() +
  facet_grid(rows = vars(FOD)) +
  geom_rect(
    data = dm,
    mapping = aes(
      xmin = medianDuration,
      xmax = medianDuration,
      ymin = 0,
      ymax = 1,
      fill = FOD
    ),
    color = "black",
    alpha = 0.5
  )


geom_point(
  data = dm,
  aes(x = median)
)


geom_point(aes(
  x = vibDurationWR,
  colour = "WholeRoom"
)) +
  geom_point(aes(
    x = vibDurationCorr,
    colour = "Corridor"
  ))


###### Analysis of differences consistency in terms  of collisions ######
col4ICC <- dfx %>%
  select(FOD, Range, ParticipantID, colls = objColl_Value) %>%
  pivot_wider(names_from = ParticipantID, values_from = colls) %>%
  ungroup() %>%
  select(-FOD, -Range)
psych::ICC(col4ICC)

Yes

# Make trail ID
# PIDTrialID <- dfp %>%
#  select(ParticipantID, testID) %>%
#  group_by(ParticipantID, testID) %>%
#  summarise(number = 1) %>%
#  mutate(runningNum = cumsum(number)) %>%
#  select(-number)
# dfp <- merge(dfp, PIDTrialID)

plot(density(dfp$TimeSinceVibStart))
plot(density(dfp$TimeSinceVibStop))

# analysis on how detections affect speed
onsets <- dfp %>%
  filter(objDet == 1) %>%
  select(ObjDetID,
    VibStartTime = RunningTime,
    SpeedAtVibStart = rollingSpeedMedian
  )

offsets <- dfp %>%
  filter(objDetStop == 1) %>%
  select(ObjDetID,
    VibStopTime = RunningTime,
    SpeedAtVibStop = rollingSpeedMedian
  )

dfp <- merge(dfp, onsets)
dfp <- merge(dfp, offsets)

mean(onsets$SpeedAtVibStart, trim = 0, na.rm = TRUE)
mean(offsets$SpeedAtVibStop, trim = 0, na.rm = TRUE)


dfp$TimeSinceVibStart <- dfp$RunningTime - dfp$VibStartTime
dfp$TimeSinceVibStop <- dfp$RunningTime - dfp$VibStopTime


dfp$SpeedDiffFromStart <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStart
dfp$SpeedDiffFromStop <- dfp$rollingSpeedMedian - dfp$SpeedAtVibStop
