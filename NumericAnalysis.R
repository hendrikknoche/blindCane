#------------------------------NumericTestValues------------------------------#

# Only Baseline data
dataBaseline <- df %>%
  filter(Range == 1) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))


# Only WholeRoom data
dataWholeRoom2m <- df %>%
  filter(Range == 2) %>%
  group_by(FOD = "WholeRoom", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

dataWholeRoom3m <- df %>%
  filter(Range == 3) %>%
  group_by(FOD = "WholeRoom", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

dataWholeRoom4m <- df %>%
  filter(Range == 4) %>%
  group_by(FOD = "WholeRoom", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))


# Only Corridor data
dataCorridor2m <- df %>%
  filter(Range == 2) %>%
  group_by(FOD = "Corridor", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

dataCorridor3m <- df %>%
  filter(Range == 3) %>%
  group_by(FOD = "Corridor", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

dataCorridor4m <- df %>%
  filter(Range == 4) %>%
  group_by(FOD = "Corridor", Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))


dataWhole <- daggByScen %>%
  group_by(FOD, Range) %>%
  summarize(objectCollisions = mean(objectCollisions, na.rm = TRUE), objectDetected = mean(objectDetected, na.rm = TRUE), Time = mean(Time * 1000))

test<-daggByScen %>%
  group_by(FOD, Range) %>%
  summarize(objectCollisions = mean(objectCollisions, na.rm = TRUE), objectDetected = mean(objectDetected, na.rm = TRUE), Time = mean(Time * 1000))


# Corridor vs WholeRoom data
CorrVsWR2m <- df %>%
  filter(Range == 2) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

CorrVsWR3m <- df %>%
  filter(Range == 3) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

CorrVsWR4m <- df %>%
  filter(Range == 4) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

daggByScenWOBL<-daggByScen[!daggByScen$FOD=="Baseline",]
m<-lm(Time~FOD*Range+totalTimeTraining,data=daggByScen[!daggByScen$FOD=="Baseline",])
summary(m)
model_equation(m)
daggByScenWOBL$predTime<-predict(m)


# BaseLine, Corridor and WholeRoom data
BVsCorrVsWHoleRoom2m <- df %>%
  filter(Range == 1 | Range == 2) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

BVsCorrVsWHoleRoom3m <- df %>%
  filter(Range == 1 | Range == 3) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))

BVsCorrVsWHoleRoom4m <- df %>%
  filter(Range == 1 | Range == 4) %>%
  group_by(FOD, Scenario) %>%
  summarize(objectCollisions = sum(objColl, na.rm = TRUE), objectDetected = sum(objDet, na.rm = TRUE), Range = mean(Range), Time = max(Time_in_MS * 1000))





oneway.test(Scenario ~ objectCollisions, data = dataWholeRoom2m)

# The difference between collisions per scenario (WholeRoom)
anova(lm(Scenario ~ objectCollisions, data = dataWholeRoom2m))

anova(lm(Scenario ~ objectCollisions, data = dataWholeRoom3m))

anova(lm(Scenario ~ objectCollisions, data = dataWholeRoom4m))


# The difference between collisions per scenario (Corridor)
anova(lm(Scenario ~ objectCollisions, data = dataCorridor2m))

anova(lm(Scenario ~ objectCollisions, data = dataCorridor3m))

anova(lm(Scenario ~ objectCollisions, data = dataCorridor4m))




# Corridor vs WholeRoom - difference between collisions per scenario
anova(lm(Scenario ~ objectCollisions, data = CorrVsWR2m))

anova(lm(Scenario ~ objectCollisions, data = CorrVsWR3m))

anova(lm(Scenario ~ objectCollisions, data = CorrVsWR4m))



# Perfect fit?
anova(lm(Range ~ objectCollisions, data = CorrVsWR2m))

anova(lm(Range ~ objectCollisions, data = CorrVsWR3m))

anova(lm(Range ~ objectCollisions, data = CorrVsWR4m))



anova(lm(Scenario ~ objectCollisions, data = BVsCorrVsWHoleRoom2m))
