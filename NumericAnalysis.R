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
m1<-lm(log(Time)~FOD*Range+totalTimeTraining,data=daggByScen)

m2<-lm(log(Time)~FOD*log(Range)+log(totalTimeTraining),data=daggByScen)
m0<-lm(Time~log(totalTimeTraining)+FOD*Range,data=daggByScenWOBL)
summary(m0)

#Splitting data up into traning time for whole room and corridor and below, baseline. 
#This yields two models, to compare against each other.
mwrcDat <-lm(log(Time)~log(totalTimeTraining)+FOD*Range,data=daggByScenWOBL)
summary(mwrcDat)
model_equation(mwrcDat)

baseDat <- daggByScen[daggByScen$FOD=="Baseline",]
mbaseDat <- lm(log(Time)~log(totalTimeTraining),data=baseDat)

model_equation(mbaseDat)
coef(lm(Time ~ totalTimeTraining, data = baseDat))



wrDat <- daggByScen[daggByScen$FOD=="WholeRoom",]
mwrDat<- lm(log(Time)~log(totalTimeTraining),data=wrDat)

model_equation(wrDat)
coef(lm(Time ~ totalTimeTraining, data = wrDat))



summary(nls(Time ~ a+b*totalTimeTraining^z,
    start = list(b = -0.1, z = 0.44, a=19),
    data = baseDat))


nls(density ~ 1/(1 + exp((xmid - log(conc))/scal)),
    data = DNase1,)




corrDat <- daggByScen[daggByScen$FOD=="Corridor",]
mcorrDat<- lm(log(Time)~log(totalTimeTraining),data=corrDat)

model_equation(corrDat)
coef(lm(Time ~ totalTimeTraining, data = corrDat))


a<-18.1559
b=-0.1073
z=0.4624
dxy<- a*dxx^b
plot(dxx,dxy)

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = corrDat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = wrDat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = baseDat))

dxx=1200:5000
a<-15
b=29
z=-0.10
dxy<- b*dxx^z
ggplot(aes(dxx,dxy))





summary(anova(m1,m2))

summary(m1)
summary(m2)
model_equation(m)
daggByScenWOBL$predTime<-predict(m)
step(m)




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
