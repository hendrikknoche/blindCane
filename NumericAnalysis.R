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
daggByScenWOBLC<-daggByScen[!daggByScen$FOD=="Corridor",]
daggByScenWOBLW<-daggByScen[!daggByScen$FOD=="WholeRoom",]

m1<-lm(log(Time)~FOD*Range+totalTimeTraining,data=daggByScen)

m2<-lm(log(Time)~FOD*log(Range)+log(totalTimeTraining),data=daggByScen)
m0<-lm(Time~log(totalTimeTraining)+FOD*Range,data=daggByScenWOBL)
summary(m0)

#Splitting data up into traning time for whole room and corridor and below, baseline. 
#This yields two models, to compare against each other.
mwrcDat <-lm(log(Time)~log(totalTimeTraining)+FOD*Range,data=daggByScenWOBL)
summary(mwrcDat)
model_equation(mwrcDat)

#-DATA SPLIT STARTS HERE-------------------#
baseDat <- daggByScen[daggByScen$FOD=="Baseline",]
mbaseDat <- lm(log(Time)~log(totalTimeTraining),data=baseDat)

model_equation(mbaseDat)
coef(lm(Time ~ totalTimeTraining, data = baseDat))



wrDat <- daggByScen[daggByScen$FOD=="WholeRoom",]
wr2Dat <- wrDat[wrDat$Range=="2",]
wr3Dat <- wrDat[wrDat$Range=="3",]
wr4Dat <- wrDat[wrDat$Range=="4",]
mwrDat<- lm(log(Time)~log(totalTimeTraining),data=wrDat)

model_equation(wrDat)
coef(lm(Time ~ totalTimeTraining, data = wrDat))



corrDat <- daggByScen[daggByScen$FOD=="Corridor",]
corr2Dat <- corrDat[corrDat$Range=="2",]
corr3Dat <- corrDat[corrDat$Range=="3",]
corr4Dat <- corrDat[corrDat$Range=="4",]
mcorrDat<- lm(log(Time)~log(totalTimeTraining),data=corrDat)



summary(nls(Time ~ a+b*totalTimeTraining^z,
    start = list(b = -0.1, z = 0.44, a=19),
    data = baseDat))


nls(density ~ 1/(1 + exp((xmid - log(conc))/scal)),
    data = DNase1,)


model_equation(mcorrDat)
coef(lm(Time ~ totalTimeTraining, data = corrDat))


a<-18.1559
b=-0.1073
z=0.4624
dxy<- a*dxx^b
plot(dxx,dxy)


#BaselineDat
summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = baseDat))


#CorridorDat
summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = corrDat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = corr2Dat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = corr3Dat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = corr4Dat))


#WholeRoomDat
summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = wrDat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = wr2Dat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = wr3Dat))

summary(nls(Time ~ b*totalTimeTraining^z,
            start = list(b = 1, z = 1),
            data = wr4Dat))





totalTimeTraining=1:6000

#Set to summary variables 
b=54.86090
z=-0.16613


#This is the Time fed into dxf
Time<- b*totalTimeTraining^z
#plot(dxx,dxy)
#Make new datafram for each dataset
dxf=as.data.frame(totalTimeTraining)

baseDatxf=as.data.frame(totalTimeTraining)

corr2Datxf=as.data.frame(totalTimeTraining)
corr3Datxf=as.data.frame(totalTimeTraining)
corr4Datxf=as.data.frame(totalTimeTraining)

wr2Datxf=as.data.frame(totalTimeTraining)
wr3Datxf=as.data.frame(totalTimeTraining)
wr4Datxf=as.data.frame(totalTimeTraining)
#Bind the Time to the dataset
dxf=cbind(dxf,Time)

baseDatxf=cbind(baseDatxf,Time)

corr2Datxf=cbind(corr2Datxf,Time)
corr3Datxf=cbind(corr3Datxf,Time)
corr4Datxf=cbind(corr4Datxf,Time)

wr2Datxf=cbind(wr2Datxf,Time)
wr3Datxf=cbind(wr3Datxf,Time)
wr4Datxf=cbind(wr4Datxf,Time)


#Adding labels of FOD (change according to FOD and)
FOD = "WholeRoom"
Range = "3"

wr4Datxf=cbind(wr4Datxf,FOD)
corr2Datxf=cbind(corr2Datxf,Range)
corr3Datxf=cbind(corr3Datxf,Range)
#Now feed the "dxf" (or what its called for the others) into a geom_line in the Analysis ggplot
#ggplot(aes(dxx,dxy))


b=NULL
z=NULL
rm(totalTimeTraining, Time)


a<-15

plot(nls(Time ~ b*totalTimeTraining^z,
         start = list(b = 29.78803, z = -0.10012),
         data = corrDat))

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
anova(lm(Scenario ~ objectCollisions, data = corr2Dat))

anova(lm(Scenario ~ objectCollisions, data = CorrVsWR3m))

anova(lm(Scenario ~ objectCollisions, data = CorrVsWR4m))



# Perfect fit?
anova(lm(Range ~ objectCollisions, data = CorrVsWR2m))

anova(lm(Range ~ objectCollisions, data = CorrVsWR3m))

anova(lm(Range ~ objectCollisions, data = CorrVsWR4m))



anova(lm(Scenario ~ objectCollisions, data = BVsCorrVsWHoleRoom2m))



#--------------------------------Actual usefull numbers--------------------------------

#Understanding the data
plot(density(daggByScen$avgSpeed))
plot(density(daggByScen$Time))
plot(density(daggByScen$totalTimeTraining))
plot(density(daggByScen$avgSpeed))


lm(log(Time)~FOD*Range,data=daggByScen)

plot(lm(log(Time)~FOD*Range,data=daggByScen))

plot(lm(log(Time)~FOD*Range+log(Time),data=daggByScen))

plot(lm(log(Time)~FOD*Range+totalTimeTraining,data=daggByScen))



#BaseDat cannot because only one length
anova(lm(Range ~ objectCollisions ,data=baseDat))

summary(glm(objectCollisions ~ Range*FOD +totalTimeTraining ,family="poisson",data=daggByScen))
summary(glm(objectDetected ~ Range*FOD +totalTimeTraining ,family="poisson",data=daggByScen))

summary(glm(objectCollisions ~ Range +totalTimeTraining,family="poisson",data=corrDat))
summary(glm(objectCollisions ~ Range +totalTimeTraining,family="poisson",data=wrDat))
summary(glm(objectDetected ~ Range+totalTimeTraining  ,family="poisson",data=corrDat))
summary(glm(objectDetected ~ Range+totalTimeTraining ,family="poisson",data=wrDat))





#the below should not work
anova(lm(objectCollisions ~  Range,data=wrDat))

#Therefore, basedat has to be compared to the others

anova(lm(Range ~ objectCollisions ,data=daggByScen))




