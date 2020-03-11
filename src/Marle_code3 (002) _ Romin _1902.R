# Multinomial prediction model - assessment of performance (1) BOOTSTRAP OPTIMISM-CORRECTED performance
# R PAJOUHESHNIA
# 05-12-19

# 1) calculate the Polytomous discrimination index, PDI, which is a correct version of the AUC for multinomial models.
# The PDI is an estimator for the probability of correctly identifying a randomly selected case 
# in a set of cases consisting of one case from each outcome category
# note, PDI=1 is perfect discrimination ; 
# !!! PDI 1/j (where j= number of outcome categories, in your case, j=3) is no discrimination !!!
# So PDI = 0.5 is not so bad! Important difference in interpretation.

# I also internally validate this by using bootstrap optimism correction of the PDI (AUC). This 
# accounts for potential "overfitting" of the model in the single data set. The difference
# between the unadjusted and adjusted PDIs gives some indication of how much the discriminative ability of
# the model will change if you apply it in a new (similar!) group of patients

# 2) Calibration plots for multinomial
# !!! Take the *parametric* plot !!!

# First make sure both your data and the R function code files are in your working directory.
# install.packages("bayesm") #if not already installed
library(bayesm)
library(VGAM)
library(nnet)
library(mcca)
setwd("U:/Database study/R")

source("VanHoorde_functions.R")
source("DeJong_functions.R")

DBI.180<-read.csv2("DBI.180.FRIDS2_2.csv")

ithr <-0.9999999999999
DBI.180$DBI.3cat <- as.factor( cut( DBI.180$DBI.calculated, c(-1,0,ithr, max(DBI.180$DBI.calculated,na.rm=T)) ) )

# 1) calculate unadjusted PDI (AUC):
model.180 <- multinom(as.factor(val) ~ DBI.3cat +leeftijd+ geslacht , data=DBI.180, Hess=TRUE)
model.180.cont <- multinom(as.factor(val) ~ bs(DBI.calculated) +leeftijd+ geslacht , data=DBI.180, Hess=TRUE)
model.agesex <- multinom(as.factor(val) ~ leeftijd+ geslacht , data=DBI.180, Hess=TRUE)

outcome.180<-DBI.180$val  

p.180 <- predict(model.180, type="prob")
p.180.cont <- predict(model.180.cont, type="prob")
p.180.agesex <- predict(model.agesex, type="prob")

PDI.unadjusted = round(PdiDF(outcome.180, p.180, cats = c(0,1,2), method_name = "unnamed")[4],3)
PDI.unadjusted.cont = round(PdiDF(outcome.180, p.180.cont, cats = c(0,1,2), method_name = "unnamed")[4],3)
PDI.agesex = round(PdiDF(outcome.180, p.180.agesex, cats = c(0,1,2), method_name = "unnamed")[4],3)

PDI.unadjusted
PDI.unadjusted.cont
PDI.agesex



####################
# 2) CALIBRATION PLOTS (not corrected for optimism... coding is more complex and needs more time!)
#get information needed for performance measures
p.180 <- predict(model.180, type="prob")
LP.180 <- fitted(multinom(model.180))[,c(2,3)]
Polcal(outcome.180, 3, p.180, LP.180, 1, plotoverall=TRUE, datapoints=TRUE)
# Look at the parametric calibration plot


#############################
#3) Calculate optimism-corrected PDI (AUC) (this takes a minute or two):
PDI.vect<- c() #to store PDI values from loop below

for (i in 1: 500){ 
  S <- DBI.180[sample(nrow(DBI.180), replace=T),]
  model <- multinom(as.factor(val) ~ DBI.3cat +leeftijd+ geslacht , data=S, Hess=TRUE)
  p<- predict(model, newdata=DBI.180, type="prob")
  PDI.vect[i]<-round(PdiDF(outcome.180, p, cats = c(0,1,2), method_name = "unnamed")[4],3)
}

PDI.vect <-unlist(PDI.vect) #put in nice format
optimism = mean(PDI.vect - unlist(PDI.unadjusted))
optimism
corrected = round(PDI.unadjusted- abs(optimism), 3)
PDI.unadjusted
corrected # remember with this "special AUC" for multinomial models, 0.33 is a model with no
#discrimination (not 0.5!). 0.41 is not very good though...



###### confidence interval
gc()

memory.limit(size = 10000)
ests(outcome.180, p.180.agesex, acc="pdi", level=0.95, method="multinom", B=1000, balance=FALSE)


########## PDI for agesex model

########################################################### FRIDS model

DBI.180.FRIDS2[1]<-NULL
DBI.180.FRIDS2<-unique(DBI.180.FRIDS2)


ithr <-0.9999999999999
DBI.180.FRIDS2$DBI.3cat <- as.factor( cut( DBI.180.FRIDS2$DBI.calculated, c(-1,0,ithr, max(DBI.180.FRIDS2$DBI.calculated,na.rm=T)) ) )

model.FRIDS <- multinom(as.factor(val) ~ leeftijd+ geslacht + loopdiuretics 
                        + TCA + SSRI + benzodiazepines + antiepileptica + nitrates  + digoxin 
                        + thiazides + aldosteronantagonist + calciumchannelblocker + RAAS
                        + alfablokkers + opiaten + tramadol + codeine + insulin + sulfonylurea
                        + statines + urinaryspasmodics + antivertigo + antihistaminics + NSAIDs
                        + dypridamole, data=DBI.180.FRIDS2, Hess=TRUE)


# 1) calculate unadjusted PDI (AUC):
model.FRIDS <- multinom(as.factor(val) ~ leeftijd+ geslacht + loopdiuretics 
                        + TCA + SSRI + benzodiazepines + antiepileptica + nitrates  + digoxin 
                        + thiazides + aldosteronantagonist + calciumchannelblocker + RAAS
                        + alfablokkers + opiaten + tramadol + codeine + insulin + sulfonylurea
                        + statines + urinaryspasmodics + antivertigo + antihistaminics + NSAIDs
                        + dypridamole, data=S, Hess=TRUE)
summary(model.FRIDS)
exp(coef(model.FRIDS))
exp(confint(model.FRIDS))

outcome.180.FRIDS<-DBI.180.FRIDS2$val  

p.180.FRIDS <- predict(model.FRIDS, type="prob")

PDI.unadjusted = round(PdiDF(outcome.180.FRIDS, p.180.FRIDS, cats = c(0,1,2), method_name = "unnamed")[4],3)


####################
# 2) CALIBRATION PLOTS (not corrected for optimism... coding is more complex and needs more time!)
#get information needed for performance measures
p.180.FRIDS <- predict(model.FRIDS, type="prob")
LP.180.FRIDS <- fitted(multinom(model.FRIDS))[,c(2,3)]
Polcal(outcome.180.FRIDS, 3, p.180.FRIDS, LP.180.FRIDS, 1, plotoverall=TRUE, datapoints=TRUE)
# Look at the parametric calibration plot


#############################
#3) Calculate optimism-corrected PDI (AUC) (this takes a minute or two):
PDI.vect<- c() #to store PDI values from loop below

for (i in 1: 500){ 
  S <- DBI.180.FRIDS2[sample(nrow(DBI.180.FRIDS2), replace=T),]
  model <- multinom(as.factor(val) ~ leeftijd+ geslacht + loopdiuretics 
                    + TCA + SSRI + benzodiazepines + antiepileptica + nitrates  + digoxin 
                    + thiazides + aldosteronantagonist + calciumchannelblocker + RAAS
                    + alfablokkers + opiaten + tramadol + codeine + insulin + sulfonylurea
                    + statines + urinaryspasmodics + antivertigo + antihistaminics + NSAIDs
                    + dypridamole, data=S, Hess=TRUE)
  p<- predict(model, newdata=DBI.180.FRIDS2, type="prob")
  PDI.vect[i]<-round(PdiDF(outcome.180.FRIDS, p, cats = c(0,1,2), method_name = "unnamed")[4],3)
}

PDI.vect <-unlist(PDI.vect) #put in nice format
optimism = mean(PDI.vect - unlist(PDI.unadjusted))
optimism
corrected = round(PDI.unadjusted- abs(optimism), 3)
PDI.unadjusted
corrected # remember with this "special AUC" for multinomial models, 0.33 is a model with no
#discrimination (not 0.5!). 0.41 is not very good though...



PDI.vect<- c() #to store PDI values from loop below

for (i in 1: 500){ 
  S <- DBI.180.FRIDS2[sample(nrow(DBI.180.FRIDS2), replace=T),]
  model <- multinom(as.factor(val) ~ leeftijd+ geslacht , data=S, Hess=TRUE)
  p<- predict(model, newdata=DBI.180.FRIDS2, type="prob")
  PDI.vect[i]<-round(PdiDF(outcome.180.FRIDS, p, cats = c(0,1,2), method_name = "unnamed")[4],3)
}

PDI.vect <-unlist(PDI.vect) #put in nice format
optimism = mean(PDI.vect - unlist(PDI.agesex))
optimism
corrected = round(PDI.agesex- abs(optimism), 3)
PDI.agesex
corrected # remember with this "special AUC" for multinomial models, 0.33 is a model with no
#discrimination (not 0.5!). 0.41 is not very good though...


ests(outcome.180.FRIDS, p.180.FRIDS, acc="pdi", level=0.95, method="multinom", B=250, balance=TRUE)
