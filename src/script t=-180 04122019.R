#library(dplyr)
#library(MASS)


setwd("U:/Database study/R")

data.medhis <- read.csv(file= "S:/data/kopie/medhis compleet anoniem.csv", 
                        sep = ";", dec = ".", header = FALSE)

colnames(data.medhis, do.NULL = FALSE, prefix = "col")    # kolomnamen v1 t/m v13 gegeven

wrk.data.medhis <- data.medhis     # werkbestand gemaakt van de data

names(wrk.data.medhis)[1] <-"PatientID"
names(wrk.data.medhis)[2] <-"FRID"
names(wrk.data.medhis)[3] <-"ATC"
names(wrk.data.medhis)[4] <- "afleverdatum"
names(wrk.data.medhis)[5] <- "dosering"
names(wrk.data.medhis)[6] <- "een"
names(wrk.data.medhis)[7] <- "daggebruik"
names(wrk.data.medhis)[8] <- "geslacht"
names(wrk.data.medhis)[9] <- "hoeveelheid"
names(wrk.data.medhis)[10] <- "preparaatnaam"
names(wrk.data.medhis)[11] <- "indexdatum"
names(wrk.data.medhis)[12] <- "periode"
names(wrk.data.medhis)[13] <- "leeftijd"                 # kolommen namen geven; let op, de volgorde van de kolommen is anders dan in het ACCESS bestand


patientenmatrix <- read.csv(file= "S:/data/Copy of patientenmatrix.csv", 
                                    sep = ";", dec = ".", header = TRUE)
wrk.patientenmatrix <- patientenmatrix

valgegevens <- wrk.patientenmatrix[c(1,11,12)]      # enkel patient ID en valgegevens


################################# koppelen van de twee databases aan de hand van patientID

koppel <- merge(wrk.data.medhis, valgegevens, by="PatientID") 


################## koppelen van de database met DDD info

DDD <- read.csv2(file = "S:/data/kopie/preparaat.csv")
names(DDD)[1] <- "preparaatnaam"
DDD[4:5] <- NULL
DDD[2] <-NULL

koppel2 <- merge(koppel, DDD, by="preparaatnaam")    ##### combi-bestand

names(koppel2)[16] <- "hDDD"       ######### DDD aangepast in hDDD

wrk.data <- subset(koppel2, koppel2$gevallen < 2)   # subset bestand, alleen de valgegevens 0 en 1

wrkdata1 <- subset(wrk.data, wrk.data$periode < -180)             #periode kiezen
wrkdata1 <- subset(wrkdata1, wrkdata1$periode > -270)

wrkdata2 <- subset(wrkdata1, wrkdata1$ATC != "")   # alles zonder ATC weg



########################################################################

pat.subset <- wrk.data2[wrkdata2$PatientID == 6, ]

table.atc <- table(as.character(pat.subset$ATC))

atc.meer.dan.1 <- table.atc[table.atc > 1]

atc.naam <- names(atc.meer.dan.1)

#table.atc[4]
#max(pat.subset$periode)

for(i in 1:length(atc.naam)){
  naam <- atc.naam[i]
  pat.data.atcselectie <- pat.subset[pat.subset$ATC == naam, ]
  max.periode <- max(pat.data.atcselectie$periode)
  pat.subset <- pat.subset[pat.subset$ATC!= naam | (pat.subset$ATC == naam & pat.subset$periode == max.periode), ]
}






for (nummer in 1:10){
  print(nummer)
}

rm(datanieuw)
for (patientnummer in unique(wrkdata2$PatientID)){
  pat.subset <- wrkdata2[wrkdata2$PatientID == patientnummer, ]
  table.atc <- table(as.character(pat.subset$ATC))
  
  atc.meer.dan.1 <- table.atc[table.atc > 1]
  
  atc.naam <- names(atc.meer.dan.1)
  
  
  #table.atc[4]
  #max(pat.subset$periode)
  if(length(atc.naam > 0)){
    for(i in 1:length(atc.naam)){
      #zoek op welke ATC codes m,eer dan 1 x voorkomen, zoek laatste datum, Gooi rest weg. 
      naam <- atc.naam[i]
      pat.data.atcselectie <- pat.subset[pat.subset$ATC == naam, ]
      max.periode <- max(pat.data.atcselectie$periode)
      pat.subset <- pat.subset[pat.subset$ATC!= naam | (pat.subset$ATC == naam & pat.subset$periode == max.periode), ]
    }
  }
  
  #opslaan in data.nieuw
  if (exists("datanieuw")){
    # data.nieuw bestaat al
    datanieuw <- rbind(datanieuw, pat.subset)
  } else {
    #data.nieuw bestaat nog niet
    datanieuw <- pat.subset
  }
}

########### de onjuiste daggebruiken vervangen

#library(dplyr)

subdata.new.onjuistgebruik <- filter(datanieuw, daggebruik == "0,00") # de gegevens waar daggebruik 0 is
subdata.new.onjuistgebruik2 <- datanieuw[is.na(datanieuw$daggebruik),] # de gegevens waar daggebruik NA is

subdata.new.onjuistgebruik <- rbind(subdata.new.onjuistgebruik, subdata.new.onjuistgebruik2)

subdata.new.onjuistgebruik[8] <- NULL    # column daggebruik verwijderd

gekke.daggebruiken <- read.csv2(file = "S:/data/dosoms_marle_03052019_2.csv")      # dit aaanpassen naar: dosoms_marle_03052019_2

gekke.daggebruiken[2] <- NULL
names(gekke.daggebruiken)[1] <- "dosering"
gekke.daggebruiken$daggebruik <- gekke.daggebruiken$PERDAG/1000
gekke.daggebruiken <- gekke.daggebruiken[c(1,3)]


subdata.new.juistgebruik <- merge(subdata.new.onjuistgebruik,gekke.daggebruiken, by = "dosering", all.x = TRUE)

subdata.new.juistgebruik2 <- datanieuw %>%
  filter(daggebruik != '0,00' & !is.na(daggebruik))  # de gegevens waar daggebruik niet 0 is
subdata.new.juistgebruik$daggebruik <- as.factor(subdata.new.juistgebruik$daggebruik)

subdata.new.juistgebruik3 <- rbind(subdata.new.juistgebruik,subdata.new.juistgebruik2)

write.csv2(subdata.new.juistgebruik3, "S:/data/subdata.new.juistgebruik3.csv") ######### de daggebruiken invullem + omzetten DDD


######### de daggebruiken invullen in excel + omzetten DDD
###### +
##### finding double ATC per patient +
############## de ATC codes van parac/codein die niet goed zijn (N02BE71) naar juiste (N02AJ06)

pat.subset2 <- DBI.werk[DBI.werk$PatientID == 5520, ]

table.atc <- table(as.character(pat.subset2$ATC.x))

atc.meer.dan.1 <- table.atc[table.atc > 1]

atc.naam <- names(atc.meer.dan.1)




for (patientnummer in unique(DBI.werk$PatientID)){    # ga elke keer langs patient ID
  pat.subset2 <-DBI.werk[DBI.werk$PatientID == patientnummer, ]  # maak subset van patientID   
  table.atc <- table(as.character(pat.subset2$ATC.x))
  
  atc.meer.dan.1 <- table.atc[table.atc > 1]
  
  atc.naam <- names(atc.meer.dan.1)
  
  if(length(atc.naam > 0)){
    
    for(i in 1:length(atc.naam)){
      
      naam <- atc.naam[i]
      pat.data.atcselectie <- pat.subset2[pat.subset2$ATC.x == naam, ]
    }
  }
  
  #opslaan in patientIDSdubbel
  if (exists("patientIDSdubbel2")){
    # patientIDSdubbel bestaat al
    patientIDSdubbel2 <- rbind(patientIDSdubbel2, pat.data.atcselectie$PatientID)
  } else {
    #patientIDSdubbel bestaat nog niet
    patientIDSdubbel2 <- pat.data.atcselectie$PatientID
  }
}


doublepatientIDS2 <- unique(patientIDSdubbel2$PatientID)

print(doublepatientIDS2) 

###################################################### uploaden uit excel

wrkdata.inclDDD <- read.csv2("S:/data/wrkdata.inclDDD.aangepast.csv") #### dit weer uploaden uit excel

DBI.etiketnaam <- read.csv(file = "S:/data/DBI.etiketnaam.csv", header = T, sep = ";", dec = ",")


names(wrkdata.inclDDD)[2] <- "PREPARAATN"

DBI.werk <- merge(wrkdata.inclDDD, DBI.etiketnaam, by="PREPARAATN", all.x = T)
DBI.werk$DDD.y <- DBI.werk$DDD.y/1000



DBI.werk <- DBI.werk %>% filter(PatientID != '0' & !is.na(PatientID))


DBI.werk2 <- DBI.werk %>% mutate(X = DDD.y * DDD.x)
DBI.werk2$daggebruik <- 
  gsub(",", ".", as.character(DBI.werk2$daggebruik))
DBI.werk2$daggebruik <- as.numeric(DBI.werk2$daggebruik)
DBI.werk2$D <- DBI.werk2$daggebruik * DBI.werk2$X


DBI.werk3 <- DBI.werk2 %>% mutate(DBI= D / (D + Minimale.DD))

DBI.werk4 <- DBI.werk3[c(3,8,12,1,5,9,7,2,16,11,13,14,44)]

#DBI.werk4 <- DBI.werk3 %>% select(PatientID, geslacht, leeftijd, PREPARAATN, ATC.x, hoeveelheid, een, dosering, daggebruik, periode, gevallen, MeerVal, val, DBI)

DBI.calc <- DBI.werk4 %>% group_by(PatientID) %>% summarise(DBI.calculated = sum(DBI, na.rm=TRUE))

DBI.werk5 <- merge(DBI.werk4, DBI.calc, by="PatientID")    

#DBI.werk6 <- DBI.werk5 %>% select(PatientID,geslacht,leeftijd,gevallen, MeerVal, val, DBI.calculated)
DBI.werk6 <- DBI.werk5[c(1,2,3,11,12,14)]

#valgegevens.2 <- DBI.180.FRIDS2[c(2,7)]
#DBI.werk6 <- merge(valgegevens.2, DBI.werk6)

DBI.werk7 <- DBI.werk6[!duplicated(DBI.werk6$PatientID),] 


########## gemiddelde etc

library(dplyr)

DBI.180 <- DBI.werk7

DBI.180 %>% group_by(val) %>% summarise(n = n())
DBI.180 %>% group_by(geslacht) %>% summarise(n = n())
subset(DBI.180, DBI.180$geslacht == "V") %>% group_by(val) %>% summarise(n = n())
subset(DBI.180, DBI.180$geslacht == "M") %>% group_by(val) %>% summarise(n = n())


DBI.180 %>% summarise (leeftijd = mean(leeftijd))

DBI.180 %>% group_by(gevallen) %>% summarise(mean = mean(DBI.calculated))
DBI.180 %>% group_by(MeerVal) %>% summarise(mean = mean(DBI.calculated))


DBI.180 %>% group_by(gevallen) %>% summarise(count = n())
DBI.180 %>% group_by(MeerVal) %>% summarise(count = n())

hist(DBI.180$DBI.calculated,n=50, main="DBI distribution", col="grey", xlab="DBI", xlim=c(-0.1,6), ylab="Frequency", freq = T)


hist(DBI.180$DBI.calculated,n=50, main="DBI distribution over study population", col="grey", xlab="DBI", xlim=c(-0.1,6), ylab="Number of patients", freq = T)
hist(DBI.180$DBI.calculated[DBI.180$val == 0],n=50, main="DBI distribution in non-fallers", col=rgb(1,0,0,0.5), xlab="DBI", xlim=c(-0.1,6), ylab="Number of patients", freq = T)
hist(DBI.180$DBI.calculated[DBI.180$val == 1],n=50, main="DBI distribution in single fallers", col=rgb(1,0,1,0.5), xlab="DBI", xlim=c(-0.1,6), ylab="Number of patients", freq = T)
hist(DBI.180$DBI.calculated[DBI.180$val == 2],n=50, main="DBI distribution in recurrent fallers", col=rgb(0,1,0,0.5), xlab="DBI", xlim=c(-0.1,6), ylab="Number of patients", freq = T)

median(DBI.180$DBI.calculated)
quantile(DBI.180$DBI.calculated, 0.25)
quantile(DBI.180$DBI.calculated, 0.75)

median(DBI.180$DBI.calculated[DBI.180$val == 0])
quantile(DBI.180$DBI.calculated[DBI.180$val == 0], 0.25)
quantile(DBI.180$DBI.calculated[DBI.180$val == 0], 0.75)

median(DBI.180$DBI.calculated[DBI.180$val == 1])
quantile(DBI.180$DBI.calculated[DBI.180$val == 1], 0.25)
quantile(DBI.180$DBI.calculated[DBI.180$val == 1], 0.75)

median(DBI.180$DBI.calculated[DBI.180$val == 2])
quantile(DBI.180$DBI.calculated[DBI.180$val == 2], 0.25)
quantile(DBI.180$DBI.calculated[DBI.180$val == 2], 0.75)


######### sv



DBI.180$DBI.3cat <- as.factor( cut( MF.DBI$DBI.calculated, c(-1,0,1, max(DBI.180$DBI,na.rm=T)) ) )

as.factor( cut( MF.DBI$DBI.calculated, c(-1,0,1, 1000) ) )

model.test <- glm(gevallen~ factor( cut( MF.DBI$DBI.calculated, c(-1,0,1, 1000) ) ) + leeftijd+geslacht, family="binomial", data=MF.DBI)

model.test <- glm(MeerVal~ I( as.factor( cut( MF.DBI$DBI.calculated, c(-1,0,1, 1000) ) ) )  + leeftijd+geslacht, family="binomial", data=MF.DBI)

exp(coef(model.test))
exp(confint(model.test))


DBI.180$DBI.2cat <- as.factor( cut( MF.DBI$DBI.calculated, c(-1,0, max(MF.DBI$DBI.calculated,na.rm=T)) ) )

model.DBI.2cat <- glm(MeerVal~ DBI.2cat + leeftijd+geslacht, family="binomial", data=DBI.180)
model.DBI.2cat$coef["DBI.2cat(0,5.44]"]


######## model multiple falls

ithr <- 1.49999999

thr<-c(0.4999999,0.999999,1.099999,1.199999,1.299999,1.399999,1.499999,1.599999,1.6999999, 1.999999, 2.499999, 2.999999)
DBI.3cat.2.1 <- DBI.3cat.3.1 <- c()
for(ithr in thr){
  DBI.180$DBI.3cat <- as.factor( cut( DBI.180$DBI.calculated, c(-1,0,ithr, max(DBI.180$DBI.calculated,na.rm=T)) ) )
  model.DBI180 <- glm(MeerVal~ DBI.3cat + leeftijd+geslacht, family="binomial", data=DBI.180)
  
  
  DBI.3cat.2.1 <- c(DBI.3cat.2.1, exp(coef(model.DBI180))[paste0("DBI.3cat(0,",round(ithr,2), "]")])
  #DBI.3cat.3.1 <- c(DBI.3cat.3.1, exp(coef(model.DBI))[paste0("DBI.3cat(",ithr,",5.44]")])
  DBI.3cat.3.1 <- c(DBI.3cat.3.1, exp(coef(model.DBI180))[paste0("DBI.3cat(",round(ithr,2),",",round(max(DBI.180$DBI.calculated,na.rm=T),2),"]")])
  
  
  
}

DBI.3cat.2.1
DBI.3cat.3.1

exp(coef(model.DBI180))
exp(confint(model.DBI180))


#############################

library(nnet)

ithr <-0.99999999999999

DBI.180$DBI.3cat <- as.factor( cut( DBI.180$DBI.calculated, c(-1,0,ithr, max(DBI.180$DBI.calculated,na.rm=T)) ) )

model.180 <- multinom(as.factor(val) ~ DBI.3cat +leeftijd+ geslacht , data=DBI.180, Hess=TRUE)

summary(model.180)
exp(coef(model.180))
exp(confint(model.180))



#### counting patients
subset(DBI.180, DBI.180$val == 2) %>% group_by(DBI.3cat) %>% summarise(aantal = n())

####### advies Romin zie e-mail
library(pROC)

ithr <- 1.9999999999999999999999

DBI.180$DBI.3cat <- as.factor( cut( DBI.180$DBI.calculated, c(-1,0,ithr, max(DBI.180$DBI.calculated,na.rm=T)) ) )



model.180 <- multinom(as.factor(val) ~ DBI.3cat +leeftijd+ geslacht , data=DBI.180, Hess=TRUE)

summary(model.180)
exp(coef(model.180))
exp(confint(model.180))


X <- predict(model.180, newdata = DBI.180, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- data.frame(X)

DBI.180 <- transform(DBI.180, ONE_VS_OTHER=ifelse(val=="1", "1", "0"))
DBI.180 <- transform(DBI.180, MORE_VS_OTHER=ifelse(val=="2", "1", "0"))


roc.onevsother <- roc(DBI.180$ONE_VS_OTHER, X$X1, plot=TRUE)
roc.morevsother <- roc(DBI.180$MORE_VS_OTHER, X$X2, plot=TRUE)

ci(roc.onevsother)
ci(roc.morevsother)
roc.onevsother
roc.morevsother



############ univariate analyses geneesmiddelen

wrkdata.univariate <- wrkdata.inclDDD[c(1,2,3,5,8,12,18)]

wrkdata.univariate.loops <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C03CA01" | wrkdata.univariate$ATC == "C03CA02"),"loopdiuretics")
wrkdata.univariate.loops[4:7] <- NULL
wrkdata.univariate.loops[1:2] <- NULL

DBI.180.FRIDS <- DBI.180
DBI.180.FRIDS[8:10] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.loops, by= "PatientID", all.x = TRUE)

wrkdata.univariate.TCA <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N06AA02" | wrkdata.univariate$ATC == "N06AA04" | wrkdata.univariate$ATC == "N06AA09" | wrkdata.univariate$ATC == "N06AA10" | wrkdata.univariate$ATC == "N06AA12" | wrkdata.univariate$ATC == "N06AA21"), "TCA")
wrkdata.univariate.TCA[4:7] <- NULL
wrkdata.univariate.TCA[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.TCA, by="PatientID", all.x = TRUE)


wrkdata.univariate.SSRI <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N06AB03" | wrkdata.univariate$ATC == "N06AB04" | wrkdata.univariate$ATC == "N06AB05" | wrkdata.univariate$ATC == "N06AB06" | wrkdata.univariate$ATC == "N06AB08" | wrkdata.univariate$ATC == "N06AB10"), "SSRI")
wrkdata.univariate.SSRI[4:7] <- NULL
wrkdata.univariate.SSRI[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.SSRI, by= "PatientID", all.x = TRUE)


wrkdata.univariate.antidepressants <-  cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N06AB03" | wrkdata.univariate$ATC == "N06AB04" | wrkdata.univariate$ATC == "N06AB05" | wrkdata.univariate$ATC == "N06AB06" | wrkdata.univariate$ATC == "N06AB08" | wrkdata.univariate$ATC == "N06AB10" | wrkdata.univariate$ATC == "N06AA02" | wrkdata.univariate$ATC == "N06AA04" | wrkdata.univariate$ATC == "N06AA09" | wrkdata.univariate$ATC == "N06AA10" | wrkdata.univariate$ATC == "N06AA12" | wrkdata.univariate$ATC == "N06AA21" | wrkdata.univariate$ATC == "N06AF04" | wrkdata.univariate$ATC == "N06AF03" | wrkdata.univariate$ATC == "N06AG02" | wrkdata.univariate$ATC == "N06AA21" | wrkdata.univariate$ATC == "N06AX03" | wrkdata.univariate$ATC == "N06AX05" | wrkdata.univariate$ATC == "N06AX11" | wrkdata.univariate$ATC == "N06AX12" | wrkdata.univariate$ATC == "N06AX16" | wrkdata.univariate$ATC == "N06AX21" | wrkdata.univariate$ATC == "N06AX22" ), "Antidepressants")
wrkdata.univariate.antidepressants[4:7] <- NULL
wrkdata.univariate.antidepressants[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.antidepressants, by= "PatientID", all.x = TRUE)


wrkdata.univariate.antipsychotics <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N05AA01" | wrkdata.univariate$ATC == "N05AA02" | wrkdata.univariate$ATC == "N05AB03" | wrkdata.univariate$ATC == "N05AD01" | wrkdata.univariate$ATC == "N05AD05" | wrkdata.univariate$ATC == "N05AD06" |  wrkdata.univariate$ATC == "N05AF01" | wrkdata.univariate$ATC == "N05AF05" | wrkdata.univariate$ATC == "N05AG03" | werkdata.univariate2$ATC == "N05AG02" | wrkdata.univariate$ATC == "N05AH02" | werkdata.univariate2$ATC == "N05AH03" | wrkdata.univariate$ATC == "N05AH04" | wrkdata.univariate$ATC == "N05AL01"), "antipsychotics")
wrkdata.univariate.antipsychotics[4:7] <- NULL
wrkdata.univariate.antipsychotics[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.antipsychotics, by= "PatientID", all.x = TRUE)


wrkdata.univariate.benzo <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N05CD01" | wrkdata.univariate$ATC == "N03AE01" | wrkdata.univariate$ATC == "N05CD02" | werkdata.univariate2$ATC == "N05CD03" | wrkdata.univariate$ATC == "N05CD06" | wrkdata.univariate$ATC == "N05CD07" | wrkdata.univariate$ATC == "N05CD08" |  wrkdata.univariate$ATC == "N05CF01" | wrkdata.univariate$ATC == "N05CF02" | wrkdata.univariate$ATC == "N05BA01" | wrkdata.univariate$ATC == "N05BA02" | wrkdata.univariate$ATC == "N05BA04" | wrkdata.univariate$ATC == "N05BA06" | wrkdata.univariate$ATC == "N05BA08" | wrkdata.univariate$ATC == "N05BA09" | wrkdata.univariate$ATC == "N05BA12" | wrkdata.univariate$ATC == "N05BA16"), "benzodiazepines")
wrkdata.univariate.benzo[4:7] <- NULL
wrkdata.univariate.benzo[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.benzo, by= "PatientID", all.x = TRUE)


wrkdata.univariate.antiepileptica <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N03AA02" | wrkdata.univariate$ATC == "N03AB02" | wrkdata.univariate$ATC == "N03AF01" | wrkdata.univariate$ATC == "N03AF02" | wrkdata.univariate$ATC == "N03AG01" | wrkdata.univariate$ATC == "N03AG04" |  wrkdata.univariate$ATC == "N03AX09" | wrkdata.univariate$ATC == "N03AX11" | wrkdata.univariate$ATC == "N03AX12" | wrkdata.univariate$ATC == "N03AX14" | wrkdata.univariate$ATC == "N03AX16"), "antiepileptica")
wrkdata.univariate.antiepileptica[4:7] <- NULL
wrkdata.univariate.antiepileptica[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.antiepileptica, by= "PatientID", all.x = TRUE)

wrkdata.univariate.digoxin <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C01AA05"), "digoxin")
wrkdata.univariate.digoxin[4:7] <- NULL
wrkdata.univariate.digoxin[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.digoxin, by= "PatientID", all.x = TRUE)


wrkdata.univariate.nitrates <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C01DA02" | wrkdata.univariate$ATC == "C01DA08" | wrkdata.univariate$ATC == "C01DA14" | wrkdata.univariate$ATC == "C01EB17"), "nitrates")
wrkdata.univariate.nitrates[4:7] <- NULL
wrkdata.univariate.nitrates[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.nitrates, by= "PatientID", all.x = TRUE)

wrkdata.univariate.thiazides<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C03AA03" | wrkdata.univariate$ATC == "C03BA04" | wrkdata.univariate$ATC == "C09XA52" | wrkdata.univariate$ATC == "C09BA01" | wrkdata.univariate$ATC == "C09BA02" | wrkdata.univariate$ATC == "C09BA03" | wrkdata.univariate$ATC == "C09BA04" | wrkdata.univariate$ATC == "C09BA05" | wrkdata.univariate$ATC == "C09BA06" | wrkdata.univariate$ATC == "C09BA09" | wrkdata.univariate$ATC == "C09DA01" | wrkdata.univariate$ATC == "C09DA02" | wrkdata.univariate$ATC == "C09DA03" | wrkdata.univariate$ATC == "C09DA04" | wrkdata.univariate$ATC == "C09DA06" | wrkdata.univariate$ATC == "C09DA07" | wrkdata.univariate$ATC == "C09DA08"), "thiazides")
wrkdata.univariate.thiazides[4:7] <- NULL
wrkdata.univariate.thiazides[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, werkdata.univariate.thiazides, by= "PatientID", all.x = TRUE)


wrkdata.univariate.aldosteronantagonist<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C03DA01" | wrkdata.univariate$ATC == "C03DA04" ), "aldosteronantagonist")
wrkdata.univariate.aldosteronantagonist[4:7] <- NULL
wrkdata.univariate.aldosteronantagonist[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.aldosteronantagonist, by= "PatientID", all.x = TRUE)


wrkdata.univariate.betablokker<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C07BB52" | wrkdata.univariate$ATC == "C07AA03" | wrkdata.univariate$ATC == "C07AA05" | wrkdata.univariate$ATC == "C07AA07" | wrkdata.univariate$ATC == "C07AG01" | wrkdata.univariate$ATC == "C07AG02" | wrkdata.univariate$ATC == "C07AB02" | wrkdata.univariate$ATC == "C07AB03" | wrkdata.univariate$ATC == "C07AB07" | wrkdata.univariate$ATC == "C07AB12"), "betablokker")
wrkdata.univariate.betablokker[4:7] <- NULL
wrkdata.univariate.betablokker[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.betablokker, by= "PatientID", all.x = TRUE)


wrkdata.univariate.calciumchannelblocker<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C08CA01" | wrkdata.univariate$ATC == "C08CA02" | wrkdata.univariate$ATC == "C08CA05" | wrkdata.univariate$ATC == "C08CA12" | wrkdata.univariate$ATC == "C08CA13" | wrkdata.univariate$ATC == "C08DA01" | wrkdata.univariate$ATC == "C08DB01" | wrkdata.univariate$ATC == "C09DB01" | wrkdata.univariate$ATC == "C09DB02" | wrkdata.univariate$ATC == "C09DX01" | wrkdata.univariate$ATC == "C09DX03" | wrkdata.univariate$ATC == "C09BB04" | wrkdata.univariate$ATC == "C09BX01"), "calciumchannelblocker")
wrkdata.univariate.calciumchannelblocker[4:7] <- NULL
wrkdata.univariate.calciumchannelblocker[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.calciumchannelblocker, by= "PatientID", all.x = TRUE)


wrkdata.univariate.RAAS<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C09AA01" | wrkdata.univariate$ATC == "C09AA02" | wrkdata.univariate$ATC == "C09AA03" | wrkdata.univariate$ATC == "C09AA04" | wrkdata.univariate$ATC == "C09AA05" | wrkdata.univariate$ATC == "C09AA06" | wrkdata.univariate$ATC == "C09AA09" | wrkdata.univariate$ATC == "C09BA02" | wrkdata.univariate$ATC == "C09BA03" | wrkdata.univariate$ATC == "C09BA04" | wrkdata.univariate$ATC == "C09BA05" | wrkdata.univariate$ATC == "C09BA06" | wrkdata.univariate$ATC == "C09BA09" | wrkdata.univariate$ATC == "C09BB04" | wrkdata.univariate$ATC == "C09CA01" | wrkdata.univariate$ATC == "C09CA02" | wrkdata.univariate$ATC == "C09CA03" | wrkdata.univariate$ATC == "C09CA04" | wrkdata.univariate$ATC == "C09CA06" | wrkdata.univariate$ATC == "C09BA07" | wrkdata.univariate$ATC == "C09BA08" | wrkdata.univariate$ATC == "C09DA01" | wrkdata.univariate$ATC == "C09DA02" | wrkdata.univariate$ATC == "C09DA03" | wrkdata.univariate$ATC == "C09DA04" | wrkdata.univariate$ATC == "C09DA06" | wrkdata.univariate$ATC == "C09DA07" | wrkdata.univariate$ATC == "C09DA08" | wrkdata.univariate$ATC == "C09DB01" | wrkdata.univariate$ATC == "C09DB02" | wrkdata.univariate$ATC == "C09DX01" | wrkdata.univariate$ATC == "C09DX03" | wrkdata.univariate$ATC == "C09XA02" | wrkdata.univariate$ATC == "C09XA52"), "RAAS")
wrkdata.univariate.RAAS[4:7] <- NULL
wrkdata.univariate.RAAS[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.RAAS, by= "PatientID", all.x = TRUE)

DBI.180.FRIDS <- unique(DBI.180.FRIDS)

#write.csv2(DBI.180.FRIDS, "S:/data/DBI.180.FRIDS.csv")

#DBI.180.FRIDS <- read.csv2(file = "S:/data/DBI.180.FRIDS.csv")

wrkdata.univariate.insulin<- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "A10AD01" | wrkdata.univariate$ATC == "A10AD04" | wrkdata.univariate$ATC == "A10AD05" | wrkdata.univariate$ATC == "A10AD06" | wrkdata.univariate$ATC == "A10AD30" | wrkdata.univariate$ATC == "A10AD02" | wrkdata.univariate$ATC == "A10AD03"), "insulin")
wrkdata.univariate.insulin[4:7] <- NULL
wrkdata.univariate.insulin[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.insulin, by= "PatientID", all.x = TRUE)

wrkdata.univariate.metformin <-cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "A10BA02" | wrkdata.univariate$ATC == "A10BD05" | wrkdata.univariate$ATC == "A10BD02" | wrkdata.univariate$ATC == "A10BD07" ), "metformin")
wrkdata.univariate.metformin[4:7] <- NULL
wrkdata.univariate.metformin[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.metformin, by= "PatientID", all.x = TRUE)


wrkdata.univariate.sulfonylurea <-cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "A10BB01" | wrkdata.univariate$ATC == "A10BB03" | wrkdata.univariate$ATC == "A10BB09" | wrkdata.univariate$ATC == "A10BB12" | wrkdata.univariate$ATC == "A10BD02" ), "sulfonylurea")
wrkdata.univariate.sulfonylurea[4:7] <- NULL
wrkdata.univariate.sulfonylurea[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.sulfonylurea, by= "PatientID", all.x = TRUE)

wrkdata.univariate.oralantidiabetics <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "A10BB01" | wrkdata.univariate$ATC == "A10BB03" | wrkdata.univariate$ATC == "A10BB09" | wrkdata.univariate$ATC == "A10BB12" | wrkdata.univariate$ATC == "A10BD02" | wrkdata.univariate$ATC == "A10BA02" | wrkdata.univariate$ATC == "A10BD05" | wrkdata.univariate$ATC == "A10BD02" | wrkdata.univariate$ATC == "A10BD07" | wrkdata.univariate$ATC == "A10BF01" | wrkdata.univariate$ATC == "A10BG03" | wrkdata.univariate$ATC == "A10BH01" | wrkdata.univariate$ATC == "A10BH02" | wrkdata.univariate$ATC == "A10BH03" | wrkdata.univariate$ATC == "A10BH05" | wrkdata.univariate$ATC == "A10BJ01" | wrkdata.univariate$ATC == "A10BJ02" | wrkdata.univariate$ATC == "A10BJ05"), "oralantidiabetics")
wrkdata.univariate.oralantidiabetics[4:7] <- NULL
wrkdata.univariate.oralantidiabetics[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.oralantidiabetics, by= "PatientID", all.x = TRUE)


wrkdata.univariate.alfablokkers <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "G04CA01" | wrkdata.univariate$ATC == "G04CA02" | wrkdata.univariate$ATC == "GA04CA04" | wrkdata.univariate$ATC == "G04CA52" | wrkdata.univariate$ATC == "G04CA53" | wrkdata.univariate$ATC == "C02CA04" | wrkdata.univariate$ATC == "C02CA06"), "alfablokkers")
wrkdata.univariate.alfablokkers[4:7] <- NULL
wrkdata.univariate.alfablokkers[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.alfablokkers, by= "PatientID", all.x = TRUE)


wrkdata.univariate.opiaten <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N02AB03" | wrkdata.univariate$ATC == "N02AE01" | wrkdata.univariate$ATC == "N07BC02" | wrkdata.univariate$ATC == "N02AX06" | wrkdata.univariate$ATC == "N02AA01" | wrkdata.univariate$ATC == "N02AA05" | wrkdata.univariate$ATC == "N02AA55" | wrkdata.univariate$ATC == "N07BC51"), "opiaten")
wrkdata.univariate.opiaten[4:7] <- NULL
wrkdata.univariate.opiaten[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.opiaten, by= "PatientID", all.x = TRUE)


wrkdata.univariate.tramadol <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N02AX02" | wrkdata.univariate$ATC == "N02AJ13" | wrkdata.univariate$ATC == "N02AJ15"), "tramadol")
wrkdata.univariate.tramadol[4:7] <- NULL
wrkdata.univariate.tramadol[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.tramadol, by= "PatientID", all.x = TRUE)



wrkdata.univariate.statines <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C10AA01" | wrkdata.univariate$ATC == "C10AA03" | wrkdata.univariate$ATC == "C10AA04" | wrkdata.univariate$ATC == "C10AA05" | wrkdata.univariate$ATC == "C10AA06" | wrkdata.univariate$ATC == "C10AA07" | wrkdata.univariate$ATC == "C10BA02" | wrkdata.univariate$ATC == "C10BA03" | wrkdata.univariate$ATC == "C10BA05"), "statines")
wrkdata.univariate.statines[4:7] <- NULL
wrkdata.univariate.statines[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.statines, by= "PatientID", all.x = TRUE)

DBI.180.FRIDS <- unique(DBI.180.FRIDS)  




########## extra gm

wrkdata.univariate.codeine <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "R05DA04" | wrkdata.univariate$ATC == "N02AJ06" | wrkdata.univariate$ATC == "N02AA59" | wrkdata.univariate$ATC == "N02AA79" | wrkdata.univariate$ATC == "N02AJ09" | wrkdata.univariate$ATC == "N02BE71" ), "codeine")
wrkdata.univariate.codeine[4:7] <- NULL
wrkdata.univariate.codeine[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.codeine, by= "PatientID", all.x = TRUE)

wrkdata.univariate.allopiates <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "R05DA04" | wrkdata.univariate$ATC == "N02AJ06" | wrkdata.univariate$ATC == "N02AA59" | wrkdata.univariate$ATC == "N02AA79" | wrkdata.univariate$ATC == "N02AJ09" | wrkdata.univariate$ATC == "N02BE71" | wrkdata.univariate$ATC == "N02AX02" | wrkdata.univariate$ATC == "N02AJ13" | wrkdata.univariate$ATC == "N02AJ15" | wrkdata.univariate$ATC == "N02AB03" | wrkdata.univariate$ATC == "N02AE01" | wrkdata.univariate$ATC == "N07BC02" | wrkdata.univariate$ATC == "N02AX06" | wrkdata.univariate$ATC == "N02AA01" | wrkdata.univariate$ATC == "N02AA05" | wrkdata.univariate$ATC == "N02AA55" | wrkdata.univariate$ATC == "N07BC51"), "allopiates")
wrkdata.univariate.allopiates[4:7] <- NULL
wrkdata.univariate.allopiates[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.allopiates, by= "PatientID", all.x = TRUE)

wrkdata.univariate.urinaryspasmodics <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "G04BD02" | wrkdata.univariate$ATC == "G04BD04" | wrkdata.univariate$ATC == "G04BD07" | wrkdata.univariate$ATC == "G04BD08" | wrkdata.univariate$ATC == "G04BD10" | wrkdata.univariate$ATC == "G04BD12" | wrkdata.univariate$ATC == "G04BD11" ), "urinaryspasmodics")
wrkdata.univariate.urinaryspasmodics[4:7] <- NULL
wrkdata.univariate.urinaryspasmodics[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.urinaryspasmodics, by= "PatientID", all.x = TRUE)


wrkdata.univariate.potassiumdiuretics <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C03DB02" | wrkdata.univariate$ATC == "C03DB01" ), "potassiumdiuretics")
wrkdata.univariate.potassiumdiuretics[4:7] <- NULL
wrkdata.univariate.potassiumdiuretics[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.potassiumdiuretics, by= "PatientID", all.x = TRUE)


wrkdata.univariate.otheranxiolytics <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N05BE01" | wrkdata.univariate$ATC == "N05BB01" | wrkdata.univariate$ATC == "N05BB51" | wrkdata.univariate$ATC == "N05BB51" ), "otheranxiolytics")
wrkdata.univariate.otheranxiolytics[4:7] <- NULL
wrkdata.univariate.otheranxiolytics[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.otheranxiolytics, by= "PatientID", all.x = TRUE)


wrkdata.univariate.antivertigo <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N07CA01" | wrkdata.univariate$ATC == "N07CA02" | wrkdata.univariate$ATC == "N07CA03" | wrkdata.univariate$ATC == "N07CA52"), "antivertigo")
wrkdata.univariate.antivertigo[4:7] <- NULL
wrkdata.univariate.antivertigo[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.antivertigo, by= "PatientID", all.x = TRUE)


wrkdata.univariate.baclofen <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "M03BX01"), "baclofen")
wrkdata.univariate.baclofen[4:7] <- NULL
wrkdata.univariate.baclofen[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.baclofen, by= "PatientID", all.x = TRUE)


wrkdata.univariate.antihistamines <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "R06AA04" | wrkdata.univariate$ATC == "R06AD02" | wrkdata.univariate$ATC == "R06AD01" | wrkdata.univariate$ATC == "R06AE03" | wrkdata.univariate$ATC == "R06AE07" | wrkdata.univariate$ATC == "R06AE09" | wrkdata.univariate$ATC == "R06AX13" | wrkdata.univariate$ATC == "R06AX22" | wrkdata.univariate$ATC == "R06AX25" | wrkdata.univariate$ATC == "R06AX26" | wrkdata.univariate$ATC == "R06AX27" | wrkdata.univariate$ATC == "R06AX28"), "antihistaminics")
wrkdata.univariate.antihistamines[4:7] <- NULL
wrkdata.univariate.antihistamines[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.antihistamines, by= "PatientID", all.x = TRUE)

DBI.180.FRIDS <- unique(DBI.180.FRIDS)  


wrkdata.univariate.NSAIDs <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "M01AB01" | wrkdata.univariate$ATC == "M01AB05" | wrkdata.univariate$ATC == "M01AB06" | wrkdata.univariate$ATC == "M01AB16" | wrkdata.univariate$ATC == "M01AC06" | wrkdata.univariate$ATC == "M01AE01" | wrkdata.univariate$ATC == "M01AE02" | wrkdata.univariate$ATC == "M01AE17" | wrkdata.univariate$ATC == "M01AH01" | wrkdata.univariate$ATC == "M01AH05" ), "NSAIDs")
wrkdata.univariate.NSAIDs[4:7] <- NULL
wrkdata.univariate.NSAIDs[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.NSAIDs, by= "PatientID", all.x = TRUE)

DBI.180.FRIDS <- unique(DBI.180.FRIDS)  


wrkdata.univariate.dipyridamole <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "B01AC07" | wrkdata.univariate$ATC == "B01AC30" ), "dypridamole")
wrkdata.univariate.dipyridamole[4:7] <- NULL
wrkdata.univariate.dipyridamole[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.dipyridamole, by= "PatientID", all.x = TRUE)


wrkdata.univariate.lithium <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "N05AN01" ), "lithium")
wrkdata.univariate.lithium[4:7] <- NULL
wrkdata.univariate.lithium[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.lithium, by= "PatientID", all.x = TRUE)

wrkdata.univariate.classIanthiarrhytmica <- cbind(subset(wrkdata.univariate, wrkdata.univariate$ATC == "C01BC04" | wrkdata.univariate$ATC == "C01BC03"  | wrkdata.univariate$ATC == "C01BA03" ), "classIanthiarrhytmica")
wrkdata.univariate.classIanthiarrhytmica[4:7] <- NULL
wrkdata.univariate.classIanthiarrhytmica[1:2] <- NULL

DBI.180.FRIDS <- merge(DBI.180.FRIDS, wrkdata.univariate.classIanthiarrhytmica, by= "PatientID", all.x = TRUE)


# write.csv2(DBI.180.FRIDS, "S:/data/DBI.180.FRIDS2_2.csv")

#DBI.180.FRIDS2 <- read.csv2(file = "S:/data/DBI.180.FRIDS2_2.csv")
#DBI.180.FRIDS2[1] <- NULL




univ <- multinom(val ~ betablokker, data = DBI.180.FRIDS2)


summary(univ)
exp(coef(univ))
exp(confint(univ))

subset(DBI.180.FRIDS2, DBI.180.FRIDS2$betablokker == "1") %>% group_by(val) %>% summarise (n=n())
subset(DBI.180.FRIDS2, DBI.180.FRIDS2$betablokker == "0") %>% group_by(val) %>% summarise (n=n())

#n1 <- c(n, subset(DBI.180.FRIDS2, DBI.180.FRIDS2$statines == "1") %>% group_by(val) %>% summarise (n=n()))
#n2 <- c(n, subset(DBI.180.FRIDS2, DBI.180.FRIDS2$statines == "0") %>% group_by(val) %>% summarise (n=n()))

#chisq.test(data.frame(n1$n, n2$n))
#fisher.test(data.frame(n1$n, n2$n))


##### building  model
###### only p <0,10
#install.packages("nnet")
library(nnet)
library(dplyr)



model180 <- multinom(as.factor(val) ~  leeftijd + geslacht + Antidepressants 
                     + antiepileptica + nitrates + thiazides 
                     + loopdiuretics + aldosteronantagonist
                     + allopiates + statines
                     + alfablokkers + urinaryspasmodics + antivertigo, data = DBI.180.FRIDS2)
library(MASS)
stepAIC(model180, direction = "backward")




summary(model180)
exp(coef(model180))
exp(confint(model180))

##### final model

model180.final <- multinom(as.factor(val) ~  leeftijd + geslacht + Antidepressants 
                     + antiepileptica + aldosteronantagonist
                     + allopiates + statines
                     + alfablokkers + urinaryspasmodics + antivertigo, data = DBI.180.FRIDS2)

exp(coef(model180.final))
exp(confint(model180.final))

X <- predict(model180.final, newdata = DBI.180.FRIDS2, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- data.frame(X)

DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, ONE_VS_OTHER=ifelse(val=="1", "1", "0"))
DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, MORE_VS_OTHER=ifelse(val=="2", "1", "0"))


roc.onevsother <- roc(DBI.180.FRIDS2$ONE_VS_OTHER, X$X1, plot=TRUE)
roc.morevsother <- roc(DBI.180.FRIDS2$MORE_VS_OTHER, X$X2, plot=TRUE)

library(pROC)

roc.onevsother
ci(roc.onevsother)
roc.morevsother
ci(roc.morevsother)


model180.hvz <- multinom(as.factor(val) ~  leeftijd + geslacht  + nitrates + thiazides 
                     + loopdiuretics + aldosteronantagonist
                     + statines
                     + alfablokkers, data = DBI.180.FRIDS2)
library(MASS)
stepAIC(model180.hvz, direction = "backward")


model180.hvz <- multinom(as.factor(val) ~  leeftijd + geslacht  + aldosteronantagonist
                         + statines, data = DBI.180.FRIDS2)



X <- predict(model180.hvz, newdata = DBI.180.FRIDS2, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- data.frame(X)

DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, ONE_VS_OTHER=ifelse(val=="1", "1", "0"))
DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, MORE_VS_OTHER=ifelse(val=="2", "1", "0"))


roc.onevsother <- roc(DBI.180.FRIDS2$ONE_VS_OTHER, X$X1, plot=TRUE)
roc.morevsother <- roc(DBI.180.FRIDS2$MORE_VS_OTHER, X$X2, plot=TRUE)

roc.onevsother
ci(roc.onevsother)
roc.morevsother
ci(roc.morevsother)

exp(coef(model180.hvz))
exp(confint(model180.hvz))


model180.psych <- multinom(as.factor(val) ~  leeftijd + geslacht + Antidepressants 
                           + antiepileptica  
                           + allopiates 
                           + urinaryspasmodics + antivertigo, data = DBI.180.FRIDS2)

library(MASS)
stepAIC(model180.psych, direction = "backward")


X <- predict(model180.psych, newdata = DBI.180.FRIDS2, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- data.frame(X)

DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, ONE_VS_OTHER=ifelse(val=="1", "1", "0"))
DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, MORE_VS_OTHER=ifelse(val=="2", "1", "0"))


roc.onevsother <- roc(DBI.180.FRIDS2$ONE_VS_OTHER, X$X1, plot=TRUE)
roc.morevsother <- roc(DBI.180.FRIDS2$MORE_VS_OTHER, X$X2, plot=TRUE)

roc.onevsother
ci(roc.onevsother)
roc.morevsother
ci(roc.morevsother)

exp(coef(model180.psych))
exp(confint(model180.psych))


######### CHI-square test geslacht en ANOVA leeftijd

library(dplyr)

subset(DBI.180.FRIDS2, DBI.180.FRIDS2$geslacht == "M") %>% group_by(val) %>% summarise (n=n())
subset(DBI.180.FRIDS2, DBI.180.FRIDS2$geslacht == "V") %>% group_by(val) %>% summarise (n=n())

n1 <- c(n, subset(DBI.180.FRIDS2, DBI.180.FRIDS2$geslacht == "M") %>% group_by(val) %>% summarise (n=n()))
n2 <- c(n, subset(DBI.180.FRIDS2, DBI.180.FRIDS2$geslacht == "V") %>% group_by(val) %>% summarise (n=n()))

data.frame(n1$n, n2$n)

chisq.test(data.frame(n1$n, n2$n))

anova_leeftijd <- aov(leeftijd ~ as.factor(val), data = DBI.180.FRIDS2)
summary(anova_leeftijd)
TukeyHSD(anova_leeftijd)

summary(anova_leeftijd)

group_by(DBI.180.FRIDS2, val) %>%
  summarise(
    count = n(),
    mean = mean(leeftijd, na.rm = TRUE),
    sd = sd(leeftijd, na.rm = TRUE)
  )


######### analyses DBI

#### verschil tussen groepen en gemiddelde DBI
kruskal.test(DBI.calculated ~ as.factor(val), data = DBI.180.FRIDS2)

pairwise.wilcox.test(DBI.180.FRIDS2$DBI.calculated, DBI.180.FRIDS2$val, p.adjust.method = "BH")

############# goodness of fit Hosmer-Lemeshow
#install.packages("generalhoslem")
#library(generalhoslem)

X <- predict(model180.hvz, newdata = DBI.180.FRIDS2, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- predict(model180.psych, newdata = DBI.180.FRIDS2, type = "prob")
X <- predict(model180.final, newdata = DBI.180.FRIDS2, type = "prob")

logitgof(DBI.180.FRIDS2$val, X, g = 10, ord = FALSE)




ithr <- 0.9999999999999999999999

DBI.180$DBI.3cat <- as.factor( cut( DBI.180$DBI.calculated, c(-1,0,ithr, max(DBI.180$DBI.calculated,na.rm=T)) ) )
DBI.3cat +

model.180 <- multinom(as.factor(val) ~ leeftijd+ geslacht , data=DBI.180, Hess=TRUE)

X <- predict(model.180, newdata = DBI.180, type = "prob")     # prob 0 / 1 and 2 for each patient

logitgof(DBI.180$val, X, g = 10, ord = FALSE)




############

model180 <- multinom(as.factor(val) ~  leeftijd + geslacht + SSRI + TCA + benzodiazepines
                     + antiepileptica + digoxin + nitrates + thiazides 
                     + loopdiuretics + aldosteronantagonist + calciumchannelblocker + RAAS + sulfonylurea + insulin
                     + tramadol + codeine + opiaten + statines + antihistaminics + NSAIDs
                     + alfablokkers + urinaryspasmodics + antivertigo + dypridamole, data = DBI.180.FRIDS2)

summary(model180)
exp(coef(model180))


X <- predict(model180, newdata = DBI.180.FRIDS2, type = "prob")     # prob 0 / 1 and 2 for each patient
X <- data.frame(X)

DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, ONE_VS_OTHER=ifelse(val=="1", "1", "0"))
DBI.180.FRIDS2 <- transform(DBI.180.FRIDS2, MORE_VS_OTHER=ifelse(val=="2", "1", "0"))

library(pROC)

roc.onevsother <- roc(DBI.180.FRIDS2$ONE_VS_OTHER, X$X1, plot=TRUE)
roc.morevsother <- roc(DBI.180.FRIDS2$MORE_VS_OTHER, X$X2, plot=TRUE)

roc.onevsother
ci(roc.onevsother)
roc.morevsother
ci(roc.morevsother)

logitgof(DBI.180.FRIDS2$val, X, g = 10, ord = FALSE)   ### hosmer??


#bootstrap-corrected estimated area under ROC curve and calibration curve
optimism.corrected = rms::validate(model180, method="boot", B=500) # bootstrapped (internal) validation
Area.under.ROC= optimism.corrected[1,5]/2 +0.5
Area.under.ROC # closer to 1 the better, 0.5 is a model with no ability to discriminate between future cases/non-cases
plot(rms::calibrate(mod1, method="boot", B=500), las=1) # create calibration curve
#The 45 degree line indicates what perfect calibration would look like




cutt.off.DBI <- c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,2.0,2.5,3.0)
oddsratio.onefall <- c(1.32,1.32,1.33,1.33,1.42,1.44,1.45,1.44,1.32,1.83,1.30)
oddsratio.rec.fall <- c(1.55,1.64,1.63,1.72,1.86,1.87,1.93,1.99,2.00,2.24,2.15)

plot(cutt.off.DBI,oddsratio, main="Predicting falling by DBI exposure", xlab="DBI threshold", ylab="odds ratio for DBI >= threshold", ylim=c(1,2.5))

plot(cutt.off.DBI, oddsratio.onefall, main="Predicting falling by DBI exposure", xlab="DBI threshold", ylab="odds ratio", pch=1, col="blue", xlim = c(0.9, 3.1), ylim = c(0.8, 2.3))
points(cutt.off.DBI,oddsratio.rec.fall,pch=2,col="red")
legend("topleft", c("single fallers", "recurrent fallers"), fill = c("blue", "red"))
abline(h=1, lty=2, col="lightgrey")


#install.packages("plotrix")
#require(plotrix)


oddsratio.onefall.uiw <- c(1.67, 1.69, 1.72, 1.73, 1.88, 1.94, 1.98, 1.99, 0.95, 3.04, 3.05)
oddsratio.onefall.liw <- c(1.04, 1.04, 1.03, 1.02, 1.07, 1.07, 1.06, 1.04, 0.89, 1.10, 0.55)

oddsratio.rec.fall.uiw <- c(1.98, 2.10, 2.11, 2.24, 2.46, 2.51, 2.62, 2.74, 2.89, 3.72, 4.57)
oddsratio.rec.fall.liw <- c(1.22, 1.28, 1.26, 1.31, 1.40, 1.39, 1.42, 1.45, 1.39, 1.35, 1.01)

plotCI(cutt.off.DBI, oddsratio.onefall, ui = oddsratio.onefall.uiw, li = oddsratio.onefall.liw, main="Predicting falling by DBI exposure", xlab="DBI threshold", ylab="odds ratio",pch=1, col="blue", xlim = c(0.9, 3.1), ylim = c(0,5))
par(new=T)
plotCI(cutt.off.DBI, oddsratio.rec.fall, ui = oddsratio.rec.fall.uiw, li = oddsratio.rec.fall.liw, pch=2,col="red", main="Predicting falling by DBI exposure", xlab="DBI threshold", ylab="odds ratio", xlim = c(0.9, 3.1), ylim = c(0,5))



#######



ithr <-0.99999999999999999999999999999999999999999999999

DBI.180.FRIDS2$DBI.3cat <- as.factor( cut( DBI.180.FRIDS2$DBI.calculated, c(-1,0,ithr, max(DBI.180.FRIDS2$DBI.calculated,na.rm=T)) ) )

model.180 <- multinom(as.factor(val) ~ DBI.3cat +leeftijd+ geslacht , data=DBI.180.FRIDS2, Hess=TRUE)

summary(model.180)
exp(coef(model.180))
exp(confint(model.180))

library(dplyr)

DBI.180.FRIDS2 %>% group_by(DBI.3cat) %>% summarise(count=n())
subset(DBI.180.FRIDS2, DBI.180$val =="1") %>% group_by(DBI.3cat) %>% summarise(count=n())
