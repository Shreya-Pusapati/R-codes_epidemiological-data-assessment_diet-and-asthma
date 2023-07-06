
# =================================TRIAL=====================================================

# Converting all NA values in all 16 food group columns (original columns)
# original columns were used in as.factor for log analysis of 16 foods
# converting them back to as.numeric
# *******also convert NA to 0***********************

A_chi$Meat <- as.numeric(A_chi$Meat)
A_chi$Seafood <- as.numeric(A_chi$Seafood)
A_chi$Fruits <- as.numeric(A_chi$Fruits)
A_chi$Vegetables <- as.numeric(A_chi$Vegetables)
A_chi$Pulses <- as.numeric(A_chi$Pulses)
A_chi$Cereals <- as.numeric(A_chi$Cereals)
A_chi$Rice <- as.numeric(A_chi$Rice)
A_chi$Butter <- as.numeric(A_chi$Butter)
A_chi$Margarine <- as.numeric(A_chi$Margarine)
A_chi$Potatoes <- as.numeric(A_chi$Potatoes)
A_chi$Milk <- as.numeric(A_chi$Milk)
A_chi$Eggs <- as.numeric(A_chi$Eggs)
A_chi$`Burgers/fast.food` <- as.numeric(A_chi$`Burgers/fast.food`)
A_chi$Nuts <- as.numeric(A_chi$Nuts)
A_chi$`Yakult/Vitagen` <- as.numeric(A_chi$`Yakult/Vitagen`)
A_chi$Pasta <- as.numeric(A_chi$Pasta)


# converting NA to 0 in all columns
A_chi$Meat[is.na(A_chi$Meat)] <- 0
A_chi$Seafood[is.na(A_chi$Seafood)] <- 0
A_chi$Fruits[is.na(A_chi$Fruits)] <- 0
A_chi$Vegetables[is.na(A_chi$Vegetables)] <- 0
A_chi$Pulses[is.na(A_chi$Pulses)] <- 0
A_chi$Cereals[is.na(A_chi$Cereals)] <- 0
A_chi$Pasta[is.na(A_chi$Pasta)] <- 0
A_chi$Rice[is.na(A_chi$Rice)] <- 0
A_chi$Butter[is.na(A_chi$Butter)] <- 0
A_chi$Margarine[is.na(A_chi$Margarine)] <- 0
A_chi$Nuts[is.na(A_chi$Nuts)] <- 0
A_chi$Potatoes[is.na(A_chi$Potatoes)] <- 0
A_chi$Milk[is.na(A_chi$Milk)] <- 0
A_chi$Eggs[is.na(A_chi$Eggs)] <- 0
A_chi$`Burgers/fast.food`[is.na(A_chi$`Burgers/fast.food`)] <- 0
A_chi$`Yakult/Vitagen`[is.na(A_chi$`Yakult/Vitagen`)] <- 0

#---------------------------SCORE INDICES CODES-----------------------------------

#------------ FOR FATS
high_fats<-c('Margarine', 'Butter', 'Seafood', 'Burgers/fast.food','Meat','Eggs','Milk')
low_fats<-c('Fruits', 'Vegetables', 'Pulses','Cereals','Potatoes','Rice') 
summary(A_chi[,c(high_fats,low_fats)])

for(.i in 1:nrow(A_chi))
{.low1<-rowSums(A_chi[.i, low_fats]==1)
.low2<-rowSums(A_chi[.i, low_fats]==2)
.low3<-rowSums(A_chi[.i, low_fats]==3)
.high1<-rowSums(A_chi[.i, high_fats]==1)
.high2<-rowSums(A_chi[.i, high_fats]==2)
.high3<-rowSums(A_chi[.i, high_fats]==3)
A_chi[.i, 'DQDFS']<- (-2*.low2)+(-7*.low3)+(2*.high2)+(7*.high3)}


quantile(A_chi$DQDFS, probs =c(.33,.66), na.rm = TRUE)
A_chi$DQDFS<-as.numeric(A_chi$DQDFS)
summary(A_chi$DQDFS)

#categorizing into low, high and moderate 
A_chi$DQDFS.CAT<-with(A_chi, ifelse(DQDFS>=0, 'High', 
                                  ifelse(DQDFS>=-8 & DQDFS<=-1, 'Moderate',
                                         ifelse(DQDFS<=-9, 'Low',NA))))
A_chi$DQDFS.CAT<-as.factor(A_chi$DQDFS.CAT)
summary(A_chi$DQDFS.CAT)
A_chi$DQDFS.CAT<-relevel(A_chi$DQDFS.CAT,ref = "Low") # make low the reference.
summary(A_chi$DQDFS.CAT)

#-----------------FOR PROTEINS
high_proteins<-c('Meat', 'Seafood', 'Nuts', 'Eggs','Burgers/fast.food','Cereals')
low_proteins<-c('Milk', 'Rice', 'Pulses','Vegetables','Potatoes','Fruits') 
summary(A_chi[,c(high_proteins,low_proteins)])

for(.i in 1:nrow(A_chi))
{.lo1<-rowSums(A_chi[.i, low_proteins]==1)
.lo2<-rowSums(A_chi[.i, low_proteins]==2)
.lo3<-rowSums(A_chi[.i, low_proteins]==3)
.hi1<-rowSums(A_chi[.i, high_proteins]==1)
.hi2<-rowSums(A_chi[.i, high_proteins]==2)
.hi3<-rowSums(A_chi[.i, high_proteins]==3)
A_chi[.i, 'DQDPS']<- (-2*.lo2)+(-7*.lo3)+(2*.hi2)+(7*.hi3)}

quantile(A_chi$DQDPS, probs =c(.33,.66), na.rm = TRUE)
A_chi$DQDPS<-as.numeric(A_chi$DQDPS)
summary(A_chi$DQDPS)

#categorizing into low, high and moderate to obtain bell-curve like data distribution
A_chi$DQDPS.CAT<-with(A_chi, ifelse(DQDPS>=0, 'High', 
                                  ifelse(DQDPS>=-7 & DQDPS<=-1, 'Moderate',
                                         ifelse(DQDPS<=-8, 'Low',NA))))
A_chi$DQDPS.CAT<-as.factor(A_chi$DQDPS.CAT)
summary(A_chi$DQDPS.CAT)
A_chi$DQDPS.CAT<-relevel(A_chi$DQDPS.CAT,ref = "Low") # make low the reference.
summary(A_chi$DQDPS.CAT) #check for re-leveling

#----------------FOR GLYCEMIC INDEX
high_gi<-c('Cereals', 'Rice', 'Potatoes', 'Burgers/fast.food') 
low_gi<-c('Fruits','Vegetables','Pulses', 'Nuts', 'Milk', 'Yakult/Vitagen')
summary(A_chi[, c(high_gi, low_gi)])


for(.i in 1:nrow(A_chi))
{.low1<-rowSums(A_chi[.i, low_gi]==1)
.low2<-rowSums(A_chi[.i, low_gi]==2)
.low3<-rowSums(A_chi[.i, low_gi]==3)
.high1<-rowSums(A_chi[.i, high_gi]==1)
.high2<-rowSums(A_chi[.i, high_gi]==2)
.high3<-rowSums(A_chi[.i, high_gi]==3)
A_chi[.i, 'DQDGiS']<- (-2*.low2)+(-7*.low3)+(2*.high2)+(7*.high3)}

quantile(A_chi$DQDGiS, probs =c(.33,.66), na.rm = TRUE)
A_chi$DQDGiS<-as.numeric(A_chi$DQDGiS)
summary(A_chi$DQDGiS)

#categorizing into low, high and moderate to obtain bell-curve like data distribution
A_chi$DQDGiS.CAT<-with(A_chi, ifelse(DQDGiS>=-2, 'High', 
                                   ifelse(DQDGiS>=-9 & DQDGiS<=-3, 'Moderate',
                                          ifelse(DQDGiS<=-10, 'Low',NA))))
A_chi$DQDGiS.CAT<-as.factor(A_chi$DQDGiS.CAT)
summary(A_chi$DQDGiS.CAT)
A_chi$DQDGiS.CAT<-relevel(A_chi$DQDGiS.CAT,ref = "Low") # make low the reference.
summary(A_chi$DQDGiS.CAT)


# =========================Perform univariate and multivariate analyses=============================================
m<-glm(everAS ~DQDFS.CAT, data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m))
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)

m<-glm(everAS ~DQDFS.CAT + Age + Gender+ Parentalhistory + BMI , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m)) 
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)
