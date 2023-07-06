#Dietary indices

#================ Foodc columns created and used for indices=========================

# removing 9 values in all the original 16 food group columns
# converting 9 values to NA in original columns

A_chi$Meat[which(A_chi$Meat == 9)] <- NA
A_chi$Seafood[which(A_chi$Seafood == 9)] <- NA
A_chi$Fruits[which(A_chi$Fruits == 9)] <- NA
A_chi$Vegetables[which(A_chi$Vegetables == 9)] <- NA
A_chi$Pulses[which(A_chi$Pulses == 9)] <- NA
A_chi$Cereals[which(A_chi$Cereals == 9)] <- NA
A_chi$Pasta[which(A_chi$Pasta == 9)] <- NA
A_chi$Rice[which(A_chi$Rice == 9)] <- NA
A_chi$Butter[which(A_chi$Butter == 9)] <- NA
A_chi$Margarine[which(A_chi$Margarine == 9)] <- NA
A_chi$Nuts[which(A_chi$Nuts == 9)] <- NA
A_chi$Potatoes[which(A_chi$Potatoes == 9)] <- NA
A_chi$Milk[which(A_chi$Milk == 9)] <- NA
A_chi$Eggs[which(A_chi$Eggs == 9)] <- NA
A_chi$`Burgers/fast.food`[which(A_chi$`Burgers/fast.food` == 9)] <- NA
A_chi$`Yakult/Vitagen`[which(A_chi$`Yakult/Vitagen` == 9)] <- NA

A_chi$Eggs[which(A_chi$Eggs == "22")] <- NA #ADDITIONAL RESPONSE CONVERTED TO NA


A_chi$Meat <- as.factor(A_chi$Meat)
A_chi$Seafood <- as.factor(A_chi$Seafood)
A_chi$Fruits <- as.factor(A_chi$Fruits)
A_chi$Vegetables <- as.factor(A_chi$Vegetables)
A_chi$Pulses <- as.factor(A_chi$Pulses)
A_chi$Cereals <- as.factor(A_chi$Cereals)
A_chi$Rice <- as.factor(A_chi$Rice)
A_chi$Butter <- as.factor(A_chi$Butter)
A_chi$Margarine <- as.factor(A_chi$Margarine)
A_chi$Potatoes <- as.factor(A_chi$Potatoes)
A_chi$Milk <- as.factor(A_chi$Milk)
A_chi$Eggs <- as.factor(A_chi$Eggs)
A_chi$`Burgers/fast.food` <- as.factor(A_chi$`Burgers/fast.food`)
A_chi$Nuts <- as.factor(A_chi$Nuts)
A_chi$`Yakult/Vitagen` <- as.factor(A_chi$`Yakult/Vitagen`)
A_chi$Pasta <- as.factor(A_chi$Pasta)

# used these columns for log regression analysis for 16 foods.

#creating 16 new columns for INDICES based on 0,2,7 scoring system.
A_chi$Meatc <- ifelse(A_chi$Meat == 3,'7',
                      ifelse(A_chi$Meat %in% c(1,NA),'0','2'))
A_chi$Seafoodc <- ifelse(A_chi$Seafood == 3,'7',
                         ifelse(A_chi$Seafood %in% c(1,NA),'0','2'))
A_chi$Fruitsc <- ifelse(A_chi$Fruits == 3,'7',
                        ifelse(A_chi$Fruits %in% c(1,NA),'0','2'))
A_chi$Vegetablesc <- ifelse(A_chi$Vegetables == 3,'7',
                            ifelse(A_chi$Vegetables %in% c(1,NA),'0','2'))
A_chi$Pulsesc <- ifelse(A_chi$Pulses == 3,'7',
                        ifelse(A_chi$Pulses %in% c(1,NA),'0','2'))
A_chi$Cerealsc <- ifelse(A_chi$Cereals == 3,'7',
                         ifelse(A_chi$Cereals %in% c(1,NA),'0','2'))
A_chi$Pastac <- ifelse(A_chi$Pasta == 3,'7',
                       ifelse(A_chi$Pasta %in% c(1,NA),'0','2'))
A_chi$Ricec <- ifelse(A_chi$Rice == 3,'7',
                      ifelse(A_chi$Rice %in% c(1,NA),'0','2'))
A_chi$Butterc <- ifelse(A_chi$Butter == 3,'7',
                        ifelse(A_chi$Butter %in% c(1,NA),'0','2'))
A_chi$Margarinec <- ifelse(A_chi$Margarine == 3,'7',
                           ifelse(A_chi$Margarine %in% c(1,NA),'0','2'))
A_chi$Nutsc <- ifelse(A_chi$Nuts == 3,'7',
                      ifelse(A_chi$Nuts %in% c(1,NA),'0','2'))
A_chi$Potatoesc <- ifelse(A_chi$Potatoes == 3,'7',
                          ifelse(A_chi$Potatoes %in% c(1,NA),'0','2'))
A_chi$Milkc <- ifelse(A_chi$Milk == 3,'7',
                      ifelse(A_chi$Milk %in% c(1,NA),'0','2'))
A_chi$Eggsc <- ifelse(A_chi$Eggs == 3,'7',
                      ifelse(A_chi$Eggs %in% c(1,NA),'0','2'))
A_chi$Burgers.fastfoodc <- ifelse(A_chi$`Burgers/fast.food` == 3,'7',
                          ifelse(A_chi$`Burgers/fast.food` %in% c(1,NA),'0','2'))
A_chi$Yakultc <- ifelse(A_chi$`Yakult/Vitagen` == 3,'7',
                        ifelse(A_chi$`Yakult/Vitagen` %in% c(1,NA),'0','2'))

# converting rest of the NA values to 0 in all the Foodc columns
A_chi$Meatc[is.na(A_chi$Meatc)] <- 0
A_chi$Seafoodc[is.na(A_chi$Seafoodc)] <- 0
A_chi$Fruitsc[is.na(A_chi$Fruitsc)] <- 0
A_chi$Vegetablesc[is.na(A_chi$Vegetablesc)] <- 0
A_chi$Pulsesc[is.na(A_chi$Pulsesc)] <- 0
A_chi$Cerealsc[is.na(A_chi$Cerealsc)] <- 0
A_chi$Pastac[is.na(A_chi$Pastac)] <- 0
A_chi$Ricec[is.na(A_chi$Ricec)] <- 0
A_chi$Butterc[is.na(A_chi$Butterc)] <- 0
A_chi$Margarinec[is.na(A_chi$Margarinec)] <- 0
A_chi$Nutsc[is.na(A_chi$Nutsc)] <- 0
A_chi$Potatoesc[is.na(A_chi$Potatoesc)] <- 0
A_chi$Milkc[is.na(A_chi$Milkc)] <- 0
A_chi$Eggsc[is.na(A_chi$Eggsc)] <- 0
A_chi$Burgers.fastfoodc[is.na(A_chi$Burgers.fastfoodc)] <- 0
A_chi$Yakultc[is.na(A_chi$Yakultc)] <- 0

#convert 16 new columns to numeric
A_chi$Meatc <- as.numeric(A_chi$Meatc)
A_chi$Seafoodc <- as.numeric(A_chi$Seafoodc)
A_chi$Fruitsc <- as.numeric(A_chi$Fruitsc)
A_chi$Vegetablesc <- as.numeric(A_chi$Vegetablesc)
A_chi$Pulsesc <- as.numeric(A_chi$Pulsesc)
A_chi$Cerealsc <- as.numeric(A_chi$Cerealsc)
A_chi$Pastac <- as.numeric(A_chi$Pastac)
A_chi$Ricec <- as.numeric(A_chi$Ricec)
A_chi$Butterc <- as.numeric(A_chi$Butterc)
A_chi$Margarinec <- as.numeric(A_chi$Margarinec)
A_chi$Nutsc <- as.numeric(A_chi$Nutsc)
A_chi$Potatoesc <- as.numeric(A_chi$Potatoesc)
A_chi$Milkc <- as.numeric(A_chi$Milkc)
A_chi$Eggsc <- as.numeric(A_chi$Eggsc)
A_chi$Burgers.fastfoodc <- as.numeric(A_chi$Burgers.fastfoodc)
A_chi$Yakultc <- as.numeric(A_chi$Yakultc)


#create columns for dietary indices

#==========CARBOHYDRATES==============
A_chi$Carbohydrate <- (A_chi$Meatc*0.00 + A_chi$Seafoodc*3.11 + A_chi$Fruitsc*11.01 + A_chi$Vegetablesc*6.29  + A_chi$Pulsesc*9.55 + 
                         A_chi$Cerealsc*33.83  + A_chi$Pastac*26.54 + A_chi$Ricec*51.40 + A_chi$Butterc*0.70 + A_chi$Margarinec*0.50 +
                         A_chi$Nutsc*16.28 + A_chi$Potatoesc*37.59 + A_chi$Milkc*6.93 + A_chi$Eggsc*2.27 + A_chi$Burgers.fastfoodc*24.70 +
                         A_chi$Yakultc*10.10)
quantile(A_chi$Carbohydrate, probs =c(.25,.5,.75), na.rm = FALSE)
quantile(A_chi$Carbohydrate, probs =c(.68,.5,.95), na.rm = FALSE)
summary(A_chi$Carbohydrate)
sd(A_chi$Carbohydrate)

A_chi$Carbohydrates2 <- ifelse(A_chi$Carbohydrate >= 1000.2700  ,"High intake",
                               ifelse(A_chi$Carbohydrate <= 706.6275  ,"Low intake", "Moderate intake"))
A_chi$Carbohydrates2 <- factor(A_chi$Carbohydrates2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Carbohydrates2)

#===========FATS========================
A_chi$Fats <- (A_chi$Meatc*12.89 + A_chi$Seafoodc*3.77 + A_chi$Fruitsc*0.16 + A_chi$Vegetablesc*0.18 + A_chi$Pulsesc*0.20 + 
                 A_chi$Cerealsc*2.20 + A_chi$Pastac*5.55 + A_chi$Ricec*0.50 + A_chi$Butterc*82.00 + A_chi$Margarinec*63.88 + A_chi$Nutsc*50.50 + A_chi$Potatoesc*0.20 + 
                 A_chi$Milkc*3.32 + A_chi$Eggsc*13.23 + A_chi$Burgers.fastfoodc*14.68 + A_chi$Yakultc*0.00)
quantile(A_chi$Fats, probs =c(.25,.5,.75))
quantile(A_chi$Fats, probs =c(.68,.5,.95))
summary(A_chi$Fats)
sd(A_chi$Fats)

A_chi$Fats2 <- ifelse(A_chi$Fats <=  256.5375 ,"Low intake",
                      ifelse(A_chi$Fats >= 641.3200   ,"High intake", "Moderate intake"))
A_chi$Fats2 <- factor(A_chi$Fats2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Fats2)

#============PROTEINS===================
A_chi$Proteins <- (A_chi$Meatc*25.06 + A_chi$Seafoodc*22.73 + A_chi$Fruitsc*0.65 + A_chi$Vegetablesc*2.15 + A_chi$Pulsesc*2.48 +
                     A_chi$Cerealsc*9.68 + A_chi$Pastac*6.60 + A_chi$Ricec*2.56 + A_chi$Butterc*0.65 + A_chi$Margarinec*0.35 + A_chi$Nutsc*21.40 + A_chi$Potatoesc*1.98 +
                     A_chi$Milkc*3.97 + A_chi$Eggsc*12.20 + A_chi$Burgers.fastfoodc*10.50 + A_chi$Yakultc*1.20)
quantile(A_chi$Proteins, probs =c(.25,.5,.75))
quantile(A_chi$Proteins, probs =c(.68,.5,.95))
summary(A_chi$Proteins)
sd(A_chi$Proteins)

A_chi$Proteins2 <- ifelse(A_chi$Proteins <= 380.9700    ,"Low intake",
                          ifelse(A_chi$Proteins >= 560.5825     ,"High intake", "Moderate intake"))
A_chi$Proteins2 <- factor(A_chi$Proteins2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Proteins2)


#===============DIETARY FIBRE=====================
A_chi$DietaryFibre <- ( A_chi$Seafoodc*0.05 + A_chi$Fruitsc*2.07 + A_chi$Vegetablesc*2.23 + A_chi$Pulsesc*2.37 +
                          A_chi$Cerealsc*3.98 + A_chi$Pastac*1.92 + A_chi$Ricec*0.97 + A_chi$Butterc*0 + A_chi$Margarinec*0 +
                          A_chi$Nutsc*9.20 + A_chi$Potatoesc*2.65 + A_chi$Milkc*0.18 +
                          A_chi$Eggsc*0 + A_chi$Burgers.fastfoodc*2.21 + A_chi$Yakultc*0.70)

quantile(A_chi$DietaryFibre, probs =c(.25,.5,.75))
quantile(A_chi$DietaryFibre, probs =c(.68,.95))
summary(A_chi$DietaryFibre)
sd(A_chi$DietaryFibre)

A_chi$DietaryFibre2 <- ifelse(A_chi$DietaryFibre <=  61.00   ,"Low intake",
                              ifelse(A_chi$DietaryFibre >=  100.04 ,"High intake", "Moderate intake"))
A_chi$DietaryFibre2 <- factor(A_chi$DietaryFibre2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$DietaryFibre2)


#===============CHOLESTEROL============================
A_chi$Cholesterol <- ( A_chi$Meatc*73.80 + A_chi$Seafoodc*115.60 + A_chi$Fruitsc*0 + A_chi$Vegetablesc*0 +
                         A_chi$Pastac*14.24 + A_chi$Ricec*0 + A_chi$Butterc*240.0 + A_chi$Margarinec*0 +
                         A_chi$Nutsc*0 + A_chi$Potatoesc*0 +
                         A_chi$Milkc*10.07 + A_chi$Eggsc*551.33 + A_chi$Burgers.fastfoodc*19.78)

quantile(A_chi$Cholesterol, probs =c(.25,.5,.75), na.rm = TRUE)
quantile(A_chi$Cholesterol, probs =c(.68,.95))
summary(A_chi$Cholesterol)
sd(A_chi$Cholesterol)

A_chi$Cholesterol2 <- ifelse(A_chi$Cholesterol <=2398 , "Low intake",
                             ifelse(A_chi$Cholesterol >=5295  ,"High intake", "Moderate intake"))
A_chi$Cholesterol2 <- factor(A_chi$Cholesterol2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Cholesterol2)


#=============SUGAR==========================
A_chi$Sugar <- ( A_chi$Meatc*0 + A_chi$Seafoodc*0 + A_chi$Fruitsc*8.68 + A_chi$Vegetablesc*6.93 +
                   A_chi$Pastac*1.37 + A_chi$Ricec*0.05 + A_chi$Butterc*0.70 + A_chi$Margarinec*0.50 +
                   A_chi$Nutsc*4.97 + A_chi$Potatoesc*7.32 + A_chi$Cerealsc*6.90 +
                   A_chi$Milkc*7.28 + A_chi$Eggsc*0.24 + A_chi$Burgers.fastfoodc*4.80 + A_chi$Yakultc*10.10)

quantile(A_chi$Sugar, probs =c(.25,.5,.75))
quantile(A_chi$Sugar, probs =c(.68,.95))
summary(A_chi$Sugar)
sd(A_chi$Sugar)

A_chi$Sugar2 <- ifelse(A_chi$Sugar <=150.6 , "Low intake",
                       ifelse(A_chi$Sugar >= 239.1 ,"High intake", "Moderate intake"))
A_chi$Sugar2 <- factor(A_chi$Sugar2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Sugar2)

#================VITAMIN C=========================
A_chi$VitC <- (A_chi$Meatc*0.20 + A_chi$Seafoodc*2.96 + A_chi$Fruitsc*22.70 + A_chi$Vegetablesc*24.59 + A_chi$Pulsesc*18.40 +
                 A_chi$Cerealsc*1.86 + A_chi$Pastac*0 + A_chi$Ricec*0 + A_chi$Butterc*0 + A_chi$Margarinec*0 +
                 A_chi$Nutsc*14.26 + A_chi$Potatoesc*15.05 + A_chi$Milkc*1.53 +
                 A_chi$Eggsc*0 + A_chi$Burgers.fastfoodc*0.20 + A_chi$Yakultc*0.40)

quantile(A_chi$VitC, probs =c(.25,.5,.75))
quantile(A_chi$VitC, probs =c(.68,.95))
summary(A_chi$VitC)
sd(A_chi$VitC)

A_chi$VitC2 <- ifelse(A_chi$VitC <= 314.5  ,"Low intake",
                      ifelse(A_chi$VitC >= 467.9 ,"High intake", "Moderate intake"))
A_chi$VitC2 <- factor(A_chi$VitC2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$VitC2)

A_chi$VitC3 <- ifelse(A_chi$VitC2 == "Low intake" ,"Low intake",
                      ifelse(A_chi$VitC2 == "Moderate intake","Moderate-High intake",
                             ifelse(A_chi$VitC2 == "High intake","Moderate-High intake",NA)))
A_chi$VitC3 <- factor(A_chi$VitC3 ,levels= c("Low intake", "Moderate-High intake"))
table(A_chi$VitC3)

#=====================CALCIUM========================
A_chi$Calcium <- (A_chi$Meatc*10.80 + A_chi$Seafoodc*107.60 + A_chi$Fruitsc*12.67 + A_chi$Vegetablesc*46.17 + A_chi$Pulsesc*48.0+
                    A_chi$Cerealsc*136.75 + A_chi$Pastac*29.81 + A_chi$Ricec*3.0 + A_chi$Butterc*17.50 + A_chi$Margarinec*1.50 +
                    A_chi$Nutsc*115.20 + A_chi$Potatoesc*15.50 + A_chi$Milkc*117.67 +
                    A_chi$Eggsc*51.0 + A_chi$Burgers.fastfoodc*50.13 + A_chi$Yakultc*79.0)

quantile(A_chi$Calcium, probs =c(.25,.5,.75))
quantile(A_chi$Calcium, probs =c(.68,.95))
summary(A_chi$Calcium)
sd(A_chi$Calcium)

A_chi$Calcium2 <- ifelse(A_chi$Calcium <= 2017 ,"Low intake",
                         ifelse(A_chi$Calcium >= 3382 ,"High intake", "Moderate intake"))
A_chi$Calcium2 <- factor(A_chi$Calcium2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Calcium2)


#=====================SODIUM========================
A_chi$Sodium <- (A_chi$Meatc*137.40 + A_chi$Seafoodc*464.80 + A_chi$Fruitsc*2.83 + A_chi$Vegetablesc*53.62 + A_chi$Pulsesc*5.0+
                   A_chi$Cerealsc*515.0 + A_chi$Pastac*215.10 + A_chi$Ricec*4.33 + A_chi$Butterc*310.0 + A_chi$Margarinec*523.50 +
                   A_chi$Nutsc*5.60 + A_chi$Potatoesc*10.50 + A_chi$Milkc*44.17 +
                   A_chi$Eggsc*106.20 + A_chi$Burgers.fastfoodc*440.80 + A_chi$Yakultc*41.0)

quantile(A_chi$Sodium, probs =c(.25,.5,.75))
quantile(A_chi$Sodium, probs =c(.68,.95))
summary(A_chi$Sodium)
sd(A_chi$Sodium)

A_chi$Sodium2 <- ifelse(A_chi$Sodium <=6089 ,"Low intake",
                        ifelse(A_chi$Sodium >= 10364,"High intake", "Moderate intake"))
A_chi$Sodium2 <- factor(A_chi$Sodium2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Sodium2)


#=================IRON=============================
A_chi$Iron <- (A_chi$Meatc*1.91 + A_chi$Seafoodc*2.73 + A_chi$Fruitsc*0.27 + A_chi$Vegetablesc*1.23 + A_chi$Pulsesc*0.99+
                 A_chi$Cerealsc*8.25 + A_chi$Pastac*1.0 + A_chi$Ricec*0.26 + A_chi$Butterc*0 + A_chi$Margarinec*0 +
                 A_chi$Nutsc*4.87 + A_chi$Potatoesc*0.98 + A_chi$Milkc*0.17 +
                 A_chi$Eggsc*3.13 + A_chi$Burgers.fastfoodc*1.25 + A_chi$Yakultc*1.30)

quantile(A_chi$Iron, probs =c(.25,.5,.75))
quantile(A_chi$Iron, probs =c(.68,.95))
summary(A_chi$Iron)
sd(A_chi$Iron)

A_chi$Iron2 <- ifelse(A_chi$Iron <=75.32  ,"Low intake",
                      ifelse(A_chi$Iron >=129.93  ,"High intake", "Moderate intake"))
A_chi$Iron2 <- factor(A_chi$Iron2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$Iron2)

# =================Vitamin A========================
A_chi$VitA <- (A_chi$Meatc*10.2 + A_chi$Seafoodc*67 + A_chi$Fruitsc*41.83 + A_chi$Vegetablesc*480.85 + A_chi$Pulsesc*47.67+
                 A_chi$Cerealsc*22.75 + A_chi$Pastac*35.96 + A_chi$Ricec*0 + A_chi$Butterc*1082.5 + A_chi$Margarinec*1106 +
                 A_chi$Nutsc*16 + A_chi$Potatoesc*0 + A_chi$Milkc*95.5 +
                 A_chi$Eggsc*267.67 + A_chi$Burgers.fastfoodc*16.64 + A_chi$Yakultc*39)

quantile(A_chi$VitA, probs =c(.25,.5,.75))
quantile(A_chi$VitA, probs =c(.68,.95))
summary(A_chi$VitA)
sd(A_chi$VitA)

A_chi$VitA2 <- ifelse(A_chi$VitA <= 5634  ,"Low intake",
                      ifelse(A_chi$VitA >=10711,"High intake", "Moderate intake"))
A_chi$VitA2 <- factor(A_chi$VitA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$VitA2)

#===============Vitamin D====================
A_chi$VitD <- (A_chi$Meatc*8.6 + A_chi$Seafoodc*30.34 + A_chi$Fruitsc*0 +
                 A_chi$Cerealsc*1.5 + A_chi$Pastac*0 + A_chi$Ricec*0 + A_chi$Butterc*44 + A_chi$Margarinec*347 +
                 A_chi$Nutsc*0 + A_chi$Potatoesc*0.98 + A_chi$Milkc*26.36 +
                 A_chi$Eggsc*0 + A_chi$Burgers.fastfoodc*0 )

quantile(A_chi$VitD, probs =c(.25,.5,.75))
quantile(A_chi$VitD, probs =c(.68,.95))
summary(A_chi$VitD)
sd(A_chi$VitD)

A_chi$VitD2 <- ifelse(A_chi$VitD <= 274.1 ,"Low intake",
                      ifelse(A_chi$VitD >=1099.9,"High intake", "Moderate intake"))
A_chi$VitD2 <- factor(A_chi$VitD2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$VitD2)

#===========================LOG REGRESSION======================================
m<-glm(AScase32 ~ VitD2 , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m))
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)

m<-glm(AScase43 ~ VitD2 + Gender + Parentalhistory + Age + BMI  , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m)) 
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)

