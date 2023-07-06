# Types of FATS- Amount indices

#----------Linoleic acid---------------------18:2 PUFA
A_chi$LA <- (A_chi$Meatc*1.25 + A_chi$Seafoodc*0.736 + A_chi$Fruitsc*0.0478 + A_chi$Vegetablesc*0.0457  + A_chi$Pulsesc*0.105 + 
             A_chi$Cerealsc*1.25  + A_chi$Pastac*0.420 + A_chi$Ricec*0.132 + A_chi$Butterc*2.73 + A_chi$Margarinec*17.9 + A_chi$Nutsc*10.6 + 
               A_chi$Potatoesc*0.0520 + A_chi$Milkc*0.280 + A_chi$Eggsc*1.06 + A_chi$Burgers.fastfoodc*4.63 + A_chi$Yakultc*0.00)
quantile(A_chi$LA, probs =c(.33,.66))
summary(A_chi$LA)
sd(A_chi$LA)

A_chi$LA2 <- ifelse(A_chi$LA >= 88.7818  ,"High intake",
                               ifelse(A_chi$LA <= 44.7220   ,"Low intake", "Moderate intake"))
A_chi$LA2 <- factor(A_chi$LA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$LA2)

#-----------Alpha-linoleic acid--------------18:3 PUFA
A_chi$ALA <- (A_chi$Meatc*0.120 + A_chi$Seafoodc*0.0477 + A_chi$Fruitsc*0.0167 + A_chi$Vegetablesc*0.0310  + A_chi$Pulsesc*0.0975 + 
               A_chi$Cerealsc*0.120  + A_chi$Pastac*0.0340 + A_chi$Ricec*0.00870 + A_chi$Butterc*0.315 + A_chi$Margarinec*2.67 + A_chi$Nutsc*0.131 + 
               A_chi$Potatoesc*0.00650 + A_chi$Milkc*0.0620 + A_chi$Eggsc*0.180 + A_chi$Burgers.fastfoodc*0.410 + A_chi$Yakultc*0.00)
quantile(A_chi$ALA, probs =c(.33,.66))
summary(A_chi$ALA)
sd(A_chi$ALA)

A_chi$ALA2 <- ifelse(A_chi$ALA >= 9.54870   ,"High intake",
                    ifelse(A_chi$ALA <= 4.10152    ,"Low intake", "Moderate intake"))
A_chi$ALA2 <- factor(A_chi$ALA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$ALA2)

#----------Trans fatty acid----------------
A_chi$TFA <- (A_chi$Meatc*0.514 + A_chi$Seafoodc*0.00350 + A_chi$Fruitsc*0.00 + A_chi$Vegetablesc*0.00  + A_chi$Pulsesc*0.00 + 
                A_chi$Cerealsc*0.0210  + A_chi$Pastac*0.0404 + A_chi$Ricec*0.00 + A_chi$Butterc*3.28 + A_chi$Margarinec*6.86 + A_chi$Nutsc*0.0350 + 
                A_chi$Potatoesc*0.00 + A_chi$Milkc*0.185 + A_chi$Eggsc*0.0380 + A_chi$Burgers.fastfoodc*0.180 + A_chi$Yakultc*0.00)
quantile(A_chi$TFA, probs =c(.33,.66))
summary(A_chi$TFA)
sd(A_chi$TFA)

A_chi$TFA2 <- ifelse(A_chi$TFA >= 24.8263    ,"High intake",
                     ifelse(A_chi$TFA <= 5.3463     ,"Low intake", "Moderate intake"))
A_chi$TFA2 <- factor(A_chi$TFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$TFA2)

#---------Saturated fatty acids-----------
A_chi$SFA <- (A_chi$Meatc*6.43 + A_chi$Seafoodc*0.717 + A_chi$Fruitsc*0.0540 + A_chi$Vegetablesc*0.0359  + A_chi$Pulsesc*0.0742 + 
                A_chi$Cerealsc*0.113  + A_chi$Pastac*0.380 + A_chi$Ricec*0.0870 + A_chi$Butterc*51.0 + A_chi$Margarinec*12.7 + A_chi$Nutsc*7.04 + 
                A_chi$Potatoesc*0.0325 + A_chi$Milkc*1.50 + A_chi$Eggsc*3.40 + A_chi$Burgers.fastfoodc*3.93 + A_chi$Yakultc*0.00)
quantile(A_chi$SFA, probs =c(.33,.66))
summary(A_chi$SFA)
sd(A_chi$SFA)

A_chi$SFA2 <- ifelse(A_chi$SFA >= 208.31670 ,"High intake",
                     ifelse(A_chi$SFA <= 91.33558  ,"Low intake", "Moderate intake"))
A_chi$SFA2 <- factor(A_chi$SFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$SFA2)

#---------Mono-unsaturated fatty acids------
A_chi$MUFA <- (A_chi$Meatc*7.64 + A_chi$Seafoodc*1.07 + A_chi$Fruitsc*0.0480 + A_chi$Vegetablesc*0.0190  + A_chi$Pulsesc*0.0520 + 
                A_chi$Cerealsc*0.580  + A_chi$Pastac*0.409 + A_chi$Ricec*0.145 + A_chi$Butterc*21.7 + A_chi$Margarinec*27.7 + A_chi$Nutsc*34.7 + 
                A_chi$Potatoesc*0.00150 + A_chi$Milkc*0.830 + A_chi$Eggsc*4.89 + A_chi$Burgers.fastfoodc*5.77 + A_chi$Yakultc*0.00)
quantile(A_chi$MUFA, probs =c(.33,.66))
summary(A_chi$MUFA)
sd(A_chi$MUFA)

A_chi$MUFA2 <- ifelse(A_chi$MUFA >= 249.6863  ,"High intake",
                     ifelse(A_chi$MUFA <= 144.4445   ,"Low intake", "Moderate intake"))
A_chi$MUFA2 <- factor(A_chi$MUFA2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$MUFA2)

#----------------Total Fats--------------------
A_chi$TotF <- (A_chi$Meatc*17.4 + A_chi$Seafoodc*3.22 + A_chi$Fruitsc*0.225 + A_chi$Vegetablesc*0.191  + A_chi$Pulsesc*0.435 + 
                 A_chi$Cerealsc*3.38  + A_chi$Pastac*1.57 + A_chi$Ricec*0.410 + A_chi$Butterc*81.1 + A_chi$Margarinec*65.2 + A_chi$Nutsc*54.4 + 
                 A_chi$Potatoesc*0.135 + A_chi$Milkc*2.84 + A_chi$Eggsc*11.6 + A_chi$Burgers.fastfoodc*16.1 + A_chi$Yakultc*0.00)
quantile(A_chi$TotF, probs =c(.33,.66))
summary(A_chi$TotF)
sd(A_chi$TotF)

A_chi$TotF2 <- ifelse(A_chi$TotF >= 607.5420   ,"High intake",
                      ifelse(A_chi$TotF <= 337.0127    ,"Low intake", "Moderate intake"))
A_chi$TotF2 <- factor(A_chi$TotF2 ,levels= c("Low intake", "Moderate intake", "High intake"))
table(A_chi$TotF2)

#------------Concordant analysis---------------
#-----LA and Total Fats------------------------
A_chi$ToF1 <- ifelse(A_chi$LA2 == "High intake" & A_chi$TotF2 == "High intake", "High",
                             ifelse(A_chi$LA2 == "High intake" & A_chi$TotF2 == "Moderate intake", "HighLAlowTotF",
                                    ifelse(A_chi$LA2 == "High intake" & A_chi$TotF2 == "Low intake", "HighLAlowTotF",
                                           ifelse(A_chi$LA2 == "Moderate intake" & A_chi$TotF2 == "High intake", "ModLAhighTotF",
                                                  ifelse(A_chi$LA2 == "Moderate intake" & A_chi$TotF2 == "Moderate intake", "Mod",
                                                         ifelse(A_chi$LA2 == "Moderate intake" & A_chi$TotF2 == "Low intake", "ModLAlowTotF",
                                                                ifelse(A_chi$LA2 == "Low intake" & A_chi$TotF2 == "High intake", "LowLAhighTotF",
                                                                       ifelse(A_chi$LA2 == "Low intake" & A_chi$TotF2 == "Moderate intake", "LowLAhighTotF",
                                                                              ifelse(A_chi$LA2 == "Low intake" & A_chi$TotF2 == "Low intake", "Low", NA)))))))))
A_chi$ToF1 <- as.factor(A_chi$ToF1)
A_chi$ToF1 <- relevel(A_chi$ToF1, ref = "Low")
summary(A_chi$ToF1)

addmargins(table(A_chi$ToF1, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$ToF1, A_chi$eia, exclude = NULL))

#-----ALA and Total Fats---------------------------------
A_chi$ToF2 <- ifelse(A_chi$ALA2 == "High intake" & A_chi$TotF2 == "High intake", "High",
                     ifelse(A_chi$ALA2 == "High intake" & A_chi$TotF2 == "Moderate intake", "HighALAlowTotF",
                            ifelse(A_chi$ALA2 == "High intake" & A_chi$TotF2 == "Low intake", "HighALAlowTotF",
                                   ifelse(A_chi$ALA2 == "Moderate intake" & A_chi$TotF2 == "High intake", "ModALAhighTotF",
                                          ifelse(A_chi$ALA2 == "Moderate intake" & A_chi$TotF2 == "Moderate intake", "Mod",
                                                 ifelse(A_chi$ALA2 == "Moderate intake" & A_chi$TotF2 == "Low intake", "ModALAlowTotF",
                                                        ifelse(A_chi$ALA2 == "Low intake" & A_chi$TotF2 == "High intake", "LowALAhighTotF",
                                                               ifelse(A_chi$ALA2 == "Low intake" & A_chi$TotF2 == "Moderate intake", "LowALAhighTotF",
                                                                      ifelse(A_chi$ALA2 == "Low intake" & A_chi$TotF2 == "Low intake", "Low", NA)))))))))
A_chi$ToF2 <- as.factor(A_chi$ToF2)
A_chi$ToF2 <- relevel(A_chi$ToF2, ref = "Low")
summary(A_chi$ToF2)

addmargins(table(A_chi$ToF2, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$ToF2, A_chi$eia, exclude = NULL))

#-----TFA and Total Fats----------------------------------
A_chi$ToF3 <- ifelse(A_chi$TFA2 == "High intake" & A_chi$TotF2 == "High intake", "High",
                     ifelse(A_chi$TFA2 == "High intake" & A_chi$TotF2 == "Moderate intake", "HighTFAlowTotF",
                            ifelse(A_chi$TFA2 == "High intake" & A_chi$TotF2 == "Low intake", "HighTFAlowTotF",
                                   ifelse(A_chi$TFA2 == "Moderate intake" & A_chi$TotF2 == "High intake", "ModTFAhighTotF",
                                          ifelse(A_chi$TFA2 == "Moderate intake" & A_chi$TotF2 == "Moderate intake", "Mod",
                                                 ifelse(A_chi$TFA2 == "Moderate intake" & A_chi$TotF2 == "Low intake", "ModTFAlowTotF",
                                                        ifelse(A_chi$TFA2 == "Low intake" & A_chi$TotF2 == "High intake", "LowTFAhighTotF",
                                                               ifelse(A_chi$TFA2 == "Low intake" & A_chi$TotF2 == "Moderate intake", "LowTFAhighTotF",
                                                                      ifelse(A_chi$TFA2 == "Low intake" & A_chi$TotF2 == "Low intake", "Low", NA)))))))))
A_chi$ToF3 <- as.factor(A_chi$ToF3)
A_chi$ToF3 <- relevel(A_chi$ToF3, ref = "Low")
summary(A_chi$ToF3)

addmargins(table(A_chi$ToF3, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$ToF3, A_chi$eia, exclude = NULL))

#-----SFA and Total Fats-------------------------------
A_chi$ToF4 <- ifelse(A_chi$SFA2 == "High intake" & A_chi$TotF2 == "High intake", "High",
                     ifelse(A_chi$SFA2 == "High intake" & A_chi$TotF2 == "Moderate intake", "HighSFAlowTotF",
                            ifelse(A_chi$SFA2 == "High intake" & A_chi$TotF2 == "Low intake", "HighSFAlowTotF",
                                   ifelse(A_chi$SFA2 == "Moderate intake" & A_chi$TotF2 == "High intake", "ModSFAhighTotF",
                                          ifelse(A_chi$SFA2 == "Moderate intake" & A_chi$TotF2 == "Moderate intake", "Mod",
                                                 ifelse(A_chi$SFA2 == "Moderate intake" & A_chi$TotF2 == "Low intake", "ModSFAlowTotF",
                                                        ifelse(A_chi$SFA2 == "Low intake" & A_chi$TotF2 == "High intake", "LowSFAhighTotF",
                                                               ifelse(A_chi$SFA2 == "Low intake" & A_chi$TotF2 == "Moderate intake", "LowSFAhighTotF",
                                                                      ifelse(A_chi$SFA2 == "Low intake" & A_chi$TotF2 == "Low intake", "Low", NA)))))))))
A_chi$ToF4 <- as.factor(A_chi$ToF4)
A_chi$ToF4 <- relevel(A_chi$ToF4, ref = "Low")
summary(A_chi$ToF4)

addmargins(table(A_chi$ToF4, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$ToF4, A_chi$eia, exclude = NULL))

#-----MUFA and Total Fats------------------------
A_chi$ToF5 <- ifelse(A_chi$MUFA2 == "High intake" & A_chi$TotF2 == "High intake", "High",
                     ifelse(A_chi$MUFA2 == "High intake" & A_chi$TotF2 == "Moderate intake", "HighMUFAlowTotF",
                            ifelse(A_chi$MUFA2 == "High intake" & A_chi$TotF2 == "Low intake", "HighMUFAlowTotF",
                                   ifelse(A_chi$MUFA2 == "Moderate intake" & A_chi$TotF2 == "High intake", "ModMUFAhighTotF",
                                          ifelse(A_chi$MUFA2 == "Moderate intake" & A_chi$TotF2 == "Moderate intake", "Mod",
                                                 ifelse(A_chi$MUFA2 == "Moderate intake" & A_chi$TotF2 == "Low intake", "ModMUFAlowTotF",
                                                        ifelse(A_chi$MUFA2 == "Low intake" & A_chi$TotF2 == "High intake", "LowMUFAhighTotF",
                                                               ifelse(A_chi$MUFA2 == "Low intake" & A_chi$TotF2 == "Moderate intake", "LowMUFAhighTotF",
                                                                      ifelse(A_chi$MUFA2 == "Low intake" & A_chi$TotF2 == "Low intake", "Low", NA)))))))))
A_chi$ToF5 <- as.factor(A_chi$ToF5)
A_chi$ToF5 <- relevel(A_chi$ToF5, ref = "Low")
summary(A_chi$ToF5)

addmargins(table(A_chi$ConAnalysis4, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$ConAnalysis4, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$ConAnalysis4, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$ConAnalysis4, A_chi$AScase43, exclude = NULL))

# LOGISTIC REGRESSION TEMPLATE
m<-glm(AScase43 ~ ToF5 , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m))
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)

m<-glm(AScase43 ~ ToF5 + Gender + Parentalhistory + Age + BMI  , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m)) 
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)
