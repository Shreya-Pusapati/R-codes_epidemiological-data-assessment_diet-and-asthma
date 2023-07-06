
#Separated Animal and plant protein indices

#------------------------Animal protein-----------------------------------
# Categorizing the food groups 
A_proteins<-c('Meat', 'Seafood', 'Milk', 'Eggs','Burgers/fast.food','Butter')
summary(A_chi[,c(A_proteins)])

# Assigning scores. 

for(.i in 1:nrow(A_chi))
{.hi1<-rowSums(A_chi[.i, A_proteins]==1)
.hi2<-rowSums(A_chi[.i, A_proteins]==2)
.hi3<-rowSums(A_chi[.i, A_proteins]==3)
A_chi[.i, 'DAP']<-(2*.hi2)+(7*.hi3)}

#Find out the 33/66th cut-offs
# Forming the score column based on the 33/66th cut-offs 
# Carry out your logistic regression analyses

quantile(A_chi$DAP, probs =c(.33,.66), na.rm = TRUE)
A_chi$DAP<-as.numeric(A_chi$DAP)
summary(A_chi$DAP)

#categorizing into low, high and moderate 
A_chi$DAP.CAT<-with(A_chi, ifelse(DAP>=25, 'High', 
                                   ifelse(DAP>= 18 & DAP<= 24, 'Moderate',
                                          ifelse(DAP<= 17, 'Low',NA))))
A_chi$DAP.CAT<-as.factor(A_chi$DAP.CAT)
summary(A_chi$DAP.CAT)
A_chi$DAP.CAT<-relevel(A_chi$DAP.CAT,ref = "Low") # make low the reference.
summary(A_chi$DAP.CAT)

#----------------Plant Protein score indice----------------------------
# Categorizing the food groups 
P_proteins<-c('Cereals', 'Rice', 'Pulses','Vegetables','Potatoes','Fruits','Nuts','Pasta') 
summary(A_chi[,c(P_proteins)])

# Assigning scores. 

for(.i in 1:nrow(A_chi))
{.lo1<-rowSums(A_chi[.i, P_proteins]==1)
.lo2<-rowSums(A_chi[.i, P_proteins]==2)
.lo3<-rowSums(A_chi[.i, P_proteins]==3)
A_chi[.i, 'DPP']<-(2*.lo2)+(7*.lo3)}

#Find out the 33/66th cut-offs
# Forming the score column based on the 33/66th cut-offs 
# Carry out your logistic regression analyses

quantile(A_chi$DPP, probs =c(.33,.66), na.rm = TRUE)
A_chi$DPP<-as.numeric(A_chi$DPP)
summary(A_chi$DPP)

#categorizing into low, high and moderate 
A_chi$DPP.CAT<-with(A_chi, ifelse(DPP>=34, 'High', 
                                   ifelse(DPP>=27 & DPP<= 33, 'Moderate',
                                          ifelse(DPP<= 26, 'Low',NA))))
A_chi$DPP.CAT<-as.factor(A_chi$DPP.CAT)
summary(A_chi$DPP.CAT)
A_chi$DPP.CAT<-relevel(A_chi$DPP.CAT,ref = "Low") # make low the reference.
summary(A_chi$DPP.CAT)

# COMBINED ANIMAL AND PLANT PROTEIN SCORE INDICE--------------------------------------------------
A_chi$ConAnalysis4 <- ifelse(A_chi$DAP.CAT == "High" & A_chi$DPP.CAT == "High", "HighAP&PP",
                             ifelse(A_chi$DAP.CAT == "High" & A_chi$DPP.CAT == "Moderate", "HighAPlowPP",
                                    ifelse(A_chi$DAP.CAT == "High" & A_chi$DPP.CAT == "Low", "HighAPlowPP",
                                           ifelse(A_chi$DAP.CAT == "Moderate" & A_chi$DPP.CAT == "High", "ModAPhighPP",
                                                  ifelse(A_chi$DAP.CAT == "Moderate" & A_chi$DPP.CAT == "Moderate", "ModAP&PP",
                                                         ifelse(A_chi$DAP.CAT == "Moderate" & A_chi$DPP.CAT == "Low", "ModAPlowPP",
                                                                ifelse(A_chi$DAP.CAT == "Low" & A_chi$DPP.CAT == "High", "LowAPhighPP",
                                                                       ifelse(A_chi$DAP.CAT == "Low" & A_chi$DPP.CAT == "Moderate", "LowAPhighPP",
                                                                              ifelse(A_chi$DAP.CAT == "Low" & A_chi$DPP.CAT == "Low", "Low", NA)))))))))
A_chi$ConAnalysis4 <- as.factor(A_chi$ConAnalysis4)
A_chi$ConAnalysis4 <- relevel(A_chi$ConAnalysis4, ref = "HighAPlowPP") # the ref is not LOW; trying to compare against the bad diet.
summary(A_chi$ConAnalysis4)

# -----------------------------LOGISTIC REGRESSION TEMPLATE-------------------------------------------
m<-glm(mild ~ ConAnalysis4 , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m))
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)

m<-glm(mild ~ ConAnalysis4 + Gender + Parentalhistory + Age + BMI  , data=A_chi, family=binomial(link='logit'))
summary(m)
exp(m$coefficients) 
exp(confint(m)) 
round(exp(m$coefficients), digits = 3)
round(exp(confint(m)), digits = 3)