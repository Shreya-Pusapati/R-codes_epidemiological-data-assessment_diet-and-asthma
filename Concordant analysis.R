# CONCORDANT ANALYSIS

#---------------Fat and Protein-------------------
A_chi$ConAnalysis1 <- ifelse(A_chi$Fats2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                             ifelse(A_chi$Fats2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighFlowP",
                                    ifelse(A_chi$Fats2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighFlowP",
                                           ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModFhighP",
                                                  ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                         ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModFlowP",
                                                                ifelse(A_chi$Fats2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowFhighP",
                                                                       ifelse(A_chi$Fats2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowFhighP",
                                                                              ifelse(A_chi$Fats2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$ConAnalysis1 <- as.factor(A_chi$ConAnalysis1)
A_chi$ConAnalysis1 <- relevel(A_chi$ConAnalysis1, ref = "Low")
summary(A_chi$ConAnalysis1)
# -------CODE FOR GENERATING CASE CONTROL NUMBERS FOR SF CALCULATOR-------------------
addmargins(table(A_chi$ConAnalysis1, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$ConAnalysis1, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$ConAnalysis1, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$ConAnalysis1, A_chi$AScase43, exclude = NULL))


# -------------------Fat and Carbohydrates---------------------
A_chi$ConAnalysis2 <- ifelse(A_chi$Fats2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                             ifelse(A_chi$Fats2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighFlowC",
                                    ifelse(A_chi$Fats2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighFlowC",
                                           ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModFhighC",
                                                  ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                         ifelse(A_chi$Fats2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModFlowC",
                                                                ifelse(A_chi$Fats2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowFhighC",
                                                                       ifelse(A_chi$Fats2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowFhighC",
                                                                              ifelse(A_chi$Fats2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$ConAnalysis2 <- as.factor(A_chi$ConAnalysis2)
A_chi$ConAnalysis2 <- relevel(A_chi$ConAnalysis2, ref = "Low")
summary(A_chi$ConAnalysis2)

addmargins(table(A_chi$ConAnalysis2, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$ConAnalysis2, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$ConAnalysis2, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$ConAnalysis2, A_chi$AScase43, exclude = NULL))


#-------------------Carbohydrates & Proteins----------------------------
A_chi$ConAnalysis3 <- ifelse(A_chi$Carbohydrates2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                             ifelse(A_chi$Carbohydrates2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighClowP",
                                    ifelse(A_chi$Carbohydrates2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighClowP",
                                           ifelse(A_chi$Carbohydrates2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModChighP",
                                                  ifelse(A_chi$Carbohydrates2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                         ifelse(A_chi$Carbohydrates2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModClowP",
                                                                ifelse(A_chi$Carbohydrates2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowChighP",
                                                                       ifelse(A_chi$Carbohydrates2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowChighP",
                                                                              ifelse(A_chi$Carbohydrates2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$ConAnalysis3 <- as.factor(A_chi$ConAnalysis3)
A_chi$ConAnalysis3 <- relevel(A_chi$ConAnalysis3, ref = "Low")
summary(A_chi$ConAnalysis3)


addmargins(table(A_chi$ConAnalysis3, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$ConAnalysis3, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$ConAnalysis3, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$ConAnalysis3, A_chi$AScase43, exclude = NULL))

#----------------------SODIUM-----------------------------------
A_chi$SodxProtein <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                            ifelse(A_chi$Sodium2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighSlowP",
                                   ifelse(A_chi$Sodium2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighSlowP",
                                          ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModShighP",
                                                 ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                        ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModSlowP",
                                                               ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowShighP",
                                                                      ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowShighP",
                                                                             ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$SodxProtein <- as.factor(A_chi$SodxProtein)
A_chi$SodxProtein <- relevel(A_chi$SodxProtein, ref = "Low")
summary(A_chi$SodxProtein)


addmargins(table(A_chi$SodxProtein, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxProtein, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxProtein, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxProtein, A_chi$AScase43, exclude = NULL))



A_chi$SodxFat <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$Fats2 == "High intake", "High",
                        ifelse(A_chi$Sodium2 == "High intake" & A_chi$Fats2 == "Moderate intake", "HighSlowF",
                               ifelse(A_chi$Sodium2 == "High intake" & A_chi$Fats2 == "Low intake", "HighSlowF",
                                      ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Fats2 == "High intake", "ModShighF",
                                             ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Fats2 == "Moderate intake", "Mod",
                                                    ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Fats2 == "Low intake", "ModSlowF",
                                                           ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Fats2 == "High intake", "LowShighF",
                                                                  ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Fats2 == "Moderate intake", "LowShighF",
                                                                         ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Fats2 == "Low intake", "Low", NA)))))))))
A_chi$SodxFat <- as.factor(A_chi$SodxFat)
A_chi$SodxFat <- relevel(A_chi$SodxFat, ref = "Low")
summary(A_chi$SodxFat)


addmargins(table(A_chi$SodxFat, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxFat, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxFat, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxFat, A_chi$AScase43, exclude = NULL))


A_chi$SodxCarbs <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                          ifelse(A_chi$Sodium2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighSlowC",
                                 ifelse(A_chi$Sodium2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighSlowC",
                                        ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModShighC",
                                               ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                      ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModSlowC",
                                                             ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowShighC",
                                                                    ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowShighC",
                                                                           ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$SodxCarbs <- as.factor(A_chi$SodxCarbs)
A_chi$SodxCarbs <- relevel(A_chi$SodxCarbs, ref = "Low")
summary(A_chi$SodxCarbs)


addmargins(table(A_chi$SodxCarbs, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxCarbs, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxCarbs, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxCarbs, A_chi$AScase43, exclude = NULL))


A_chi$SodxCholesterol <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$Cholesterol2 == "High intake", "High",
                                ifelse(A_chi$Sodium2 == "High intake" & A_chi$Cholesterol2 == "Moderate intake", "HighSlowCho",
                                       ifelse(A_chi$Sodium2 == "High intake" & A_chi$Cholesterol2 == "Low intake", "HighSlowCho",
                                              ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Cholesterol2 == "High intake", "ModShighCho",
                                                     ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Cholesterol2 == "Moderate intake", "Mod",
                                                            ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$Cholesterol2 == "Low intake", "ModSlowCho",
                                                                   ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Cholesterol2 == "High intake", "LowShighCho",
                                                                          ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Cholesterol2 == "Moderate intake", "LowShighCho",
                                                                                 ifelse(A_chi$Sodium2 == "Low intake" & A_chi$Cholesterol2 == "Low intake", "Low", NA)))))))))
A_chi$SodxCholesterol <- as.factor(A_chi$SodxCholesterol)
A_chi$SodxCholesterol <- relevel(A_chi$SodxCholesterol, ref = "Low")
summary(A_chi$SodxCholesterol)


addmargins(table(A_chi$SodxCholesterol, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxCholesterol, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxCholesterol, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxCholesterol, A_chi$AScase43, exclude = NULL))


A_chi$SodxVitA <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitA2 == "High intake", "High",
                         ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitA2 == "Moderate intake", "HighSlowVitA",
                                ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitA2 == "Low intake", "HighSlowVitA",
                                       ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitA2 == "High intake", "ModShighVitA",
                                              ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitA2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitA2 == "Low intake", "ModSlowVitA",
                                                            ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitA2 == "High intake", "LowShighVitA",
                                                                   ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitA2 == "Moderate intake", "LowShighVitA",
                                                                          ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitA2 == "Low intake", "Low", NA)))))))))
A_chi$SodxVitA <- as.factor(A_chi$SodxVitA)
A_chi$SodxVitA <- relevel(A_chi$SodxVitA, ref = "Low")
summary(A_chi$SodxVitA)


addmargins(table(A_chi$SodxVitA, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxVitA, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxVitA, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxVitA, A_chi$AScase43, exclude = NULL))


A_chi$SodxVitD <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitD2 == "High intake", "High",
                         ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitD2 == "Moderate intake", "HighSlowVitD",
                                ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitD2 == "Low intake", "HighSlowVitD",
                                       ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitD2 == "High intake", "ModShighVitD",
                                              ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitD2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitD2 == "Low intake", "ModSlowVitD",
                                                            ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitD2 == "High intake", "LowShighVitD",
                                                                   ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitD2 == "Moderate intake", "LowShighVitD",
                                                                          ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitD2 == "Low intake", "Low", NA)))))))))
A_chi$SodxVitD <- as.factor(A_chi$SodxVitD)
A_chi$SodxVitD <- relevel(A_chi$SodxVitD, ref = "Low")
summary(A_chi$SodxVitD)


addmargins(table(A_chi$SodxVitD, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxVitD, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxVitD, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxVitD, A_chi$AScase43, exclude = NULL))


A_chi$SodxVitC <- ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitC2 == "High intake", "High",
                         ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitC2 == "Moderate intake", "HighSlowVitC",
                                ifelse(A_chi$Sodium2 == "High intake" & A_chi$VitC2 == "Low intake", "HighSlowVitC",
                                       ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitC2 == "High intake", "ModShighVitC",
                                              ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitC2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$Sodium2 == "Moderate intake" & A_chi$VitC2 == "Low intake", "ModSlowVitC",
                                                            ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitC2 == "High intake", "LowShighVitC",
                                                                   ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitC2 == "Moderate intake", "LowShighVitC",
                                                                          ifelse(A_chi$Sodium2 == "Low intake" & A_chi$VitC2 == "Low intake", "Low", NA)))))))))
A_chi$SodxVitC <- as.factor(A_chi$SodxVitC)
A_chi$SodxVitC <- relevel(A_chi$SodxVitC, ref = "Low")
summary(A_chi$SodxVitC)


addmargins(table(A_chi$SodxVitC, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$SodxVitC, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$SodxVitC, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$SodxVitC, A_chi$AScase43, exclude = NULL))


#------------------------------CHOLESTEROL-------------------------------------------
A_chi$CholesterolxProtein <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                                    ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighCholowP",
                                           ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighCholowP",
                                                  ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModChohighP",
                                                         ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                                ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModCholowP",
                                                                       ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowChohighP",
                                                                              ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowChohighP",
                                                                                     ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxProtein <- as.factor(A_chi$CholesterolxProtein)
A_chi$CholesterolxProtein <- relevel(A_chi$CholesterolxProtein, ref = "Low")
summary(A_chi$CholesterolxProtein)


addmargins(table(A_chi$CholesterolxProtein, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$CholesterolxProtein, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxProtein, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxProtein, A_chi$AScase43, exclude = NULL))


A_chi$CholesterolxFat <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Fats2 == "High intake", "High",
                                ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Fats2 == "Moderate intake", "HighCholowF",
                                       ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Fats2 == "Low intake", "HighCholowF",
                                              ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Fats2 == "High intake", "ModCholhighF",
                                                     ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Fats2 == "Moderate intake", "Mod",
                                                            ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Fats2 == "Low intake", "ModCholowF",
                                                                   ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Fats2 == "High intake", "LowChohighF",
                                                                          ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Fats2 == "Moderate intake", "LowChohighF",
                                                                                 ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Fats2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxFat <- as.factor(A_chi$CholesterolxFat)
A_chi$CholesterolxFat <- relevel(A_chi$CholesterolxFat, ref = "Low")
summary(A_chi$CholesterolxFat)


addmargins(table(A_chi$CholesterolxFat, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$CholesterolxFat, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxFat, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxFat, A_chi$AScase43, exclude = NULL))


A_chi$CholesterolxCarbs <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                                  ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighCholowC",
                                         ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighCholowC",
                                                ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModCholhighC",
                                                       ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                              ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModCholowC",
                                                                     ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowChohighC",
                                                                            ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowChohighC",
                                                                                   ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxCarbs <- as.factor(A_chi$CholesterolxCarbs)
A_chi$CholesterolxCarbs <- relevel(A_chi$CholesterolxCarbs, ref = "Low")
summary(A_chi$CholesterolxCarbs)


addmargins(table(A_chi$CholesterolxCarbs, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$CholesterolxCarbs, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxCarbs, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxCarbs, A_chi$AScase43, exclude = NULL))


#did not use
A_chi$CholesterolxVitA <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitA2 == "High intake", "High",
                                 ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitA2 == "Moderate intake", "HighCholowVitA",
                                        ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitA2 == "Low intake", "HighCholowVitA",
                                               ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitA2 == "High intake", "ModCholhighVitA",
                                                      ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitA2 == "Moderate intake", "Mod",
                                                             ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitA2 == "Low intake", "ModCholowVitA",
                                                                    ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitA2 == "High intake", "LowChohighVitA",
                                                                           ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitA2 == "Moderate intake", "LowChohighVitA",
                                                                                  ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitA2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxVitA <- as.factor(A_chi$CholesterolxVitA)
A_chi$CholesterolxVitA <- relevel(A_chi$CholesterolxVitA, ref = "Low")
summary(A_chi$CholesterolxVitA)


addmargins(table(A_chi$CholesterolxVitA, A_chi$ever.asthma, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$AScase43, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$com4, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$com6, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitA, A_chi$com5, exclude = NULL))
# did not use 
A_chi$CholesterolxVitD <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitD2 == "High intake", "High",
                                 ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitD2 == "Moderate intake", "HighCholowVitD",
                                        ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitD2 == "Low intake", "HighCholowVitD",
                                               ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitD2 == "High intake", "ModCholhighVitD",
                                                      ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitD2 == "Moderate intake", "Mod",
                                                             ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitD2 == "Low intake", "ModCholowVitD",
                                                                    ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitD2 == "High intake", "LowChohighVitD",
                                                                           ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitD2 == "Moderate intake", "LowChohighVitD",
                                                                                  ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitD2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxVitD <- as.factor(A_chi$CholesterolxVitD)
A_chi$CholesterolxVitD <- relevel(A_chi$CholesterolxVitD, ref = "Low")
summary(A_chi$CholesterolxVitD)


addmargins(table(A_chi$CholesterolxVitD, A_chi$ever.asthma, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$AScase43, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$com4, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$com6, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitD, A_chi$com5, exclude = NULL))

A_chi$CholesterolxVitC <- ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitC2 == "High intake", "High",
                                 ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitC2 == "Moderate intake", "HighCholowVitC",
                                        ifelse(A_chi$Cholesterol2 == "High intake" & A_chi$VitC2 == "Low intake", "HighCholowVitC",
                                               ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitC2 == "High intake", "ModCholhighVitC",
                                                      ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitC2 == "Moderate intake", "Mod",
                                                             ifelse(A_chi$Cholesterol2 == "Moderate intake" & A_chi$VitC2 == "Low intake", "ModCholowVitC",
                                                                    ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitC2 == "High intake", "LowChohighVitC",
                                                                           ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitC2 == "Moderate intake", "LowChohighVitC",
                                                                                  ifelse(A_chi$Cholesterol2 == "Low intake" & A_chi$VitC2 == "Low intake", "Low", NA)))))))))
A_chi$CholesterolxVitC <- as.factor(A_chi$CholesterolxVitC)
A_chi$CholesterolxVitC <- relevel(A_chi$CholesterolxVitC, ref = "Low")
summary(A_chi$CholesterolxVitC)


addmargins(table(A_chi$CholesterolxVitC, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitC, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitC, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$CholesterolxVitC, A_chi$AScase43, exclude = NULL))


#-------------------------------------VITAMIN C------------------------------------
A_chi$VitCxCarbs <- ifelse(A_chi$VitC2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                           ifelse(A_chi$VitC2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighVclowC",
                                  ifelse(A_chi$VitC2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighVclowC",
                                         ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModVchighC",
                                                ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                       ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModVclowC",
                                                              ifelse(A_chi$VitC2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowVchighC",
                                                                     ifelse(A_chi$VitC2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowVchighC",
                                                                            ifelse(A_chi$VitC2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$VitCxCarbs <- as.factor(A_chi$VitCxCarbs)
A_chi$VitCxCarbs <- relevel(A_chi$VitCxCarbs, ref = "Low")
summary(A_chi$VitCxCarbs)

addmargins(table(A_chi$VitCxCarbs, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$VitCxCarbs, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitCxCarbs, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitCxCarbs, A_chi$AScase43, exclude = NULL))


A_chi$VitCxFat <- ifelse(A_chi$VitC2 == "High intake" & A_chi$Fats2 == "High intake", "High",
                         ifelse(A_chi$VitC2 == "High intake" & A_chi$Fats2 == "Moderate intake", "HighVclowF",
                                ifelse(A_chi$VitC2 == "High intake" & A_chi$Fats2 == "Low intake", "HighVclowF",
                                       ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Fats2 == "High intake", "ModVchighF",
                                              ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Fats2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Fats2 == "Low intake", "ModVclowF",
                                                            ifelse(A_chi$VitC2 == "Low intake" & A_chi$Fats2 == "High intake", "LowVchighF",
                                                                   ifelse(A_chi$VitC2 == "Low intake" & A_chi$Fats2 == "Moderate intake", "LowVchighF",
                                                                          ifelse(A_chi$VitC2 == "Low intake" & A_chi$Fats2 == "Low intake", "Low", NA)))))))))
A_chi$VitCxFat <- as.factor(A_chi$VitCxFat)
A_chi$VitCxFat <- relevel(A_chi$VitCxFat, ref = "Low")
summary(A_chi$VitCxFat)

addmargins(table(A_chi$VitCxFat, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$VitCxFat, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitCxFat, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitCxFat, A_chi$AScase43, exclude = NULL))


#---------------------------------------------------------------------------------------------------------------------------------------------------
#ran this for atopic control analysis
A_chi$VitCxDietF <- ifelse(A_chi$VitC2 == "High intake" & A_chi$DietaryFibre2 == "High intake", "High",
                          ifelse(A_chi$VitC2 == "High intake" & A_chi$DietaryFibre2 == "Moderate intake", "HighVclowdf",
                                 ifelse(A_chi$VitC2 == "High intake" & A_chi$DietaryFibre2 == "Low intake", "HighVclowdf",
                                        ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$DietaryFibre2 == "High intake", "ModVchighdf",
                                               ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$DietaryFibre2 == "Moderate intake", "Mod",
                                                      ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$DietaryFibre2 == "Low intake", "ModVclowdf",
                                                             ifelse(A_chi$VitC2 == "Low intake" & A_chi$DietaryFibre2 == "High intake", "LowVchighdf",
                                                                    ifelse(A_chi$VitC2 == "Low intake" & A_chi$DietaryFibre2 == "Moderate intake", "LowVchighdf",
                                                                           ifelse(A_chi$VitC2 == "Low intake" & A_chi$DietaryFibre2 == "Low intake", "Low", NA)))))))))
A_chi$VitCxDietF <- as.factor(A_chi$VitCxDietF)
A_chi$VitCxDietF <- relevel(A_chi$VitCxDietF, ref = "Low")
summary(A_chi$VitCxDietF)

addmargins(table(A_chi$VitCxDietF, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$VitCxDietF, A_chi$eia, exclude = NULL))

A_chi$VitCxVitD <- ifelse(A_chi$VitC2 == "High intake" & A_chi$VitD2 == "High intake", "High",
                          ifelse(A_chi$VitC2 == "High intake" & A_chi$VitD2 == "Moderate intake", "HighVclowVd",
                                 ifelse(A_chi$VitC2 == "High intake" & A_chi$VitD2 == "Low intake", "HighVclowVd",
                                        ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$VitD2 == "High intake", "ModVchighVd",
                                               ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$VitD2 == "Moderate intake", "Mod",
                                                      ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$VitD2 == "Low intake", "ModVclowVd",
                                                             ifelse(A_chi$VitC2 == "Low intake" & A_chi$VitD2 == "High intake", "LowVchighVd",
                                                                    ifelse(A_chi$VitC2 == "Low intake" & A_chi$VitD2 == "Moderate intake", "LowVchighVd",
                                                                           ifelse(A_chi$VitC2 == "Low intake" & A_chi$VitD2 == "Low intake", "Low", NA)))))))))
A_chi$VitCxVitD <- as.factor(A_chi$VitCxVitD)
A_chi$VitCxVitD <- relevel(A_chi$VitCxVitD, ref = "Low")
summary(A_chi$VitCxVitD)

addmargins(table(A_chi$VitCxVitD, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$VitCxVitD, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitCxVitD, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitCxVitD, A_chi$AScase43, exclude = NULL))


A_chi$VitCxProtein <- ifelse(A_chi$VitC2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                                    ifelse(A_chi$VitC2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighVitClowP",
                                           ifelse(A_chi$VitC2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighVitClowP",
                                                  ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModVitChighP",
                                                         ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                                ifelse(A_chi$VitC2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModVitClowP",
                                                                       ifelse(A_chi$VitC2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowVitChighP",
                                                                              ifelse(A_chi$VitC2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowVitChighP",
                                                                                     ifelse(A_chi$VitC2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$VitCxProtein <- as.factor(A_chi$VitCxProtein)
A_chi$VitCxProtein <- relevel(A_chi$VitCxProtein, ref = "Low")
summary(A_chi$VitCxProtein)

addmargins(table(A_chi$VitCxProtein, A_chi$everAS, exclude = NULL))
addmargins(table(A_chi$VitCxProtein, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitCxProtein, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitCxProtein, A_chi$AScase43, exclude = NULL))

#----------------------------------- VITAMIN D-------------------------------------------------------------------------------

A_chi$VitDxFat <- ifelse(A_chi$VitD2 == "High intake" & A_chi$Fats2 == "High intake", "High",
                         ifelse(A_chi$VitD2 == "High intake" & A_chi$Fats2 == "Moderate intake", "HighVdlowF",
                                ifelse(A_chi$VitD2 == "High intake" & A_chi$Fats2 == "Low intake", "HighVdlowF",
                                       ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Fats2 == "High intake", "ModVdhighF",
                                              ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Fats2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Fats2 == "Low intake", "ModVdlowF",
                                                            ifelse(A_chi$VitD2 == "Low intake" & A_chi$Fats2 == "High intake", "LowVdhighF",
                                                                   ifelse(A_chi$VitD2 == "Low intake" & A_chi$Fats2 == "Moderate intake", "LowVdhighF",
                                                                          ifelse(A_chi$VitD2 == "Low intake" & A_chi$Fats2 == "Low intake", "Low", NA)))))))))
A_chi$VitDxFat <- as.factor(A_chi$VitDxFat)
A_chi$VitDxFat <- relevel(A_chi$VitDxFat, ref = "Low")
summary(A_chi$VitDxFat)

addmargins(table(A_chi$VitDxFat, A_chi$ever.asthma, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$AScase43, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$com4, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$com6, exclude = NULL))
addmargins(table(A_chi$VitDxFat, A_chi$com5, exclude = NULL))


A_chi$VitDxCarbs <- ifelse(A_chi$VitD2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                           ifelse(A_chi$VitD2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighVdlowC",
                                  ifelse(A_chi$VitD2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighVdlowC",
                                         ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModVdhighC",
                                                ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                       ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModVdlowC",
                                                              ifelse(A_chi$VitD2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowVdhighC",
                                                                     ifelse(A_chi$VitD2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowVdhighC",
                                                                            ifelse(A_chi$VitD2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$VitDxCarbs <- as.factor(A_chi$VitDxCarbs)
A_chi$VitDxCarbs <- relevel(A_chi$VitDxCarbs, ref = "Low")
summary(A_chi$VitDxCarbs)

addmargins(table(A_chi$VitDxCarbs, A_chi$ever.asthma, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$AScase43, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$com4, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$com6, exclude = NULL))
addmargins(table(A_chi$VitDxCarbs, A_chi$com5, exclude = NULL))

A_chi$VitDxProtein <- ifelse(A_chi$VitD2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                             ifelse(A_chi$VitD2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighVitDlowP",
                                    ifelse(A_chi$VitD2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighVitDlowP",
                                           ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModVitDhighP",
                                                  ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                         ifelse(A_chi$VitD2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModVitDlowP",
                                                                ifelse(A_chi$VitD2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowVitDhighP",
                                                                       ifelse(A_chi$VitD2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowVitDhighP",
                                                                              ifelse(A_chi$VitD2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$VitDxProtein <- as.factor(A_chi$VitDxProtein)
A_chi$VitDxProtein <- relevel(A_chi$VitDxProtein, ref = "Low")
summary(A_chi$VitDxProtein)

addmargins(table(A_chi$VitDxProtein, A_chi$ever.asthma, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$current.as, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$AScase32, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$AScase43, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$com4, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$com6, exclude = NULL))
addmargins(table(A_chi$VitDxProtein, A_chi$com5, exclude = NULL))

#------------------------------------------Dietary fibre------------------------------------------------------------- 

A_chi$DietFxFat <- ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Fats2 == "High intake", "High",
                         ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Fats2 == "Moderate intake", "HighdflowF",
                                ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Fats2 == "Low intake", "HighdflowF",
                                       ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Fats2 == "High intake", "ModdfhighF",
                                              ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Fats2 == "Moderate intake", "Mod",
                                                     ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Fats2 == "Low intake", "ModdflowF",
                                                            ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Fats2 == "High intake", "LowdfhighF",
                                                                   ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Fats2 == "Moderate intake", "LowdfhighF",
                                                                          ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Fats2 == "Low intake", "Low", NA)))))))))
A_chi$DietFxFat <- as.factor(A_chi$DietFxFat)
A_chi$DietFxFat <- relevel(A_chi$DietFxFat, ref = "Low")
summary(A_chi$DietFxFat)

addmargins(table(A_chi$DietFxFat, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$DietFxFat, A_chi$eia, exclude = NULL))

A_chi$DietFxCarbs <- ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Carbohydrates2 == "High intake", "High",
                          ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Carbohydrates2 == "Moderate intake", "HighdflowC",
                                 ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Carbohydrates2 == "Low intake", "HighdflowC",
                                        ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Carbohydrates2 == "High intake", "ModdfhighC",
                                               ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Carbohydrates2 == "Moderate intake", "Mod",
                                                      ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Carbohydrates2 == "Low intake", "ModdflowC",
                                                             ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Carbohydrates2 == "High intake", "LowdfhighC",
                                                                    ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Carbohydrates2 == "Moderate intake", "LowdfhighC",
                                                                           ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Carbohydrates2 == "Low intake", "Low", NA)))))))))
A_chi$DietFxCarbs <- as.factor(A_chi$DietFxCarbs)
A_chi$DietFxCarbs <- relevel(A_chi$DietFxCarbs, ref = "Low")
summary(A_chi$DietFxCarbs)

addmargins(table(A_chi$DietFxCarbs, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$DietFxCarbs, A_chi$eia, exclude = NULL))

A_chi$DietFxProtein <- ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Proteins2 == "High intake", "High",
                            ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Proteins2 == "Moderate intake", "HighdflowP",
                                   ifelse(A_chi$DietaryFibre2 == "High intake" & A_chi$Proteins2 == "Low intake", "HighdflowP",
                                          ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Proteins2 == "High intake", "ModdfhighP",
                                                 ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Proteins2 == "Moderate intake", "Mod",
                                                        ifelse(A_chi$DietaryFibre2 == "Moderate intake" & A_chi$Proteins2 == "Low intake", "ModdflowP",
                                                               ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Proteins2 == "High intake", "LowdfhighP",
                                                                      ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Proteins2 == "Moderate intake", "LowdfhighP",
                                                                             ifelse(A_chi$DietaryFibre2 == "Low intake" & A_chi$Proteins2 == "Low intake", "Low", NA)))))))))
A_chi$DietFxProtein <- as.factor(A_chi$DietFxProtein)
A_chi$DietFxProtein <- relevel(A_chi$DietFxProtein, ref = "Low")
summary(A_chi$DietFxProtein)

addmargins(table(A_chi$DietFxProtein, A_chi$EVAS, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$CUAS, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$AEVAS, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$ACUAS, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$cva, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$wva, exclude = NULL))
addmargins(table(A_chi$DietFxProtein, A_chi$eia, exclude = NULL))

