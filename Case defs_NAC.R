#=========================NON-ATOPIC NON-ASTHMA CONTROL ANALYSIS=======================================================

# ========================AS cases vs non-atopic non-asthma control definitions==================================
# USED IN ROUND 1 ANALYSIS

A_chi$Atopy_status <- ifelse(A_chi$Atopy== "Yes", "Yes",
                             ifelse(A_chi$Atopy=="SPT not done", "SPT Not done","No"))
A_chi$Atopy_status <- as.factor(A_chi$Atopy_status)
summary(A_chi$Atopy_status)

#=============================================only ever asthma
A_chi$everAS <- ifelse(A_chi$`Ever asthma`== "Yes" , "ever-asthma",
                            ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$everAS <- factor(A_chi$everAS ,levels= c("control","ever-asthma"))
summary(A_chi$everAS)

#=============================================only current asthma

A_chi$current.as <- ifelse(A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Potential asthma based on section 2`== "Yes" |
                             A_chi$`Question 06 recorrected`== "Yes" & A_chi$`Question 02 recorrected`== "Yes"| 
                             A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 10`=="Yes" , "current-asthma",
                           ifelse(A_chi$`Question 06 recorrected`=="No" & A_chi$Atopy_status == "No","control", NA))
A_chi$current.as <- factor(A_chi$current.as ,levels= c("control","current-asthma"))
summary(A_chi$current.as)

#=============================================Atopic ever asthma
A_chi$AScase32 <- ifelse(A_chi$`Ever asthma`== "Yes" & A_chi$Atopy_status == "Yes", "Atopic_everAS",
                         ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$AScase32 <- factor(A_chi$AScase32 ,levels= c("control","Atopic_everAS"))
summary(A_chi$AScase32)

#============================================Atopic current asthma
A_chi$AScase43 <- ifelse(A_chi$current.as =="current-asthma" & A_chi$Atopy_status == "Yes", "Atopic_currentAS",
                         ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$AScase43 <- factor(A_chi$AScase43 ,levels= c("control","Atopic_currentAS"))
summary(A_chi$AScase43)

#=======================================VARIANTS===========================================================
#---------------CVA vs control---------------
A_chi$com4 <- ifelse( A_chi$`Question 06 recorrected` == "Yes"& A_chi$`Question 10` == "Yes", "CVA-case",
                      ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$com4 <- factor(A_chi$com4 ,levels= c("control","CVA-case"))
summary(A_chi$com4)

A_chi$com4.case <- ifelse( A_chi$`Question 06 recorrected` == "Yes"& A_chi$`Question 10` == "Yes", "CVA-case", NA)
A_chi$com4.case <- as.factor(A_chi$com4.case )
summary(A_chi$com4.case)

A_chi$com4.control <- ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA)
A_chi$com4.control <- as.factor(A_chi$com4.control )
summary(A_chi$com4.control)

#--------------EIA vs control-----------------
A_chi$com5 <- ifelse(A_chi$`Question 06 recorrected` == "Yes"  & A_chi$`Question 09` == "Yes", "EIA-case",
                     ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$com5 <- factor(A_chi$com5 ,levels= c("control","EIA-case"))
summary(A_chi$com5)

A_chi$com5.case <- ifelse(A_chi$`Question 06 recorrected` == "Yes"  & A_chi$`Question 09` == "Yes", "EIA-case", NA)
A_chi$com5.case <- as.factor(A_chi$com5.case )
summary(A_chi$com5.case)

A_chi$com5.control <- ifelse(A_chi$`Ever asthma`== "No"  & A_chi$Atopy_status == "No","control", NA)
A_chi$com5.control <- as.factor(A_chi$com5.control )
summary(A_chi$com5.control)

#--------------WVA vs control----------------
A_chi$com6 <- ifelse(A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 02 recorrected` == "Yes" |
                       A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 03 corrected using question 04 and 05` %in% c('1 to 3','4 to 12','> 12')|
                       A_chi$`Question 06 recorrected` == "Yes" &! A_chi$`Question 04 dichotomous`=="Never" |
                       A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 05` == "Yes", "WVA-case",
                     ifelse(A_chi$`Question 06 recorrected` == "No"  & A_chi$Atopy_status == "No","control", NA))
A_chi$com6 <- factor(A_chi$com6 ,levels= c("control","WVA-case"))
summary(A_chi$com6)

A_chi$com6.case <- ifelse(A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 02 recorrected` == "Yes" |
                            A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 03 corrected using question 04 and 05` %in% c('1 to 3','4 to 12','> 12')|
                            A_chi$`Question 06 recorrected` == "Yes" &! A_chi$`Question 04 dichotomous`=="Never" |
                            A_chi$`Question 06 recorrected` == "Yes" & A_chi$`Question 05` == "Yes", "WVA-case", NA)
A_chi$com6.case <- as.factor(A_chi$com6.case )
summary(A_chi$com6.case)

A_chi$com6.control <- ifelse(A_chi$`Question 06 recorrected` == "No"  & A_chi$Atopy_status == "No","control", NA)
A_chi$com6.control <- as.factor(A_chi$com6.control )
summary(A_chi$com6.control)
