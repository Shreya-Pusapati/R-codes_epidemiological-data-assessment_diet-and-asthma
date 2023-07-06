# case control analysis for variants
library(readxl)
cca <- read_excel("CCA trial data.xlsx") #categorized cases and controls in new columns in this file.

# CVA
cca$var1 <- ifelse(cca$com4.case %in% c("f","m") , "CVA-case",
                     ifelse(cca$com4.control == "c" ,"control", NA))
cca$var1 <- factor(cca$var1 ,levels= c("control","CVA-case"))
summary(cca$var1)

# WVA
cca$var2 <- ifelse(cca$com6.case %in% c("FWVA","MWVA") , "WVA-case",
                   ifelse(cca$com6.control == "C" ,"control", NA))
cca$var2 <- factor(cca$var2 ,levels= c("control","WVA-case"))
summary(cca$var2)

# EIA
cca$var3 <- ifelse(cca$com5.case %in% c("f","m") , "EIA-case",
                   ifelse(cca$com5.control == "C" ,"control", NA))
cca$var3 <- factor(cca$var3 ,levels= c("control","EIA-case"))
summary(cca$var3)

