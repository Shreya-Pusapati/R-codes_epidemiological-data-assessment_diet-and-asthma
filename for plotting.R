
#================ TRIAL =========================
library("forestploter")
library(readxl)
diet<-read_excel("dot plot data.xlsx",sheet=1)
CI.lower <- round(diet$`CI_2.5%`,3)
CI.upper <- round(diet$`CI_97.5%`,3)
diet$` `<-paste (rep(" ",48),collapse = " ")
diet$UnadjustedOR <- paste0(diet$OR, " (",
             CI.lower, "-", CI.upper, ") ")

plot<-forest(diet[,c(1,6,7,5)], #include columns you want in the forest plot
             est=diet$OR,
             lower =diet$`CI_2.5%`,
             upper =diet$`CI_97.5%`,
             ci_column=2,
             arrow_lab = c("protective factors","risk factors"),
             ticks_at = c(0, 0.5, 1, 2,3),
             xlim = c(0,5),
             ref_line = 1)
plot
