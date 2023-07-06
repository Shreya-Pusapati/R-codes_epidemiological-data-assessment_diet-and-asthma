#*************************Code used for the analysis****************************

#====================Calling all the required packages==========================  
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(dplyr)

# For Session info
sessionInfo()

#========================Importing the dataset==================================
A <- read.xlsx("Anon survey batch AZ-X Feb2022.xlsx") #IMPORT DATASET ALREADY PRESENT IN THIS DIRECTORY
names(A)<-make.unique(names(A))
names(A)[grep('SPT\\.\\(.3', names(A))]<-'Atopy'

write.csv(A_chi,"C:/Users/P SHREYA/Desktop/capstonep_R/SG_chi_Diet/demo_table.csv", row.names = TRUE) # FOR SAVING THE CHANGES MADE INTO A CSV FILE

#========================Cleaning the columns===================================
# For Race
A[, 'Race'] <- with(
  A,
  ifelse(Race == 1, 'Chinese',
         ifelse(Race ==  2,  'Malay',
                ifelse(Race %in% c(3, 'SIKH'), 'Indian', 
                       ifelse(
                         Race %in% c(4, 'EURASIAN', 'FILIPINO', 'INDONESIAN'), 
                         'Other',
                         NA
                       ))))
)

# For SPT 
A[, 'Atopy cleaned'] <- with(
  A,
  ifelse(Atopy == 'Yes', 'Case',
         ifelse(Atopy == 'No', 'Control', NA))
)

#===================================Section 1===================================
# Renaming columns of section1 from Sections to Questions 
A[, c(
  'Question 01',
  'Question 02', 
  'Question 06',
  'Question 09', 
  'Question 10'
)]<-lapply(
  A[, c(
    'Section11',
    'Section12',
    'Section16',
    'Section19',
    'Section110'
  )],
  function(x) dplyr::recode(
    factor(ifelse(x %in% c(1,2), x, NA), c(2,1)),
    '1' = 'Yes',
    '2' = 'No'
  )
)

A[, 'Question 03'] <- dplyr::recode(
  factor(
    ifelse(A[, 'Section13'] %in% c(1,2,3), A[, 'Section13'], NA),
    c(3,1,2)
  ),
  '1' = '4 to 12',
  '2' = '> 12',
  '3' = '1 to 3'
)

A[, 'Question 04'] <- dplyr::recode(
  factor(
    ifelse(A[, 'Section14'] %in% c(1,2,3,4,5), A[, 'Section14'], NA),
    c(1,2,3,4,5)
  ),
  '1' = 'Never',
  '2' = '< 1 night',
  '3' = 'Once',
  '4' = 'Twice',
  '5' = 'Thrice\ more than 3 nights'
)

A[, 'Question 05'] <- dplyr::recode(
  factor(
    ifelse(A[, 'Section15'] %in% c(1,2), A[, 'Section15'], NA),
    c(2,1)
  ),
  '1' = 'Yes',
  '2' = 'No'
)

A[, c('Question 07', 'Question 08')] <- lapply(
  A[, c("Section17", "Section18")],
  function(x) as.numeric(ifelse(!x %in% c('99', '999', '9999'), x, NA))
)

#================================Section 2======================================
s21to29 <- grep('^Section.*2\\d$', names(A), value=T)

s21to29_clean <- paste0(s21to29, '_clean')

A[, s21to29_clean] <- lapply(A[, s21to29],
                               function(x) ifelse(x %in% c(1:6), x, NA))

#=============================CORRECTIONS=======================================
# response corrections section 1 questions only =====

A[, 'Question 03 corrected using question 05'] <- with (
  A,
  factor(
    if_else(is.na(`Question 03`) & `Question 05` == 'Yes', '1 to 3', as.character(`Question 03`)),
    c('1 to 3', '4 to 12', '> 12')
  )
)

A[, 'Question 03 corrected using question 04 and 05'] <- with (
  A,
  factor(
    if_else(is.na(`Question 03 corrected using question 05`) & 
              `Question 04` != 'Never', '1 to 3', 
            as.character(`Question 03 corrected using question 05`)),
    c('1 to 3', '4 to 12', '> 12')
  )
)

A[, 'Question 03 corrected'] <- A[, 'Question 03 corrected using question 04 and 05'] 

A[, 'Question 02 corrected'] <- with (
  A,
  factor(
    if_else(!is.na(`Question 03 corrected`) & `Question 02` %in% c(NA, 'No'), 
            'Yes', as.character(`Question 02`)),
    c('No', 'Yes')
  )
)

A[, 'Question 01 corrected'] <- with (
  A,
  factor(
    if_else(`Question 01` %in% c(NA, 'No') & `Question 02 corrected` == 'Yes', 
            'Yes', as.character(`Question 01`)),
    c('No', 'Yes')
  )
)

A[, 'Question 07 corrected'] <- floor(A[, 'Question 07'])

A[, 'Question 08 corrected'] <- with (
  A,
  ifelse(`Question 08` == 0, NA,
         ifelse(`Question 08` > 0, floor(`Question 08`), NA))
)

A[, 'Age of asthma remission'] <- A[, 'Question 07 corrected'] + A[, 'Question 08 corrected']

A[, 'Question 06 corrected'] <- A[, 'Question 06']

A[which(A[, 'Age of asthma remission'] > 0 & A[, 'Question 06'] == 'No' | 
            A[, 'Age of asthma remission'] > 0 & is.na(A[, 'Question 06'])), 
    'Question 06 corrected'] <- 'Yes'

A[, 'Age'] <- with (A, ifelse(Age < 999, as.numeric(Age), NA))

A[, 'Duration of asthma remission'] <- A[, 'Age'] - A[, 'Age of asthma remission']

A[, 'Question 02 recorrected'] <- with (
  A,
  factor(
    if_else(`Question 02 corrected`%in% c(NA, 'No') & !`Question 09` %in% c(NA, 'No'), 
            'Yes', as.character(`Question 02 corrected`)),
    c('No', 'Yes')
  )
)

A[, 'Question 01 recorrected'] <- with (
  A,
  factor(
    if_else(`Question 01 corrected` %in% c(NA, 'No') & `Question 02 recorrected` == 'Yes', 
            'Yes', as.character(`Question 01 corrected`)),
    c('No', 'Yes')
  )
)

# ===================response corrections plus section 2 questions=====

A[, 'Potential asthma based on section 2'] <- apply(
  A[, grep('n\\d{2}_clean', names(A), value = T)],
  1,
  function(x) ifelse(any(x > 1, na.rm=T), 'Yes', 'No')
)

A[, 'Question 06 recorrected'] <- with (
  A,
  factor(if_else(`Potential asthma based on section 2` == 'Yes' & 
                   `Question 06 corrected` %in% c(NA, 'No'), 
                 'Yes', 
                 as.character(`Question 06 corrected`)), 
         c('No', 'Yes'))
)

with(A, table(`Question 06 corrected`, `Potential asthma based on section 2`))

A[, 'Question 04 dichotomous'] <- with (
  A,
  factor(
    ifelse(`Question 04` == 'Never', 'Never', 
           ifelse(`Question 04` != 'Never', 
                  'At least once in the past 12 months', 
                  NA)), 
    c('Never', 'At least once in the past 12 months')
  )
)

#==================Filtering out Chinese pop and removing KK batch==============
A_chi <- A %>% filter(Race == 'Chinese', Batch != 'KK') %>%
  mutate(
    Gender = factor(dplyr::recode(as.factor(toupper(trimws(Sex))),
                                  '1' = 'Male',
                                  '2' = 'Female',
                                  'F' = 'Female',
                                  'M' = 'Male'), c('Female', 'Male')),
    `Total monthly family income` = dplyr::recode(factor(Income, c(1,2,3,4)),
                                                  '1' = '< SGD2000',
                                                  '2' = 'SGD2000 to < SGD4000',
                                                  '3' = 'SGD4000 to < SGD6000',
                                                  '4' = '\u2265 SGD6000'),
    `Housing type` = factor(dplyr::recode(as.factor(Housing),
                                          '1' = 'HDB (Public housing)',
                                          '2' = 'Condominium / Private apartments',
                                          '3' = 'Landed property'),
                            c('HDB (Public housing)', 
                              'Condominium / Private apartments',
                              'Landed property')),
    `Born in Singapore` = factor(ifelse(Country.of.Birth %in% c(1, 'SINGAPORE'), 'Yes',
                                        ifelse(Country.of.Birth %in% c(9, 999), NA,
                                               ifelse(!is.na(Country.of.Birth), 'No', NA))), 
                                 c('No', 'Yes')),
    `Years spent in Singapore` = ifelse(!Country.of.Birth %in% c(1, 'SINGAPORE') & 
                                          !`Years.in.S'pore` %in% c(99, 999), 
                                        as.numeric(`Years.in.S'pore`), NA),
    `Number of people in household` = ifelse(Household.No. <= 15, as.numeric(Household.No.), NA),
    `History of drug allergy` = ifelse(Drug.Allergy %in% c(3, 9), NA,
                                       ifelse(Drug.Allergy == 2, 'No',
                                              ifelse(!is.na(Drug.Allergy), 'Yes', NA))),
    PhysicalActivity = dplyr::recode(factor(ifelse(Physical.Activity %in% c(1:3), Physical.Activity, NA), 
                                            c(3,1,2)),
                                     '1' = 'Once or twice per week',
                                     '2' = 'Most or all days per week',
                                     '3' = 'Never or only occasionally'),
    TVComputer = dplyr::recode(factor(ifelse(`TV/Computer` %in% c(1:4), `TV/Computer`, NA), 
                                      c(1,2,3,4)),
                               '1' = 'Less than 1 hour',
                               '2' = '1 to 3 hours',
                               '3' = 'More than 3 hours to 5 hours',
                               '4' = 'More than 5 hours'),
    Alcohol = dplyr::recode(factor(ifelse(Alcohol %in% c(1:3), Alcohol, NA), 
                                   c(3,2,1)),
                            '1' = 'Frequent',
                            '2' = 'Occasional',
                            '3' = 'Non-drinker'),
    `Smoking status` = dplyr::recode(factor(ifelse(Smoking %in% c(1:3), Smoking, NA), 
                                            c(3,2,1)),
                                     '1' = 'Current smoker',
                                     '2' = 'Ex-smoker',
                                     '3' = 'Non-smoker'),
    `Wheeze attacks in the past 12 months` = ifelse(`Question 03 corrected` %in% 
                                                      c('1 to 3', '4 to 12', '> 12'), 
                                                    'Yes', 
                                                    'No'),
    `Awoken by wheezing in the past 12 months` = ifelse(`Question 04` == 'Never', 'No', 'Yes'),
    `Cough-variant asthma` = factor(ifelse( `Question 06 recorrected` == 'Yes' & Section110 == 1, 'Case',
                                            ifelse(`Question 06 recorrected` == 'Yes' & Section110 == 2, 'Control',
                                                   NA)), c('Control', 'Case')),
    `Exercise-induced asthma` = factor(ifelse(`Question 06 recorrected` == 'Yes' & Section19 == 1, 'Case',
                                              ifelse(`Question 06 recorrected` == 'Yes' & Section19 == 2, 'Control',
                                                     NA)), c('Control', 'Case')),
    `Atopy cleaned` = factor(`Atopy cleaned`, c('Control', 'Case')),
    `Ever wheeze` = `Question 01 recorrected`,
    `Current wheeze` = `Question 02 recorrected`,
    `Wheeze attacks` = `Question 03 corrected`,
    `Ever asthma` = `Question 06 recorrected`
  ) %>% 
  mutate(`Asthma symptoms in the past 12 months` = factor(
    ifelse( apply(
      .[, c('Question 02 recorrected',
            'Wheeze attacks in the past 12 months',
            'Awoken by wheezing in the past 12 months',
            'Question 05',
            'Question 09',
            'Question 10',
            'Potential asthma based on section 2')] == 'Yes', 1, any
    ), 'Yes', 'No' ), c('No', 'Yes') )
  ) %>%
  mutate(
    `Current asthma` = factor(
      ifelse(`Question 06 recorrected` == 'Yes' & is.na(`Asthma symptoms in the past 12 months`)==T |  
               `Question 06 recorrected` == 'Yes' & `Asthma symptoms in the past 12 months` == 'No', 'No', 
             ifelse(`Question 06 recorrected` == 'Yes' & 
                      `Asthma symptoms in the past 12 months` == 'Yes', 'Yes', NA)),
      c('No', 'Yes'))
  )


A_chi[, 'Wheeze-variant asthma'] <- with(
  A_chi, 
  ifelse(
    `Question 06 recorrected` == 'Yes' & `Question 02 recorrected` == 'Yes' | 
      `Question 06 recorrected` == 'Yes' & Section13 %in% c(1, 2, 3) | 
      `Question 06 recorrected` == 'Yes' & Section14 %in% c(2, 3, 4, 5) |
      `Question 06 recorrected` == 'Yes' & `Question 05` == 'Yes', 
    'Case', 
    NA
  )
)

A_chi[, 'Wheeze-variant asthma'] <- with(
  A_chi,
  ifelse(`Question 06 recorrected` == 'Yes' & is.na(`Wheeze-variant asthma`),
         'Control', 
         `Wheeze-variant asthma`)
)


A_chi[, 'Wheeze-variant asthma'] <-  factor(A_chi[, 'Wheeze-variant asthma'] , 
                                                c('Control', 'Case'))

# severity=====

A_chi[, c('q3', 'q4', 'q5')] <- lapply(
  A_chi[,c("Question 03 corrected", 'Question 04', 'Question 05')],
  as.numeric
)

for (x in c('q3', 'q4', 'q5')) {
  A_chi[is.na(A_chi[, x])==T, x] <- 0
}

A_chi <- A_chi %>%
  mutate(
    ISAAC1_new = ifelse(q3 == 0 & q4 == 0 & q5 == 0, 'Blank', 
                        ifelse(q3 %in% c(2,3) | q4 %in% c(3,4,5) | q5 == 2, 'Severe',
                               ifelse(q3 == 1 | q4 == 2, 'Moderate',
                                      ifelse(q3 == 0 & q4 %in% c(0, 1) & q5 %in% c(0,1) , 'Mild', NA)))),
    ISAAC2_new = ifelse(q3 == 0 & q4 == 0 & q5 == 0, 'Blank',
                        ifelse(q3 %in% c(2,3) | q4 %in% c(4,5) | q5 == 2, 'Severe',
                               ifelse(q3 == 1 | q4 == 3, 'Moderate',
                                      ifelse(q3 == 0 & q4 %in% c(0, 1, 2) & q5 %in% c(0,1) , 'Mild', NA))))
  )


#====================DEMOGRAPHIC VARIABLES CONTROLLED IN MULTIVARIATE ANALYSIS=====================
A_chi[, 'Parentalhistory'] <- apply(
  A_chi[, grep('r.Asthma', names(A_chi), value=T) ],
  1,
  function(x) ifelse(any(x == 1), 'Yes', ifelse(all(x == 2), 'No', NA))
)
A_chi$Parentalhistory <- as.factor(A_chi$Parentalhistory)
A_chi$Gender <- as.factor(A_chi$Gender)
A_chi$BMI <- as.numeric(A_chi$BMI)
A_chi$Age <- as.numeric(A_chi$Age)

A_chi$BMIcat <- ifelse(A_chi$BMI %in% c(18,19,20,21,22,23), "Healthy",
                       ifelse(A_chi$BMI == 999, "NA",
                             ifelse( A_chi$BMI > 23, "Overweight",
                                     ifelse(A_chi$BMI <18, "Underweight",NA))))
A_chi$BMIcat <- as.factor(A_chi$BMIcat)
summary(A_chi$BMIcat)

