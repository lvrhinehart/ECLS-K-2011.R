#for ADHD question 


#Outline
## 1.0: install data (1.1) and install packages (1.2) 
## 2.0: Recode and Create variables: 
#2.1: Create new variables to clean 
#2.2: Clean (missing negatives rem#2.3 check for excluded students 
#2.5:  Change to factors
#3.0: Impute 
#4.0: Weights (sampling, strata, PSU)
#5.0: Glm 
#6.0: LPA




#1.1: get data

library(haven)
install.packages("missForest")

ADHD_data <- read_dta("C:/Users/LRhinehart/Desktop/ADHD.dta")
View(ADHD_data)

#1.2 packages 
#run these packages before starting
library(tidyverse)
library(mice)
library(dplyr)
library(semTools)
library(knitr)
library(ggplot2)
library(survey)
library(semTools)
library(car)
library(naniar)
library(mitools)
library(lme4)
library(RDS)
library(corrplot)
library(tidyLPA)
library(stats)
library(olsrr)
library(missForest)

install.packages("devtools")
devtools::install_github("data-edu/tidyLPA")

#2.0 recode and create variables 
##Groups of students
ADHD_data$X8SPEDCLEAN = ADHD_data$E8RECSPE

ADHD_data$X2ADHDCLEAN = ADHD_data$E2ADHD
ADHD_data$X4ADHDCLEAN = ADHD_data$E4ADHD
ADHD_data$X6ADHDCLEAN = ADHD_data$E6ADHD
ADHD_data$X7ADHDCLEAN = ADHD_data$E7ADHD
ADHD_data$X8ADHDCLEAN = ADHD_data$E8ADHD

##predictors
ADHD_data$X1ATTNFSCLEAN = ADHD_data$X1ATTNFS
ADHD_data$X1INBCNTCLEAN = ADHD_data$X1INBCNT
ADHD_data$X1TCHAPPCLEAN = ADHD_data$X1TCHAPP
ADHD_data$X1TCHCONCLEAN = ADHD_data$X1TCHCON
ADHD_data$X1TCHPERCLEAN = ADHD_data$X1TCHPER
ADHD_data$X1TCHEXTCLEAN = ADHD_data$X1TCHEXT
ADHD_data$X1TCHINTCLEAN = ADHD_data$X1TCHINT
ADHD_data$X1PRNIMPCLEAN = ADHD_data$X1PRNIMP
ADHD_data$X2CLSNSSCLEAN = ADHD_data$X2CLSNSS
ADHD_data$X2CNFLCTCLEAN = ADHD_data$X2CNFLCT

ADHD_data$X8TCHAPPCLEAN = ADHD_data$X8TCHAPP
ADHD_data$X8TCHCONCLEAN = ADHD_data$X8TCHCON
ADHD_data$X8TCHPERCLEAN = ADHD_data$X8TCHPER
ADHD_data$X8TCHEXTCLEAN = ADHD_data$X8TCHEXT
ADHD_data$X8TCHINTCLEAN = ADHD_data$X8TCHINT
ADHD_data$X8ATTNCLEAN = ADHD_data$X8ATTMCQ
ADHD_data$X8INHIBCLEAN = ADHD_data$X8INTMCQ

ADHD_data$X1READING_CLEAN = ADHD_data$X1RSCALK4
ADHD_data$X1MATH_CLEAN = ADHD_data$X1MSCALK4
ADHD_data$X1DCCSTOTCLEAN = ADHD_data$X1DCCSTOT
ADHD_data$X1NRWABLCLEAN = ADHD_data$X1NRWABL
ADHD_data$X8READING_CLEAN = ADHD_data$X8RSCALK4
ADHD_data$X8MATH_CLEAN = ADHD_data$X8MSCALK4
ADHD_data$X8NRWABLCLEAN = ADHD_data$X8NRWABL


##Covariates, child
ADHD_data$CHSEXCLEAN = ADHD_data$X_CHSEX_R

#recode sex, from ECLS 1=male, 2=female
#change to 1=male and 0=female 

table(ADHD_data$CHSEXCLEAN)
# 1=9288, 2=8847
ADHD_data$CHSEXCLEAN[ADHD_data$CHSEXCLEAN==2] <- 0
table(ADHD_data$CHSEXCLEAN)

ADHD_data$HISPCLEAN = ADHD_data$X_HISP_R
ADHD_data$HISPCLEAN[ADHD_data$HISPCLEAN==2] <- 0
table(ADHD_data$HISPCLEAN)

ADHD_data$AMINANCLEAN = ADHD_data$X_AMINAN_R
ADHD_data$ASIANBCLEAN = ADHD_data$X_ASIAN_R
ADHD_data$ASIANBCLEAN[ADHD_data$ASIANBCLEAN==2] <- 0
table(ADHD_data$ASIANBCLEAN)

ADHD_data$HAWPICLEAN = ADHD_data$X_HAWPI_R

ADHD_data$BLACKCLEAN = ADHD_data$X_BLACK_R
ADHD_data$BLACKCLEAN[ADHD_data$BLACKCLEAN==2] <- 0
table(ADHD_data$BLACKCLEAN)

ADHD_data$WHITECLEAN = ADHD_data$X_WHITE_R
ADHD_data$WHITECLEAN[ADHD_data$WHITECLEAN==2] <- 0
table(ADHD_data$WHITECLEAN)

ADHD_data$MULTRCLEAN = ADHD_data$X_MULTR_R
ADHD_data$X12SESLCLEAN = ADHD_data$X12SESL
ADHD_data$X12LANGSTCLEAN = ADHD_data$X12LANGST
ADHD_data$P1NBIFSPCLEAN = ADHD_data$P1NBIFSP
ADHD_data$P1NBIFSPCLEAN[ADHD_data$P1NBIFSPCLEAN==2] <- 0
table(ADHD_data$P1NBIFSPCLEAN)

ADHD_data$KAGECLEAN = ADHD_data$X1KAGE_R
ADHD_data$X1AGEENTCLEAN = ADHD_data$X1AGEENT

##Covariates location
ADHD_data$X2PUBPRICLEAN = ADHD_data$X2PUBPRI
ADHD_data$X2KRCETHCLEAN = ADHD_data$X2KRCETH
ADHD_data$X2FREE_OR_RED_LUNCHCLEAN = ADHD_data$S2LUNCH
ADHD_data$S2RDGPCTCLEAN = ADHD_data$S2RDGPCT
ADHD_data$S2MTHPCTCLEAN = ADHD_data$S2MTHPCT
ADHD_data$DISTPOVCLEAN = ADHD_data$X_DISTPOV
ADHD_data$X2REGIONCLEAN = ADHD_data$X2REGION
ADHD_data$X2LOCALECLEAN = ADHD_data$X2LOCALE


##IEP identifications 
ADHD_data$X8SLI = ADHD_data$E8SPCHLN
ADHD_data$X8LD = ADHD_data$E8LRNDIS
ADHD_data$X8EBD = ADHD_data$E8EMTPRB
ADHD_data$X8ID= ADHD_data$E8MNTRTR
ADHD_data$X8DD = ADHD_data$E8DEVDLY
ADHD_data$X8OHI = ADHD_data$E8HLTHIM
ADHD_data$X8AUT = ADHD_data$E8AUTISM
ADHD_data$X8MULT_DIS = ADHD_data$E8MLTIM
ADHD_data$X8NO_CLASSIFICATION = ADHD_data$E8NOCLAS

#######################################################################################################
#CLEAN DATA 2.2 
##STEP 1: RECODE MISSING DATA WITH NEGATIVES TO NA 
#-1 is "not applicable, including legitimate skips" (DONT take out -1 for SES variable (codebook page ?)
#-2 is data suppresssed, im public file only
#-4 is data suppressed due to administration error
#-5 is "item not asked in school admin questionnaire"
#-7 is "refused"
#-8 is "don't know"
#-9 is "not ascertained, a type of nonresponse"
# (blank) system missing (unit nonresponse)


#remove negs in sped categories 

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8SLI = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8SLI = -1))
table(ADHD_data$X8SLI)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8LD = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8LD = -1))
table(ADHD_data$X8LD)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8EBD = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8EBD = -1))
table(ADHD_data$X8EBD)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ID = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ID = -1))
table(ADHD_data$X8ID)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8DD = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8DD = -1))
table(ADHD_data$X8DD)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8OHI = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8OHI = -1))
table(ADHD_data$X8OHI)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8AUT = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8AUT = -1))
table(ADHD_data$X8AUT)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8MULT_DIS = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8MULT_DIS = -1))
table(ADHD_data$X8MULT_DIS)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8NO_CLASSIFICATION = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8NO_CLASSIFICATION = -1))
table(ADHD_data$X8NO_CLASSIFICATION)


#remove negs in outcomes 
#LD All
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8LDCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8LDCLEAN = -1))
summary(ADHD_data$X8LDCLEAN)
table(ADHD_data$X8LDCLEAN)

#ADHD

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ADHDCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ADHDCLEAN = -1))
summary(ADHD_data$X8ADHDCLEAN)
table(ADHD_data$X8ADHDCLEAN)

#change outcomes for adding to the imputation model 
#any sped, 4th 
#check for frequencies 
ADHD_data %>% count(X8SPEDCLEAN)
ADHD_data %>% count(E8RECSPE)
# -9: 5, 1: 1105, 2: 115, NA: 16949
#change 2s and NA to zero 
ADHD_data$E8RECSPE[is.na(ADHD_data$E8RECSPE)] <- 0
ADHD_data$E8RECSPE[ADHD_data$E8RECSPE==2] <- 0
#change -9 to NA 
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(E8RECSPE = -9))
#check to make sure 2s and NAs are zero 
ADHD_data %>% count(E8RECSPE)
#change to factor 
ADHD_data$E8RECSPE.F <- factor(ADHD_data$E8RECSPE)
class(ADHD_data$E8RECSPE.F)
ADHD_data %>% count(E8RECSPE.F)

#any ADHD, 4th  
#change name
ADHD_data$X8ADHD.for.impute = ADHD_data$E8ADHD
#check for frequencies 
ADHD_data %>% count(X8ADHD.for.impute)
# -9: 31, -1: 115, 1: 217, 2: 862, NA: 16949
#change 2s and NA to zero 
ADHD_data$X8ADHD.for.impute[is.na(ADHD_data$X8ADHD.for.impute)] <- 0
ADHD_data$X8ADHD.for.impute[ADHD_data$X8ADHD.for.impute==2] <- 0
#change -9 and -1 to NA
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ADHD.for.impute = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ADHD.for.impute = -1))
#check work
ADHD_data %>% count(X8ADHD.for.impute)
#change to factor 
ADHD_data$X8ADHD.for.impute.F <- factor(ADHD_data$X8ADHD.for.impute)
class(ADHD_data$X8ADHD.for.impute.F)
ADHD_data %>% count(X8ADHD.for.impute.F)
#146 are NAs 

##Predictors (BELOW, IN K, ONLY HAD -9s)
#reverse code: conflict with teacher, externalizing problems, internalizing problems, overactive 
library(naniar)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1ATTNFSCLEAN = -9))
summary(ADHD_data$X1ATTNFSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1INBCNTCLEAN = -9))
summary(ADHD_data$X1INBCNTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1TCHAPPCLEAN = -9))
summary(ADHD_data$X1TCHAPPCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1TCHCONCLEAN = -9))
summary(ADHD_data$X1TCHCONCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1TCHPERCLEAN = -9))
summary(ADHD_data$X1TCHPERCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1TCHEXTCLEAN = -9))
summary(ADHD_data$X1TCHEXTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1TCHINTCLEAN = -9))
summary(ADHD_data$X1TCHINTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1PRNIMPCLEAN = -9))
summary(ADHD_data$X1PRNIMPCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2CLSNSSCLEAN = -9))
summary(ADHD_data$X2CLSNSSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2CNFLCTCLEAN = -9))
summary(ADHD_data$X2CNFLCTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1EBRSTOTCLEAN = -9))
summary(ADHD_data$X1EBRSTOTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1READING_CLEAN = -9))
summary(ADHD_data$X1READING_CLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1MATH_CLEAN = -9))
summary(ADHD_data$X1MATH_CLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1DCCSTOTCLEAN = -9))
summary(ADHD_data$X1DCCSTOTCLEAN)
table(ADHD_data$X1DCCSTOTCLEAN)

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8ATTNCLEAN = -9))
summary(ADHD_data$X8ATTNCLEAN)
table(ADHD_data$X8ATTNCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8INHIBCLEAN = -9))
summary(ADHD_data$X8INHIBCLEAN)
table(ADHD_data$X8INHIBCLEAN)

ADHD_data$X8ATTNCLEAN = ADHD_data$X8ATTMCQ
ADHD_data$X8INHIBCLEAN = ADHD_data$X8INTMCQ
#24 are 0
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1NRWABLCLEAN = -9))
summary(ADHD_data$X1NRWABLCLEAN)
table(ADHD_data$X1NRWABLCLEAN)
#255 are 393 (lowest)
#OTHER READING AND MATH SCORES
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8READING_CLEAN = -9))
summary(ADHD_data$X8READING_CLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X8MATH_CLEAN = -9))
summary(ADHD_data$X8MATH_CLEAN)


#NAs in covariates
##GENDER AND RACE/ETHNICITY (ONLY RECODE FOR -9)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(CHSEXCLEAN = -9))
summary(ADHD_data$CHSEXCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(HISPCLEAN = -9))
summary(ADHD_data$HISPCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(AMINANCLEAN = -9))
summary(ADHD_data$AMINANCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(ASIANBCLEAN = -9))
summary(ADHD_data$ASIANBCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(HAWPICLEAN = -9))
summary(ADHD_data$HAWPICLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(BLACKCLEAN = -9))
summary(ADHD_data$BLACKCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(WHITECLEAN = -9))
summary(ADHD_data$WHITECLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(MULTRCLEAN = -9))
summary(ADHD_data$MULTRCLEAN)
##OTHER COVARIATES (ONLY RECODE FOR -9, EXCEPT FOR IFSP, ALSO RECODE FOR -7 AND -8)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X12SESLCLEAN = -9))
summary(ADHD_data$X12SESLCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X12LANGSTCLEAN = -9))
summary(ADHD_data$X12LANGSTCLEAN)
table(ADHD_data$X12LANGSTCLEAN)
#change 3 (other) to 1 (non English)
ADHD_data$X12LANGSTCLEAN[ADHD_data$X12LANGSTCLEAN==3] <- 0
ADHD_data$X12LANGSTCLEAN[ADHD_data$X12LANGSTCLEAN==2] <- 0
table(ADHD_data$X12LANGSTCLEAN)

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X1AGEENTCLEAN = -9))
summary(ADHD_data$X1AGEENTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(KAGECLEAN = -9))
summary(ADHD_data$KAGECLEAN)

library(naniar)
#Covariates: schoool level and location
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2PUBPRICLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2PUBPRICLEAN = -1))
summary(ADHD_data$X2PUBPRICLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2KRCETHCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2KRCETHCLEAN = -1))
summary(ADHD_data$X2KRCETHCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= -1))
summary(ADHD_data$X2FREE_OR_RED_LUNCHCLEAN)
#there are x values over 100 
count(filter(ADHD_data,X2FREE_OR_RED_LUNCHCLEAN>100))
ggplot(ADHD_data, aes(x = "FRL", y = X2FREE_OR_RED_LUNCHCLEAN)) + geom_boxplot()
#there is one outlier here, 350, so I will remove it 
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= 350))
summary(ADHD_data$X2FREE_OR_RED_LUNCHCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S2RDGPCTCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S2RDGPCTCLEAN = -1))
summary(ADHD_data$S2RDGPCTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S2MTHPCTCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S2MTHPCTCLEAN = -1))
summary(ADHD_data$S2MTHPCTCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S4ELIQDSCLEAN = -9))
summary(ADHD_data$S4ELIQDSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S6ELIQDSCLEAN = -9))
summary(ADHD_data$S6ELIQDSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S7ELIQDSCLEAN = -9))
summary(ADHD_data$S7ELIQDSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S4ELRTLSCLEAN = -9))
summary(ADHD_data$S4ELRTLSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S6ELRTLSCLEAN = -9))
summary(ADHD_data$S6ELRTLSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S7ELRTLSCLEAN = -9))
summary(ADHD_data$S7ELRTLSCLEAN)

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S8ELRTLSCLEAN = -9))
summary(ADHD_data$S8ELRTLSCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(S8ELIQDSCLEAN = -9))
summary(ADHD_data$S8ELIQDSCLEAN)

ADHD_data <- ADHD_data %>% replace_with_na(replace = list(DISTPOVCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(DISTPOVCLEAN = -1))
summary(ADHD_data$DISTPOVCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2REGIONCLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2REGIONCLEAN = -1))
summary(ADHD_data$X2REGIONCLEAN)
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2LOCALECLEAN = -9))
ADHD_data <- ADHD_data %>% replace_with_na(replace = list(X2LOCALECLEAN = -1))
summary(ADHD_data$X2LOCALECLEAN)
table(ADHD_data$X2LOCALECLEAN)
#change locale; combine urban: 12, 13 (other) to 11 
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==12] <- 11
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==13] <- 11
#change locale; combine urban: 22, 23 (other) to 21 
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==22] <- 21
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==23] <- 21
#change locale; combine urban: 32, 33 (other) to 31 
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==32] <- 31
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==33] <- 31
#change locale; combine urban: 42, 43 (other) to 41 
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==42] <- 41
ADHD_data$X2LOCALECLEAN[ADHD_data$X2LOCALECLEAN==43] <- 41
table(ADHD_data$X2LOCALECLEAN)



#change to as.factor and rename 
ADHD_data$CHSEXCLEAN.F <- factor(ADHD_data$CHSEXCLEAN)
class(ADHD_data$CHSEXCLEAN.F)
ADHD_data$HISPCLEAN.F <- factor(ADHD_data$HISPCLEAN)
class(ADHD_data$HISPCLEAN.F)
table(ADHD_data$HISPCLEAN.F)
ADHD_data$AMINANCLEAN.F <- factor(ADHD_data$AMINANCLEAN)
class(ADHD_data$AMINANCLEAN.F)
ADHD_data$ASIANBCLEAN.F <- factor(ADHD_data$ASIANBCLEAN)
class(ADHD_data$ASIANBCLEAN.F)
ADHD_data$HAWPICLEAN.F <- factor(ADHD_data$HAWPICLEAN)
class(ADHD_data$HAWPICLEAN.F)
ADHD_data$BLACKCLEAN.F <- factor(ADHD_data$BLACKCLEAN)
class(ADHD_data$BLACKCLEAN.F)
ADHD_data$WHITECLEAN.F <- factor(ADHD_data$WHITECLEAN)
class(ADHD_data$WHITECLEAN.F)
ADHD_data$MULTRCLEAN.F <- factor(ADHD_data$MULTRCLEAN)
class(ADHD_data$MULTRCLEAN.F)
ADHD_data$P1NBIFSPCLEAN.F <- factor(ADHD_data$P1NBIFSPCLEAN)
class(ADHD_data$P1NBIFSPCLEAN.F)

ADHD_data$X2REGIONCLEAN.F <- factor(ADHD_data$X2REGIONCLEAN)
class(ADHD_data$X2REGIONCLEAN.F)
ADHD_data$X2PUBPRICLEAN.F <- factor(ADHD_data$ X2PUBPRICLEAN)
class(ADHD_data$X2PUBPRICLEAN.F)
ADHD_data$S8ELIQDSCLEAN.F <- factor(ADHD_data$S8ELIQDSCLEAN)
class(ADHD_data$S8ELIQDSCLEAN.F)
ADHD_data$S8ELRTLSCLEAN.F <- factor(ADHD_data$S8ELRTLSCLEAN)
class(ADHD_data$S8ELRTLSCLEAN.F)
ADHD_data$X12LANGSTCLEAN.F <- factor(ADHD_data$X12LANGSTCLEAN)
class(ADHD_data$X12LANGSTCLEAN.F)
ADHD_data$X2LOCALECLEAN.F <- factor(ADHD_data$X2LOCALECLEAN)
class(ADHD_data$X2LOCALECLEAN.F)

table(ADHD_data$X12LANGSTCLEAN.F)
table(ADHD_data$X2LOCALECLEAN.F)
table(ADHD_data$HISPCLEAN.F)
##########################################################################################################################
#impute
############################################################################################################################################
#3.0 impute 
#impute for dissertation 
#choose variables
#order for least to most missing 


imputed.ADHD <- ADHD_data %>% select(CHILDID, CHSEXCLEAN.F, X8ADHD.for.impute.F,X2REGIONCLEAN.F,  
                                X2PUBPRICLEAN.F, X2KRCETHCLEAN, X2LOCALECLEAN.F, E8RECSPE.F,
                                HISPCLEAN.F, ASIANBCLEAN.F, BLACKCLEAN.F, 
                                WHITECLEAN.F, X12LANGSTCLEAN.F, X12SESLCLEAN, X2CLSNSSCLEAN, X2CNFLCTCLEAN, X1AGEENTCLEAN, 
                                KAGECLEAN, X1READING_CLEAN, X1DCCSTOTCLEAN, X1NRWABLCLEAN, X1MATH_CLEAN,
                                X1TCHAPPCLEAN, X1ATTNFSCLEAN, X1INBCNTCLEAN, X1TCHEXTCLEAN, X2FREE_OR_RED_LUNCHCLEAN, 
                                X1TCHINTCLEAN, X1TCHPERCLEAN, X1TCHCONCLEAN, P1NBIFSPCLEAN, X8MATH_CLEAN, X8READING_CLEAN, 
                                S2RDGPCTCLEAN, S2MTHPCTCLEAN)

#check missing 

sort(sapply(imputed.ADHD, function(x) { sum(is.na(x))}), decreasing = TRUE)

#impute with mice

MI.ADHD <- mice(imputed.ADHD, m=5, maxit = 5, printFlag = TRUE, seed = 1210)


#after imputation 

summary(MI.ADHD)

MI.ADHD$imp$X1TCHEXTCLEAN
#shows means of all variables in each imputed dataset...shows means of cat variables, but these are not valid 
MI.ADHD$chainMean
MI.ADHD$imp$CHSEXCLEAN.F

#turn each imp into a dataframe 
imp.ADHD.1 <- complete(MI.ADHD,1)
imp.ADHD.2 <- complete(MI.ADHD,2)
imp.ADHD.3 <- complete(MI.ADHD,3)
imp.ADHD.4 <- complete(MI.ADHD,4)
imp.ADHD.5 <- complete(MI.ADHD,5)

#missing in imputed datasets
sort(sapply(imp.ADHD.1, function(x) { sum(is.na(x))}), decreasing = TRUE)
# no missing in imp.data.1


mean(imp.ADHD.1$X1ATTNFSCLEAN)
mean(imp.ADHD.2$X1ATTNFSCLEAN)
mean(imp.ADHD.3$X1ATTNFSCLEAN)


#get rid of NAs 
weights.no.na = na.omit(weights)
#create survey weight with 5 imputed datasets 

imp.ADHD.with.weights <- svydesign(id = ~weights.no.na$W8CF8P_2T18PSU, strata = ~weights.no.na$W8CF8P_2T18STR, weights = ~weights.no.na$W8CF8P_2T180, 
                                data= imp.ADHD.1, nest=TRUE)


######################################################################################################################################
#create survey design and apply 
#4.0 

#imp.ADHD <- cbind(impdata.5.list, psu = weights$W8CF8P_2T18PSU, str = weights$W8CF8P_2T18STR, weight.1 = weights$W8CF8P_2T180)

#try survey desgin on single data set 
#summary of weights....all have missing 
summary(weights$W8CF8P_2T18PSU)
# summary(weights$W8CF8P_2T18PSU)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.000   2.000   2.568   3.000  16.000   15459 
summary(weights$W8CF8P_2T18STR)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.00   12.00   19.00   17.71   23.00   28.00   15459 
summary(weights$W8CF8P_2T180)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0       0       0     643    1044    7204   12065 

#try with adding weights to the dataframe 
ADHD.imp.1.with.weights <- cbind(imp.ADHD.1, psu = weights$W8CF8P_2T18PSU, str = weights$W8CF8P_2T18STR, weight.1 = weights$W8CF8P_2T180)
#get rid of NAs 
ADHD.imp.1.with.weights = na.omit(ADHD.imp.1.with.weights)
#make svy design with these weights
designs.ADHD <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = ADHD.imp.1.with.weights, nest = TRUE)

#test design.test with svyglm
#test design.test with svyglm
svymean(~X1MATH_CLEAN, designs.ADHD)
mean(ADHD_data$X1MATH_CLEAN, na.rm = TRUE)


designs.ADHD.2 <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = imputationList(1,2,3,4,5), nest = TRUE)

#with MI datasets 

designs.ADHD.mi <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = mi_list, nest = TRUE)


svyby(~ASIANBCLEAN.F, designs.ADHD)

#number in sample 
unwtd.count(ADHD.imp.1.with.weights, designs.ADHD)
#2715, indicate how many are not missing 
svytotal(~X8ADHD.for.impute.F, designs.ADHD)
# total     SE
# X8ADHD.for.impute.F0 3868906 383116.3
# X8ADHD.for.impute.F1   59419   9839.2
table(imp.ADHD.1$X8ADHD.for.impute.F)
#220, rounded to 220 

#try with different weight 
#other weight seemed to have issues with language and Asian 

summary(nw$W12T0)
summary(nw$W12T0PSU)
summary(nw$W12T0STR)


#try with adding weights to the dataframe 
ADHD.imp.1.weights <- cbind(imp.ADHD.1, psu.new = nw$W12T0PSU, str.new = nw$W12T0STR, weight.new = nw$W12T0)
#get rid of NAs 
ADHD.imp.1.weights = na.omit(ADHD.imp.1.weights)
#make svy design with these weights
designs.ADHD.new <- svydesign(id = ~psu.new, strata = ~ str.new, weights = ~ weight.new, data = ADHD.imp.1.weights, nest = TRUE)

######################################################################################################################
#ADHD section

#glm for adhd 

#frequencies for adhd 
#frequency and means for students with LD

ADHD.design.for.descriptives <- subset(designs.ADHD.new, X8ADHD.for.impute.F=="1")

svymean(~X12SESLCLEAN + X2CLSNSSCLEAN + X2CNFLCTCLEAN + X1AGEENTCLEAN +  
          KAGECLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1MATH_CLEAN
        + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN + X2FREE_OR_RED_LUNCHCLEAN + 
          X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN + X2FREE_OR_RED_LUNCHCLEAN +
        + S2RDGPCTCLEAN + S2MTHPCTCLEAN + X2KRCETHCLEAN, design = ADHD.design.for.descriptives)


prop.table(svytable(~CHSEXCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~HISPCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~ASIANBCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~BLACKCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~WHITECLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~X12LANGSTCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~P1NBIFSPCLEAN, design = ADHD.design.for.descriptives))
prop.table(svytable(~X2LOCALECLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~X2REGIONCLEAN.F, design = ADHD.design.for.descriptives))
prop.table(svytable(~X2PUBPRICLEAN.F, design = ADHD.design.for.descriptives))


table(imp.ADHD.1$X8ADHD.for.impute.F, imp.data.1$ASIANBCLEAN.F)
table(imp.ADHD.1$X8ADHD.for.impute.F, imp.data.1$BLACKCLEAN.F)
table(imp.ADHD.1$X8ADHD.for.impute.F, imp.data.1$X12LANGSTCLEAN.F)

#number of students with ADHD 
svytable(~X8ADHD.for.impute.F, design = designs.ADHD.new)

#whole group
svymean(~X12SESLCLEAN + X2CLSNSSCLEAN + X2CNFLCTCLEAN + X1AGEENTCLEAN +  
          KAGECLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1MATH_CLEAN
        + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN + X2FREE_OR_RED_LUNCHCLEAN + 
          X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN + X2FREE_OR_RED_LUNCHCLEAN +
          + S2RDGPCTCLEAN + S2MTHPCTCLEAN + X2KRCETHCLEAN, design = designs.ADHD.new)


#########################################################################################################################################

#with weighting

#model 1
glm.1.adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN,
                     design = designs.ADHD.new, family=binomial(link = "logit"))
summary(glm.1.adhd)
exp(coef(glm.1.adhd))

#reg glm
glm.1.adhd.reg <- glm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN, data =imp.ADHD.1, family=binomial)
summary(glm.1.adhd.reg)
exp(coef(glm.1.adhd.reg))

#with new weight 
glm.1.adhd.new <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN,
                     design = designs.ADHD.new, family=binomial(link = "logit"))
summary(glm.1.adhd.new)
exp(coef(glm.1.adhd.new))

#model 2: reading and math 
glm.2.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN,
                     design = designs.ADHD, family="binomial")
summary(glm.2.adhd)

#with new weight

glm.2.adhd.new <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN,
                     design = designs.ADHD.new, family="binomial")
summary(glm.2.adhd.new)
exp(coef(glm.2.adhd.new))

#model 3: add EF (NR and DCCS)

ADHD_data$w

glm.3.adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN, design = designs.ADHD, family="binomial")
summary(glm.3.adhd)

#with new weight

glm.3.adhd.new <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN, design = designs.ADHD.new, family="binomial")
summary(glm.3.adhd.new)
exp(coef(glm.3.adhd.new))

#model 4: add behavior 

#with fewer behavior
glm.4.2adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                      + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                        P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1ATTNFSCLEAN + 
                        X1TCHEXTCLEAN + X1TCHINTCLEAN + X2CNFLCTCLEAN + X1TCHCONCLEAN, design = designs.ADHD, family="quasibinomial")
summary(glm.4.2adhd)

#with new model
glm.4.adhd.new <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                      + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                        P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN +
                        X1TCHEXTCLEAN + X1TCHINTCLEAN + X2CNFLCTCLEAN + X2CLSNSSCLEAN, design = designs.ADHD.new, family="quasibinomial")
summary(glm.4.adhd.new)
exp(coef(glm.4.adhd.new))
#model 5: add school level 
#glm.5.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
#                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
#                       P1NBIFSPCLEAN + KAGECLEAN +
#                       X1MATH_CLEAN + X1READING_CLEAN 
#                     + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
#                       X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
#                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
#                       X2CLSNSSCLEAN + X2CNFLCTCLEAN +
#                       S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
#                       X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN, design = designs.ADHD, family="quasibinomial")
#summary(glm.5.adhd)

#model 6: add school level 
#glm.6.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
#                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
#                       P1NBIFSPCLEAN + KAGECLEAN +
#                       X1MATH_CLEAN + X1READING_CLEAN 
#                     + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
#                       X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
#                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
#                       X2CLSNSSCLEAN + X2CNFLCTCLEAN +
#                       S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
#                       X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN + X2REGIONCLEAN.F + X2LOCALECLEAN.F, design = designs.ADHD, family="quasibinomial")
#summary(glm.6.adhd)

#tests

regTermTest(glm.1.adhd.new, X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
            + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
              P1NBIFSPCLEAN + KAGECLEAN, method = 'Wald')

#correlation matrix 

cor_matrix_adhd <-imp.ADHD.1 %>% select(X1MATH_CLEAN,X1READING_CLEAN,X1DCCSTOTCLEAN,X1NRWABLCLEAN,X1ATTNFSCLEAN,X1INBCNTCLEAN,
                                          X1TCHEXTCLEAN,X1TCHINTCLEAN,X2CNFLCTCLEAN,X2CLSNSSCLEAN)
c_m_adhd <-cor(cor_matrix_adhd)
round(c_m_adhd,2)

rcorr(c_m_adhd, type = "pearson")
###############################################################################################
#ADHD LPA 

#impute
summary(ADHD_data$X8ATTMCQ)
summary(ADHD_data$X8INTMCQ)

#single imputation as part of tidyLPA does not work...imputed HISP min is -202 and max is 435

ADHD_impute_for_LPA <- ADHD_data %>% select(CHILDID, CHSEXCLEAN.F, X8ADHD.for.impute.F,X2REGIONCLEAN.F,  
                                     X2PUBPRICLEAN.F, X2KRCETHCLEAN, X2LOCALECLEAN.F,
                                     HISPCLEAN.F, ASIANBCLEAN.F, BLACKCLEAN.F, 
                                     WHITECLEAN.F, X12LANGSTCLEAN.F, X12SESLCLEAN, X2CLSNSSCLEAN, X2CNFLCTCLEAN, X1AGEENTCLEAN, 
                                     KAGECLEAN, X1READING_CLEAN, X1DCCSTOTCLEAN, X1NRWABLCLEAN, X1MATH_CLEAN,
                                     X1TCHAPPCLEAN, X1ATTNFSCLEAN, X1INBCNTCLEAN, X1TCHEXTCLEAN, X2FREE_OR_RED_LUNCHCLEAN, 
                                     X1TCHINTCLEAN, X1TCHPERCLEAN, X1TCHCONCLEAN, P1NBIFSPCLEAN, X8MATH_CLEAN, X8READING_CLEAN,
                                     X8ATTNCLEAN, X8INHIBCLEAN,
                                     S2RDGPCTCLEAN, S2MTHPCTCLEAN)

MI_ADHD_for_LPA <- mice(ADHD_impute_for_LPA, m=5, maxit = 5, printFlag = TRUE, seed = 1210)

#after imputation 

summary(MI_ADHD_for_LPA)

#shows means of all variables in each imputed dataset...shows means of cat variables, but these are not valid 
MI_ADHD_for_LPA$chainMean
MI_ADHD_for_LPA$imp$CHSEXCLEAN.F

#turn each imp into a dataframe############################################################################# 
imp.ADHD.1.lpa <- complete(MI_ADHD_for_LPA,1)



subset(imp.ADHD.1.lpa, subset = (X8ADHD.for.impute.F==1))->ADHD_4th_LPA

summary(ADHD_4th_LPA)
#221 :)

ADHD_4th_LPA %>% 
  subset(select = c("X8ATTNCLEAN","X8INHIBCLEAN")) %>% 
  estimate_profiles(n_profiles = 1:6)


adhd.lpa.4 <- ADHD_4th_LPA %>% 
  subset(select = c("X8ATTNCLEAN","X8INHIBCLEAN")) %>% 
  estimate_profiles(4)
get_estimates(adhd.lpa.4)

#A tibble: 16 x 8
#Category  Parameter    Estimate     se         p Class Model Classes
#* <chr>     <chr>           <dbl>  <dbl>     <dbl> <int> <dbl>   <dbl>
#  1 Means     X8ATTNCLEAN     1.18  0.0396 1.49e-194     1     1       4
#2 Means     X8INHIBCLEAN    2.43  0.150  1.96e- 59     1     1       4
#3 Variances X8ATTNCLEAN     0.102 0.0226 5.99e-  6     1     1       4
#4 Variances X8INHIBCLEAN    0.409 0.0418 1.17e- 22     1     1       4
#5 Means     X8ATTNCLEAN     4.58  0.202  5.00e-114     2     1       4
#6 Means     X8INHIBCLEAN    4.44  0.278  3.87e- 57     2     1       4
#7 Variances X8ATTNCLEAN     0.102 0.0226 5.99e-  6     2     1       4
#8 Variances X8INHIBCLEAN    0.409 0.0418 1.17e- 22     2     1       4
#9 Means     X8ATTNCLEAN     2.08  0.0509 0.            3     1       4
#10 Means     X8INHIBCLEAN    2.82  0.0698 0.            3     1       4
#11 Variances X8ATTNCLEAN     0.102 0.0226 5.99e-  6     3     1       4
#12 Variances X8INHIBCLEAN    0.409 0.0418 1.17e- 22     3     1       4
#13 Means     X8ATTNCLEAN     3.13  0.0820 0.            4     1       4
#14 Means     X8INHIBCLEAN    3.30  0.0883 7.61e-305     4     1       4
#15 Variances X8ATTNCLEAN     0.102 0.0226 5.99e-  6     4     1       4
#16 Variances X8INHIBCLEAN    0.409 0.0418 1.17e- 22     4     1       4


#make the chart 
plot_profiles(adhd.lpa.4)


adhd.lpa.4$model_1_class_4

#create a dataframe


adhd.lpa.4.df <- adhd.lpa.4$model_1_class_4$dff

#add to dataframe used for analysis 
adhd.lpa.4.df.merge <- cbind(ADHD_4th_LPA,adhd.lpa.4.df$Class)

#change name of class to profile 

names(adhd.lpa.4.df.merge)[names(adhd.lpa.4.df.merge)== 'adhd.lpa.4.df$Class'] <- 'profile'

#count in each group

table(adhd.lpa.4.df.merge$profile)

#look at demographics and means related to setting within profile 

adhd.lpa.4.df.merge %>% group_by(profile)%>%
  count("Gender" = CHSEXCLEAN.F) %>% 
  mutate(prop = prop.table(n))


adhd.lpa.4.df.merge %>% group_by(profile)%>%
  count(S8ELRTLSCLEAN.F) %>% 
  mutate(prop = prop.table(n))


adhd.lpa.4.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X2KRCETHCLEAN))

adhd.lpa.4.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8READING_CLEAN))

adhd.lpa.4.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))

adhd.lpa.4.df.merge %>%
  dplyr::summarise(Mean = mean(X8READING_CLEAN))

adhd.lpa.4.df.merge %>% 
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))

adhd.lpa.4.df.merge %>% select(X8SPEDCLEAN==1)%>%
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))

####################################################################################################

#ADHD in sped categories 
#217 total 

svytable(~X8ADHD.for.impute.F+X8LD,design = designs.ADHD.new)

wtd.table ()

count(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8LD) 
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8OHI)


#127
table(ADHD_data$X8ADHD.for.impute.F==1,ADHD_data$X8LD)
#94
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8SLI)
#83
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8EBD)
#27
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8ID)
#18
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8AUT)
#16
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8MULT_DIS)
#14

table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8DD)
#8
table(ADHD_data$X8ADHD.for.impute.F,ADHD_data$X8NO_CLASSIFICATION)
#4

table(ADHD_data$X8ADHD.for.impute.F)
#217

table(ADHD_data$X8OHI)
table(ADHD_data$X8LD)
table(ADHD_data$X8SLI)
table(ADHD_data$X8EBD)
table(ADHD_data$X8ID)
table(ADHD_data$X8AUT)
table(ADHD_data$W12)

ADHD_data %>% select(X8ADHD.for.impute.F==1)%>%
  count()

wtd.table(ADHD.imp.1.with.weights$weight.new)

#with weights 

ADHD.imp.weights.frequ <- merge(x=ADHD_data, y=nw, by="CHILDID", all =TRUE)
wtd.table(ADHD.imp.weights.frequ, weights = W12T0, type = C('X8ADHD','X8LD'))

