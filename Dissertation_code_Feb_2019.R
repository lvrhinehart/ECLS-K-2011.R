#Final code 2.4.19 


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
K423 <- read_dta("C:/Users/LRhinehart/Desktop/K423.dta")
#View(K423)

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

install.packages("devtools")
devtools::install_github("data-edu/tidyLPA")

#2.0 recode and create variables 
##Groups of students
K423$X2SPEDCLEAN = K423$E2RECSPE
K423$X4SPEDCLEAN = K423$E4RECSPE
K423$X6SPEDCLEAN = K423$E6RECSPE
K423$X7SPEDCLEAN = K423$E7RECSPE
K423$X8SPEDCLEAN = K423$E8RECSPE
K423$X2LDCLEAN = K423$E2LRNDIS
K423$X4LDCLEAN = K423$E4LRNDIS
K423$X6LDCLEAN = K423$E6LRNDIS
K423$X7LDCLEAN = K423$E7LRNDIS
K423$X8LDCLEAN = K423$E8LRNDIS
K423$X2ADHDCLEAN = K423$E2ADHD
K423$X4ADHDCLEAN = K423$E4ADHD
K423$X6ADHDCLEAN = K423$E6ADHD
K423$X7ADHDCLEAN = K423$E7ADHD
K423$X8ADHDCLEAN = K423$E8ADHD
##predictors
K423$X1ATTNFSCLEAN = K423$X1ATTNFS
K423$X1INBCNTCLEAN = K423$X1INBCNT
K423$X1TCHAPPCLEAN = K423$X1TCHAPP
K423$X1TCHCONCLEAN = K423$X1TCHCON
K423$X1TCHPERCLEAN = K423$X1TCHPER
K423$X1TCHEXTCLEAN = K423$X1TCHEXT
K423$X1TCHINTCLEAN = K423$X1TCHINT
K423$X1PRNIMPCLEAN = K423$X1PRNIMP
K423$X2CLSNSSCLEAN = K423$X2CLSNSS
K423$X2CNFLCTCLEAN = K423$X2CNFLCT
K423$X1EBRSTOTCLEAN = K423$X1EBRSTOT
K423$X1READING_CLEAN = K423$X1RSCALK4
K423$X1MATH_CLEAN = K423$X1MSCALK4
K423$X1DCCSTOTCLEAN = K423$X1DCCSTOT
K423$X1NRWABLCLEAN = K423$X1NRWABL
K423$X8READING_CLEAN = K423$X8RSCALK4
K423$X8MATH_CLEAN = K423$X8MSCALK4
##Covariates, child
K423$CHSEXCLEAN = K423$X_CHSEX_R
K423$HISPCLEAN = K423$X_HISP_R
K423$AMINANCLEAN = K423$X_AMINAN_R
K423$ASIANBCLEAN = K423$X_ASIAN_R
K423$HAWPICLEAN = K423$X_HAWPI_R
K423$BLACKCLEAN = K423$X_BLACK_R
K423$WHITECLEAN = K423$X_WHITE_R
K423$MULTRCLEAN = K423$X_MULTR_R
K423$X12SESLCLEAN = K423$X12SESL
K423$X12LANGSTCLEAN = K423$X12LANGST
K423$P1NBIFSPCLEAN = K423$P1NBIFSP
K423$KAGECLEAN = K423$X1KAGE_R
K423$X1AGEENTCLEAN = K423$X1AGEENT
##Covariates location
K423$X2PUBPRICLEAN = K423$X2PUBPRI
K423$X2KRCETHCLEAN = K423$X2KRCETH
K423$X2FREE_OR_RED_LUNCHCLEAN = K423$S2LUNCH
K423$S2RDGPCTCLEAN = K423$S2RDGPCT
K423$S2MTHPCTCLEAN = K423$S2MTHPCT
K423$S4ELIQDSCLEAN = K423$S4ELIQDS
K423$S6ELIQDSCLEAN = K423$S6ELIQDS
K423$S7ELIQDSCLEAN = K423$S7ELIQDS
K423$S4ELRTLSCLEAN = K423$S4ELRTL
K423$S6ELRTLSCLEAN = K423$S6ELRTL
K423$S7ELRTLSCLEAN = K423$S7ELRTL
K423$DISTPOVCLEAN = K423$X_DISTPOV
K423$X2REGIONCLEAN = K423$X2REGION
K423$X2LOCALECLEAN = K423$X2LOCALE
K423$S8ELIQDSCLEAN = K423$S8ELIQDS
K423$S8ELRTLSCLEAN = K423$S8ELRTL

######################################################################################################################################
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


#remove negs in outcomes 
#any sped 
K423 <- K423 %>% replace_with_na(replace = list(X2SPEDCLEAN = -9))
summary(K423$X2SPEDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X4SPEDCLEAN = -9))
summary(K423$X4SPEDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X6SPEDCLEAN = -9))
summary(K423$X6SPEDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X7SPEDCLEAN = -9))
summary(K423$X7SPEDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X8SPEDCLEAN = -9))
summary(K423$X8SPEDCLEAN)
table(K423$X8SPEDCLEAN)
K423 %>% count(X8SPEDCLEAN)
#LD
K423 <- K423 %>% replace_with_na(replace = list(X2LDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2LDCLEAN = -1))
summary(K423$X2LDCLEAN)
count(K423$X2LDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X4LDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X4LDCLEAN = -1))
summary(K423$X4LDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X6LDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X6LDCLEAN = -1))
summary(K423$X6LDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X7LDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X7LDCLEAN = -1))
summary(K423$X7LDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X8LDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X8LDCLEAN = -1))
summary(K423$X8LDCLEAN)
#LD All
K423 <- K423 %>% replace_with_na(replace = list(X2LDCLEAN_ALL = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2LDCLEAN_ALL = -1))
summary(K423$X2LDCLEAN_ALL)
K423 <- K423 %>% replace_with_na(replace = list(X8LDCLEAN_ALL = -9))
K423 <- K423 %>% replace_with_na(replace = list(X8LDCLEAN_ALL = -1))
summary(K423$X8LDCLEAN_ALL)
table(K423$X8LDCLEAN)
table(K423$X8LDCLEAN_ALL)
#ADHD
K423 <- K423 %>% replace_with_na(replace = list(X2ADHDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2ADHDCLEAN = -1))
summary(K423$X2ADHDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X4ADHDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X4ADHDCLEAN = -1))
summary(K423$X4ADHDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X6ADHDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X6ADHDCLEAN = -1))
summary(K423$X6ADHDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X7ADHDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X7ADHDCLEAN = -1))
summary(K423$X7ADHDCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X8ADHDCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X8ADHDCLEAN = -1))
summary(K423$X8ADHDCLEAN)


##Predictors (BELOW, IN K, ONLY HAD -9s)
#reverse code: conflict with teacher, externalizing problems, internalizing problems, overactive 
library(naniar)
K423 <- K423 %>% replace_with_na(replace = list(X1ATTNFSCLEAN = -9))
summary(K423$X1ATTNFSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1INBCNTCLEAN = -9))
summary(K423$X1INBCNTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1TCHAPPCLEAN = -9))
summary(K423$X1TCHAPPCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1TCHCONCLEAN = -9))
summary(K423$X1TCHCONCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1TCHPERCLEAN = -9))
summary(K423$X1TCHPERCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1TCHEXTCLEAN = -9))
summary(K423$X1TCHEXTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1TCHINTCLEAN = -9))
summary(K423$X1TCHINTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1PRNIMPCLEAN = -9))
summary(K423$X1PRNIMPCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2CLSNSSCLEAN = -9))
summary(K423$X2CLSNSSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2CNFLCTCLEAN = -9))
summary(K423$X2CNFLCTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1EBRSTOTCLEAN = -9))
summary(K423$X1EBRSTOTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1READING_CLEAN = -9))
summary(K423$X1READING_CLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1MATH_CLEAN = -9))
summary(K423$X1MATH_CLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1DCCSTOTCLEAN = -9))
summary(K423$X1DCCSTOTCLEAN)
table(K423$X1DCCSTOTCLEAN)
#24 are 0
K423 <- K423 %>% replace_with_na(replace = list(X1NRWABLCLEAN = -9))
summary(K423$X1NRWABLCLEAN)
table(K423$X1NRWABLCLEAN)
#255 are 393 (lowest)
#OTHER READING AND MATH SCORES
K423 <- K423 %>% replace_with_na(replace = list(X8READING_CLEAN = -9))
summary(K423$X8READING_CLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X8MATH_CLEAN = -9))
summary(K423$X8MATH_CLEAN)

#COVARIATES 
##GENDER AND RACE/ETHNICITY (ONLY RECODE FOR -9)
K423 <- K423 %>% replace_with_na(replace = list(CHSEXCLEAN = -9))
summary(K423$CHSEXCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(HISPCLEAN = -9))
summary(K423$HISPCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(AMINANCLEAN = -9))
summary(K423$AMINANCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(ASIANBCLEAN = -9))
summary(K423$ASIANBCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(HAWPICLEAN = -9))
summary(K423$HAWPICLEAN)
K423 <- K423 %>% replace_with_na(replace = list(BLACKCLEAN = -9))
summary(K423$BLACKCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(WHITECLEAN = -9))
summary(K423$WHITECLEAN)
K423 <- K423 %>% replace_with_na(replace = list(MULTRCLEAN = -9))
summary(K423$MULTRCLEAN)
##OTHER COVARIATES (ONLY RECODE FOR -9, EXCEPT FOR IFSP, ALSO RECODE FOR -7 AND -8)
K423 <- K423 %>% replace_with_na(replace = list(X12SESLCLEAN = -9))
summary(K423$X12SESLCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X12LANGSTCLEAN = -9))
summary(K423$X12LANGSTCLEAN)
table(K423$X12LANGSTCLEAN)
#change 3 (other) to 1 (non English)
K423$X12LANGSTCLEAN[K423$X12LANGSTCLEAN==3] <- 1
table(K423$X12LANGSTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(P1NBIFSPCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(P1NBIFSPCLEAN = -8))
K423 <- K423 %>% replace_with_na(replace = list(P1NBIFSPCLEAN = -7))
summary(K423$P1NBIFSPCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X1AGEENTCLEAN = -9))
summary(K423$X1AGEENTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(KAGECLEAN = -9))
summary(K423$KAGECLEAN)


#recode 1 and 2 to 0 and 1 
K423$CHSEXCLEAN[K423$CHSEXCLEAN==2] <- 0
table(K423$CHSEXCLEAN)
      
K423$HISPCLEAN[K423$HISPCLEAN==2] <- 0
table(K423$HISPCLEAN)

K423$ASIANBCLEAN[K423$ASIANBCLEAN==2] <- 0
table(K423$ASIANBCLEAN)

K423$BLACKCLEAN[K423$BLACKCLEAN==2] <- 0
table(K423$BLACKCLEAN)

K423$WHITECLEAN[K423$WHITECLEAN==2] <- 0
table(K423$WHITECLEAN)

K423$P1NBIFSPCLEAN[K423$P1NBIFSPCLEAN==2] <- 0
table(K423$P1NBIFSPCLEAN)



library(naniar)
#Covariates: schoool level and location
K423 <- K423 %>% replace_with_na(replace = list(X2PUBPRICLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2PUBPRICLEAN = -1))
summary(K423$X2PUBPRICLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2KRCETHCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2KRCETHCLEAN = -1))
summary(K423$X2KRCETHCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= -9))
K423 <- K423 %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= -1))
summary(K423$X2FREE_OR_RED_LUNCHCLEAN)
#there are x values over 100 
count(filter(K423,X2FREE_OR_RED_LUNCHCLEAN>100))
ggplot(K423, aes(x = "FRL", y = X2FREE_OR_RED_LUNCHCLEAN)) + geom_boxplot()
#there is one outlier here, 350, so I will remove it 
K423 <- K423 %>% replace_with_na(replace = list(X2FREE_OR_RED_LUNCHCLEAN= 350))
summary(K423$X2FREE_OR_RED_LUNCHCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S2RDGPCTCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(S2RDGPCTCLEAN = -1))
summary(K423$S2RDGPCTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S2MTHPCTCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(S2MTHPCTCLEAN = -1))
summary(K423$S2MTHPCTCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S4ELIQDSCLEAN = -9))
summary(K423$S4ELIQDSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S6ELIQDSCLEAN = -9))
summary(K423$S6ELIQDSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S7ELIQDSCLEAN = -9))
summary(K423$S7ELIQDSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S4ELRTLSCLEAN = -9))
summary(K423$S4ELRTLSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S6ELRTLSCLEAN = -9))
summary(K423$S6ELRTLSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S7ELRTLSCLEAN = -9))
summary(K423$S7ELRTLSCLEAN)

K423 <- K423 %>% replace_with_na(replace = list(S8ELRTLSCLEAN = -9))
summary(K423$S8ELRTLSCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(S8ELIQDSCLEAN = -9))
summary(K423$S8ELIQDSCLEAN)

K423 <- K423 %>% replace_with_na(replace = list(DISTPOVCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(DISTPOVCLEAN = -1))
summary(K423$DISTPOVCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2REGIONCLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2REGIONCLEAN = -1))
summary(K423$X2REGIONCLEAN)
K423 <- K423 %>% replace_with_na(replace = list(X2LOCALECLEAN = -9))
K423 <- K423 %>% replace_with_na(replace = list(X2LOCALECLEAN = -1))
summary(K423$X2LOCALECLEAN)
table(K423$X2LOCALECLEAN)
#change locale; combine urban: 12, 13 (other) to 11 
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==12] <- 11
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==13] <- 11
#change locale; combine urban: 22, 23 (other) to 21 
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==22] <- 21
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==23] <- 21
#change locale; combine urban: 32, 33 (other) to 31 
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==32] <- 31
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==33] <- 31
#change locale; combine urban: 42, 43 (other) to 41 
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==42] <- 41
K423$X2LOCALECLEAN[K423$X2LOCALECLEAN==43] <- 41
table(K423$X2LOCALECLEAN)

#change outcomes for adding to the imputation model 
#any sped, 4th 
#check for frequencies 
K423 %>% count(X8SPEDCLEAN)
K423 %>% count(E8RECSPE)
# (removed)
#change 2s and NA to zero 
K423$E8RECSPE[is.na(K423$E8RECSPE)] <- 0
K423$E8RECSPE[K423$E8RECSPE==2] <- 0
#change -9 to NA 
K423 <- K423 %>% replace_with_na(replace = list(E8RECSPE = -9))
#check to make sure 2s and NAs are zero 
K423 %>% count(E8RECSPE)
#change to factor 
K423$E8RECSPE.F <- factor(K423$E8RECSPE)
class(K423$E8RECSPE.F)
K423 %>% count(E8RECSPE.F)
#any LD, 4th  
#change name 
K423$X8LD.for.impute = K423$E8LRNDIS
#check for frequencies 
K423 %>% count(X8LD.for.impute)
# (removed)
#change 2s and NA to zero 
K423$X8LD.for.impute[is.na(K423$X8LD.for.impute)] <- 0
K423$X8LD.for.impute[K423$X8LD.for.impute==2] <- 0
#change -9 and -1 to NA
K423 <- K423 %>% replace_with_na(replace = list(X8LD.for.impute = -9))
K423 <- K423 %>% replace_with_na(replace = list(X8LD.for.impute = -1))
#check work
K423 %>% count(X8LD.for.impute.F)
#change to factor 
K423$X8LD.for.impute.F <- factor(K423$X8LD.for.impute)
class(K423$X8LD.for.impute.F)
K423 %>% count(X8LD.for.impute.F)
#any ADHD, 4th  
#change name
K423$X8ADHD.for.impute = K423$E8ADHD
#check for frequencies 
K423 %>% count(X8ADHD.for.impute)
# (removed)
#change 2s and NA to zero 
K423$X8ADHD.for.impute[is.na(K423$X8ADHD.for.impute)] <- 0
K423$X8ADHD.for.impute[K423$X8ADHD.for.impute==2] <- 0
#change -9 and -1 to NA
K423 <- K423 %>% replace_with_na(replace = list(X8ADHD.for.impute = -9))
K423 <- K423 %>% replace_with_na(replace = list(X8ADHD.for.impute = -1))
#check work
K423 %>% count(X8ADHD.for.impute)
#change to factor 
K423$X8ADHD.for.impute.F <- factor(K423$X8ADHD.for.impute)
class(K423$X8ADHD.for.impute.F)
K423 %>% count(X8ADHD.for.impute.F)

#check for 0/1 and missing 
check_for_nas <- K423 %>%
  count(CHSEXCLEAN.F,X8LD.for.impute.F, X8ADHD.for.impute.F,X2REGIONCLEAN.F,  
        X2PUBPRICLEAN.F, X2KRCETHCLEAN, X2LOCALECLEAN.F, X8SPEDCLEAN,
        HISPCLEAN.F, ASIANBCLEAN.F, BLACKCLEAN.F, 
        WHITECLEAN.F, X12LANGSTCLEAN.F)
summary(check_for_nas)

table(K423$CHSEXCLEAN.F)
table(K423$X8LD.for.impute.F)
table(K423$P1NBIFSPCLEAN.F)

summary(K423$X12SESLCLEAN)
summary(K423$X2CLSNSSCLEAN)
summary(K423$X2CNFLCTCLEAN)
summary(K423$X1AGEENTCLEAN)
summary(K423$KAGECLEAN)
summary(K423$X1EBRSTOTCLEAN)
summary(K423$X1READING_CLEAN)
summary(K423$X1DCCSTOTCLEAN)
summary(K423$X1NRWABLCLEAN)
summary(K423$X1MATH_CLEAN)
summary(K423$X1TCHAPPCLEAN)        
summary(K423$X1ATTNFSCLEAN)
summary(K423$X1INBCNTCLEAN)
summary(K423$X1TCHEXTCLEAN)        
summary(K423$X1TCHINTCLEAN)
summary(K423$X1TCHPERCLEAN)
summary(K423$X1TCHCONCLEAN)
summary(K423$X8MATH_CLEAN)
summary(K423$X8READING_CLEAN)
        

############################################################################################################################################
#3.0 impute 
#impute for dissertation 
#choose variables
#order for least to most missing 

#RERAN ON 7/31

imputed.3.27 <- K423 %>% select(CHILDID, CHSEXCLEAN.F, X8LD.for.impute.F, X8ADHD.for.impute.F,X2REGIONCLEAN.F,  
                               X2PUBPRICLEAN.F, X2KRCETHCLEAN, X2LOCALECLEAN.F, X8SPEDCLEAN,
                               HISPCLEAN.F, ASIANBCLEAN.F, BLACKCLEAN.F, 
                               WHITECLEAN.F, X12LANGSTCLEAN.F, X12SESLCLEAN, X2CLSNSSCLEAN, X2CNFLCTCLEAN, X1AGEENTCLEAN, 
                                KAGECLEAN, X1EBRSTOTCLEAN, X1READING_CLEAN, X1DCCSTOTCLEAN, X1NRWABLCLEAN, X1MATH_CLEAN,
                                X1TCHAPPCLEAN, X1ATTNFSCLEAN, X1INBCNTCLEAN, X1TCHEXTCLEAN, X2FREE_OR_RED_LUNCHCLEAN, 
                                X1TCHINTCLEAN, X1TCHPERCLEAN, X1TCHCONCLEAN, P1NBIFSPCLEAN.F, X8MATH_CLEAN, X8READING_CLEAN, 
                                S8ELRTLSCLEAN.F, S8ELIQDSCLEAN.F, S2RDGPCTCLEAN, S2MTHPCTCLEAN)
#check missing 

sort(sapply(imputed.3.27, function(x) { sum(is.na(x))}), decreasing = TRUE)

#impute with mice

MI.final <- mice(imputed.3.27, m=5, maxit = 5, printFlag = TRUE, seed = 1210)

#1.30 removed multirace, American Indian, and HAWPI because I got a warning message: glm.fit: algorithm did not converge 
#2.1 added all sped 4th, LD 4th, and ADHD 4th, but got glm.fit did not converge error again, so removed all sped 4th 
#after removing the 3 + 1 variables, the model worked 
#3/27 worked with all sped in 4th 
#takes about 15 minutes to run 

#after imputation 

#MI.final$loggedEvents shows CHILDID 

summary(MI.final)

MI.final$imp$X1TCHEXTCLEAN
#shows means of all variables in each imputed dataset...shows means of cat variables, but these are not valid 
MI.final$chainMean
MI.final$imp$CHSEXCLEAN.F

#turn each imp into a dataframe 
imp.data.1 <- complete(MI.final,1)
imp.data.2 <- complete(MI.final,2)
imp.data.3 <- complete(MI.final,3)
imp.data.4 <- complete(MI.final,4)
imp.data.5 <- complete(MI.final,5)

#missing in imputed datasets
sort(sapply(imp.data.1, function(x) { sum(is.na(x))}), decreasing = TRUE)
# no missing in imp.data.1
sort(sapply(imp.data.2, function(x) { sum(is.na(x))}), decreasing = TRUE)
sort(sapply(imp.data.3, function(x) { sum(is.na(x))}), decreasing = TRUE)
sort(sapply(imp.data.4, function(x) { sum(is.na(x))}), decreasing = TRUE)
sort(sapply(imp.data.5, function(x) { sum(is.na(x))}), decreasing = TRUE)
#no missing in other imputed data sets 

mean(imp.data.1$X1ATTNFSCLEAN)
mean(imp.data.2$X1ATTNFSCLEAN)
mean(imp.data.3$X1ATTNFSCLEAN)
mean(imp.data.4$X1ATTNFSCLEAN)
mean(imp.data.5$X1ATTNFSCLEAN)

mean(imp.data.1$X8MATH_CLEAN)
mean(imp.data.1$X8READING_CLEAN)

#mean for all 5 datasets: MI.final$m1,
# not sure how to do this 

?pool

#methods where this works 

methods(coef)
methods(vcov)


#extract all data to one long df
com <- complete(MI.final, "long")
#below works, but this doesn't account for Rubin's rules...SE will probably be off 
mean(com$X1ATTNFSCLEAN)

(MI.final$imp3)

#combine data sets 

#for (i in (1:5)) {MI.final[[i]] <- complete(MI.final$imp)}
#mi_list <- imputationList(MI.final)

#mi_list_2 <- imputationList(MI.final$imp1, MI.final$imp2, MI.final$imp3, MI.final$imp4, MI.final$imp5)

#test.imp.list <- imputationList(imp.data.1, imp.data.2, imp.data.3, imp.data.4, imp.data.5)

#designs.test.mi <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = test.imp.list, nest = TRUE)

#get rid of NAs 
weights.no.na = na.omit(weights)
#create survey weight with 5 imputed datasets 

imp.5.with.weights <- svydesign(id = ~weights.no.na$W8CF8P_2T18PSU, strata = ~weights.no.na$W8CF8P_2T18STR, weights = ~weights.no.na$W8CF8P_2T180, 
                                data= test.imp.list, nest=TRUE)
                          
#############################
#turn MI datasets into a list of imputed datasets 
MIlist <- lapply(seq(MI.final$m), function(im) complete(MI.final, im))
class(MIlist)
#this is a list
impdata.5.list <- imputationList(MIlist)

? imputationList

#####################################################################################################################
#imputed with 4th grade

######################################################################################################################################
#create survey design and apply 
#4.0 

#imp.5.with.weights <- cbind(impdata.5.list, psu = weights$W8CF8P_2T18PSU, str = weights$W8CF8P_2T18STR, weight.1 = weights$W8CF8P_2T180)

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
imp.1.with.weights <- cbind(imp.data.1, psu = weights$W8CF8P_2T18PSU, str = weights$W8CF8P_2T18STR, weight.1 = weights$W8CF8P_2T180)
#get rid of NAs 
imp.1.with.weights = na.omit(imp.1.with.weights)
#make svy design with these weights
designs.test <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = imp.1.with.weights, nest = TRUE)
designs.test <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = imp.1.with.weights, nest = TRUE)
#test design.test with svyglm
#test design.test with svyglm
svymean(~X1MATH_CLEAN, designs.test)
mean(K423$X1MATH_CLEAN, na.rm = TRUE)


#imp 2 with weights
imp.2.with.weights <- cbind(imp.data.2, psu = weights$W8CF8P_2T18PSU, str = weights$W8CF8P_2T18STR, weight.1 = weights$W8CF8P_2T180)
#get rid of NAs 
imp.2.with.weights = na.omit(imp.2.with.weights)
#make svy design with these weights
designs.test.2 <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = imp.2.with.weights, nest = TRUE)
#test design.test with svyglm


designs.test.2 <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = imputationList(1,2,3,4,5), nest = TRUE)

#with MI datasets 

designs.test.mi <- svydesign(id = ~psu, strata = ~ str, weights = ~ weight.1, data = mi_list, nest = TRUE)


svyby(~ASIANBCLEAN.F, designs.test)

#number in sample 
unwtd.count(imp.1.with.weights, designs.test)
# (removed)
svytotal(~X8LD.for.impute.F, designs.test, na.rm=FALSE)
# total     SE
#(removed)
#(removed)
table(imp.data.1$X8LD.for.impute.F)
#(removed) 


table(imp.data.1$X8LD.for.impute.F)

#weights without impute
#get rid of NAs 
weights.no.na = na.omit(weights)
K423.survey.design <- svydesign(id = ~weights.no.na$W8CF8P_2T18PSU, strata = ~weights.no.na$W8CF8P_2T18STR, weights = ~weights.no.na$W8CF8P_2T180, 
                                data= K423, nest=TRUE)


#design with 5 imputed datasets 
designs.test.5 <- svydesign(id = ~weights.no.na$W8CF8P_2T18PSU, strata = ~weights.no.na$W8CF8P_2T18STR, weights = ~weights.no.na$W8CF8P_2T180, data = impdata.5.list, nest = TRUE)

###############################################################################################################

#descriptives 


#frequency and means for all students 
prop.table(svytable(~CHSEXCLEAN.F, design = designs.test))
prop.table(svytable(~HISPCLEAN.F, design = designs.test))
prop.table(svytable(~ASIANBCLEAN.F, design = designs.test))
prop.table(svytable(~BLACKCLEAN.F, design = designs.test))
prop.table(svytable(~WHITECLEAN.F, design = designs.test))
prop.table(svytable(~X12LANGSTCLEAN.F, design = designs.test))
prop.table(svytable(~P1NBIFSPCLEAN.F, design = designs.test))
prop.table(svytable(~X2LOCALECLEAN.F, design = designs.test))
prop.table(svytable(~X2REGIONCLEAN.F, design = designs.test))
prop.table(svytable(~X2PUBPRICLEAN.F, design = designs.test))
prop.table(svytable(~S8ELRTLSCLEAN.F, design = designs.test))
prop.table(svytable(~S8ELIQDSCLEAN.F, design = designs.test))

svymean(~X12SESLCLEAN + X2CLSNSSCLEAN + X2CNFLCTCLEAN + X1AGEENTCLEAN +  
        KAGECLEAN + X1EBRSTOTCLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1MATH_CLEAN
        + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN + X2FREE_OR_RED_LUNCHCLEAN + 
        X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN + X2FREE_OR_RED_LUNCHCLEAN +  S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F
        + S2RDGPCTCLEAN + S2MTHPCTCLEAN + X2KRCETHCLEAN, design = designs.test)

svytotal(~CHSEXCLEAN.F, design = designs.test)
svytotal(~HISPCLEAN.F, design = designs.test)
svytotal(~ASIANBCLEAN.F, design = designs.test)
svytotal(~BLACKCLEAN.F, design = designs.test)
svytotal(~WHITECLEAN.F, design = designs.test)



svytable(~X8LD.for.impute.F, design = designs.test)



#frequency and means for students with LD

LD.design.for.descriptives <- subset(designs.test, X8LD.for.impute.F=="1")

svymean(~X12SESLCLEAN + X2CLSNSSCLEAN + X2CNFLCTCLEAN + X1AGEENTCLEAN +  
          KAGECLEAN + X1EBRSTOTCLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1MATH_CLEAN
        + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN + X2FREE_OR_RED_LUNCHCLEAN + 
          X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN + X2FREE_OR_RED_LUNCHCLEAN +  S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F
        + S2RDGPCTCLEAN + S2MTHPCTCLEAN + X2KRCETHCLEAN, design = LD.design.for.descriptives)

# check to see if the total adds up to weighted number of students with LD...it does :)
svytotal(~CHSEXCLEAN.F, design = LD.design.for.descriptives)


prop.table(svytable(~CHSEXCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~HISPCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~ASIANBCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~BLACKCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~WHITECLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~X12LANGSTCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~P1NBIFSPCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~X2LOCALECLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~X2REGIONCLEAN.F, design = LD.design.for.descriptives))
prop.table(svytable(~X2PUBPRICLEAN.F, design = LD.design.for.descriptives))



#correlation matrix 
for.cor <- K423 %>% select(X2CLSNSSCLEAN, X2CNFLCTCLEAN, X1AGEENTCLEAN, 
                               X1READING_CLEAN, X1DCCSTOTCLEAN, X1NRWABLCLEAN, X1MATH_CLEAN,
                               X1TCHAPPCLEAN, X1ATTNFSCLEAN, X1INBCNTCLEAN, X1TCHEXTCLEAN, X2FREE_OR_RED_LUNCHCLEAN, 
                               X1TCHINTCLEAN, X1TCHPERCLEAN, X1TCHCONCLEAN)
for.cor = na.omit(for.cor)
mean(for.cor$X2CNFLCTCLEAN )
cor(for.cor)
cor(imp.data.3)
####################################################################################################################################
# t tests


svyttest(X2CLSNSSCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X2CNFLCTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1AGEENTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1READING_CLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1DCCSTOTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1NRWABLCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1MATH_CLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHAPPCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1ATTNFSCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1INBCNTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHEXTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHCONCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHINTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHPERCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X1TCHCONCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X12SESLCLEAN~X8LD.for.impute.F, design = designs.test)

svyttest(X2FREE_OR_RED_LUNCHCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(S2MTHPCTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(S2RDGPCTCLEAN~X8LD.for.impute.F, design = designs.test)
svyttest(X2KRCETHCLEAN~X8LD.for.impute.F, design = designs.test)

# chi square 

svychisq(~CHSEXCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~HISPCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~ASIANBCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~BLACKCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~WHITECLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~X12LANGSTCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~P1NBIFSPCLEAN+X8LD.for.impute.F, design = designs.test)
svychisq(~S8ELRTLSCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~S8ELIQDSCLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~X2LOCALECLEAN.F+X8LD.for.impute.F, design = designs.test)
svychisq(~X2REGIONCLEAN.F+X8LD.for.impute.F, design = designs.test)

########################################################################################################################################
#glm with weights and imputation 


#note on outcome 

table(K423$X8LD.for.impute.F)
?svyglm
class(imp.data.1$CHSEXCLEAN.F)
#this is a factor 

#test for aa

glm.fixed.aa <- svyglm(X8LD.for.impute.F ~  as.factor(BLACKCLEAN.F), 
                       design = designs.test, family=binomial(link = "logit"))
summary(glm.fixed.aa)
exp(glm.fixed.aa$coefficients)

#model 1 

glm.fixed.12 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + as.factor(P1NBIFSPCLEAN.F) + KAGECLEAN,
                design = designs.test, family=binomial(link = "logit"))
summary(glm.fixed.12)

exp(glm.fixed.12$coefficients)

#model 1 with imp data set 2
#glm.fixed.1.2.imp <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                      # + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + P1NBIFSPCLEAN + KAGECLEAN,
                       #design = designs.test.mi, family=binomial(link = "logit"))
#summary(glm.fixed.1.2.imp)

#pool 

#pool.compare(glm.fixed.12, glm.fixed.1.2.imp)


#regTermTest(glm.fixed.12, test.terms, method = "Wald", "WorkingWald", "LRT")

#for odds ratio
cbind(OddsRatio = exp(glm.fixed.12$coefficients), exp(confint(glm.fixed.12)))

exp(coef(glm.fixed.12))

anova(glm.fixed.12)
          
#changed family to quasibinomial, but didn't seem to change outcome  

#glm.fixed.123 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
#+ as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + P1NBIFSPCLEAN,
#design = designs.test, family=quasibinomial())
#summary(glm.fixed.123)

#changed family for odds ratio 

glm.fixed.1234 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                       + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + P1NBIFSPCLEAN,
                       design = designs.test, family=quasipoisson(log))
summary(glm.fixed.1234)

# not sure why this doesn't work: print.summary.svyglm.RDS 
#I think this works for odds ratio 
exp(coef(glm.fixed.1))
anova(glm.fixed.12)

#model 2: reading and math 
glm.2 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
       + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + P1NBIFSPCLEAN + KAGECLEAN +
         X1MATH_CLEAN + X1READING_CLEAN,
       design = designs.test, family=binomial(link = "logit"))
summary(glm.2)
exp(glm.2$coefficients)



glm.2.3 <- svyglm(X8LD.for.impute.F ~ CHSEXCLEAN.F + HISPCLEAN.F + ASIANBCLEAN.F + BLACKCLEAN.F + WHITECLEAN.F + X12LANGSTCLEAN + 
                    X12SESLCLEAN + P1NBIFSPCLEAN.F + KAGECLEAN,
                design = designs.test, family=binomial(link = "logit"))
summary(glm.2.3)
exp(glm.2.2$coefficients)


#model 3: add reading AND math and EF (NR and DCCS)

glm.3 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                  P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN,
                design = designs.test, family=binomial(link = "logit"))
  
summary(glm.3)
exp(glm.3$coefficients)


#model 4: add behavior

glm.4 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                  P1NBIFSPCLEAN + KAGECLEAN +
                  X1MATH_CLEAN + X1READING_CLEAN 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                  X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                  X2CLSNSSCLEAN + X2CNFLCTCLEAN,
                design = designs.test, family=binomial(link = "logit"))

summary(glm.4)
exp(glm.4$coefficients)

table(ASIANBCLEAN.F)

#odds ratios 

exp(glm.4$coefficients)


#model 5: add school level variables:



glm.5 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                  P1NBIFSPCLEAN + KAGECLEAN +
                  X1MATH_CLEAN + X1READING_CLEAN 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                  X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                  X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                  S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                  X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN, design = designs.test, family=binomial(link = "logit"))
summary(glm.5)


#model 6: with location 

glm.6 <- svyglm(X8LD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                  P1NBIFSPCLEAN + KAGECLEAN +
                  X1MATH_CLEAN + X1READING_CLEAN 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                  X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                  X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                  S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                  X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN + X2REGIONCLEAN.F + X2LOCALECLEAN.F, 
                design = designs.test, family=binomial(link = "logit"))
summary(glm.6)

glm.6$
glm.6$aic
glm.5$aic
glm.4$aic
glm.fixed.12$aic

glm.6$terms

#for odds ratio
cbind(OddsRatio = exp(glm.6$coefficients), exp(confint(glm.6)))

options(scipen = 999)
exp(glm.6$coefficients)


#separate models 

glm.2.only <- svyglm(X8LD.for.impute.F ~ 
                  X1MATH_CLEAN + X1READING_CLEAN, design = designs.test, family=binomial(link = "logit"))
glm.3.only <- svyglm(X8LD.for.impute.F ~ 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN, design = designs.test, family=binomial(link = "logit"))
glm.4.only <- svyglm(X8LD.for.impute.F ~ 
                             X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                           + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                             X2CLSNSSCLEAN + X2CNFLCTCLEAN, design = designs.test, family=binomial(link = "logit"))
glm.5.only <- svyglm(X8LD.for.impute.F ~ S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                  X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN, design = designs.test, family=binomial(link = "logit"))

glm.5.b.only <- svyglm(X8LD.for.impute.F ~ S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F, design = designs.test, family=binomial(link = "logit"))

glm.6.only <- svyglm(X8LD.for.impute.F ~ X2REGIONCLEAN.F + X2LOCALECLEAN.F, 
                design = designs.test, family=binomial(link = "logit"))

summary(glm.2.only)
#both reading and math sig
summary(glm.3.only)
# WM but not DCCS significant 
summary(glm.4.only)
#only approaches are significant
summary(glm.5.only)
#none are significant
summary(glm.5.b.only)
#neither are sig 
summary(glm.6.only)
# regions and locale are sig 
#######################################################################



##################################################################################################################################
#with lmer4 
#glm with sampling weight 

glmer1 <- glmer(K423$X8LD.for.impute.F ~ K423$X1MATH_CLEAN + K423$X1READING_CLEAN, weights = weights$W8CF8P_2T180, 
      family="binomial")

#with imputed dataset 

glmer1.1 <- glmer(imp.data.1$X8LD.for.impute.F ~ imp.data.1$X1MATH_CLEAN + imp.data.1$X1READING_CLEAN, weights = weights$W8CF8P_2T180, 
                family="binomial")


#################################################################################################################################
#glm with MI data 


#model 2 
fit.glm.with.mice.2 <- glm.mids(X8LD.for.impute.F ~ X1MATH_CLEAN + X1READING_CLEAN, data = MI.final, family="binomial")
fit.glm.with.mice.2
summary(pool(fit.glm.with.mice.2))

#model 3 
fit.glm.with.mice.3 <- glm.mids(X8LD.for.impute.F ~ X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN, 
                                data = MI.final, family="binomial")
fit.glm.with.mice.3
summary(pool(fit.glm.with.mice.3))



########################################################################################################################

#6.0 LPA for LD group 

#4th grade predictors 
#closeness and conflict not available, also attentional focus and inhibitory contol not available 

K423$X8TCHAPPCLEAN = K423$X8TCHAPP
K423$X8TCHCONCLEAN = K423$X8TCHCON
K423$X8TCHPERCLEAN = K423$X8TCHPER
K423$X8TCHEXTCLEAN = K423$X8TCHEXT
K423$X8TCHINTCLEAN = K423$X8TCHINT
K423$X8CLSNSSCLEAN = K423$X8CLSNSS
K423$X8CNFLCTCLEAN = K423$X8CNFLCT
K423$X8DCCSTOTCLEAN = K423$X8DCCSSCR
K423$X8NRWABLCLEAN = K423$X8NRWABL
K423$X8READING_CLEAN = K423$X8RSCALK4
K423$X8MATH_CLEAN = K423$X8MSCALK4

lpa.glm <- svyglm(X8LD.for.impute.F ~ 
                  X1MATH_CLEAN + X1READING_CLEAN 
                + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                  X1TCHAPPCLEAN 
                design = designs.test, family=binomial(link = "logit"))
summary(lpa.glm)

#create group with just LD in 4th with imputed data 
subset(imp.data.1, subset = (X8LD.for.impute.F==1))->LDgroup_4th_LPA
attach(LDgroup_4th_LPA)

summary(LDgroup_4th_LPA)
length(LDgroup_4th_LPA)
nrow(LDgroup_4th_LPA)


LDgroup_4th_LPA %>% 
  subset(select = c("X1NRWABLCLEAN", "X1MATH_CLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(n_profiles = 1:3)


warnings()
warning
LDgroup_4th_LPA %>% 
  subset(select = c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(2)

LDgroup_4th_LPA %>% 
  subset(select = c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(3)

LDgroup_4th_LPA %>% 
  subset(select = c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(4)
 
LDgroup_4th_LPA %>% 
  subset(select = c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(5)

estimate_profiles(LDgroup_4th_LPA, 4)


lpa.test.1 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                  n_profiles = 1)
lpa.test.2 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                                n_profiles = 2)
lpa.test.3 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                                n_profiles = 3)
lpa.test.4 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                                n_profiles = 4)
lpa.test.5 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                                n_profiles = 5)
lpa.test.6 <- estimate_profiles(LDgroup_4th_LPA, X1MATH_CLEAN, X1READING_CLEAN, X1NRWABLCLEAN, X1TCHAPPCLEAN,
                                n_profiles = 6)

#in above profile 3 is best

lpa.test.3$profile
mean(lpa.test.3$profile,lpa.test.3$X1NRWABLCLEAN)

plot_profiles(lpa.test.3, to_center = TRUE, to_scale = TRUE)

get_estimates(lpa.test.3)

get_data(lpa.test.3)

#look at demogrpahics within 
#first merge groups back to dataset 

lpa.LD.march.merge <- cbind(LDgroup_4th_LPA, lpa.test.3$profile)


#check averge WM in both groups 
lpa.LD.march.merge %>% group_by(lpa.test.3$profile)%>%
  dplyr::summarise(Mean = mean(X1NRWABLCLEAN))

#test
mean(LDgroup_4th_LPA$X1READING_CLEAN)
mean(LDgroup_4th_LPA$X1NRWABLCLEAN)
######################################################
#with tidylpa 1.0
LDgroup_4th_LPA %>% 
  subset(select =c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(n_profiles = 1:6) 

lpa.3 <- LDgroup_4th_LPA %>% 
  subset(select =c("X1MATH_CLEAN", "X1READING_CLEAN", "X1NRWABLCLEAN", "X1TCHAPPCLEAN")) %>% 
  estimate_profiles(3) 
get_estimates(lpa.3)

#3 profiles are still best 

print(get_estimates(lpa.3), n=24)

#make the chart 
plot_profiles(lpa.3)

lpa.3$model_1_class_3

#create a dataframe
lpa.3.df <- lpa.3$model_1_class_3$dff

#add to dataframe used for analysis 
lpa.3.df.merge <- cbind(LDgroup_4th_LPA, lpa.3.df$Class)

#change name of class to profile 

names(lpa.3.df.merge)[names(lpa.3.df.merge)== 'lpa.3.df$Class'] <- 'profile'

#count in each group

table(lpa.3.df.merge$profile)
       
#look at demographics within group 

lpa.3.df.merge %>% group_by(profile)%>%
  count("Gender" = CHSEXCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(HISPCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(ASIANBCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(BLACKCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(WHITECLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(X12LANGSTCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(P1NBIFSPCLEAN) %>% 
  mutate(prop = prop.table(n))


lpa.3.df.merge %>% group_by(profile)%>%
  count(S8ELRTLSCLEAN.F) %>% 
  mutate(prop = prop.table(n))


lpa.3.df.merge %>% group_by(profile)%>%
  count(S8ELIQDSCLEAN.F) %>% 
  mutate(prop = prop.table(n))

lpa.3.df.merge %>% group_by(profile)%>%
  count(X8ADHD.for.impute.F) %>% 
  mutate(prop = prop.table(n))



################################################
#mean 
lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X2FREE_OR_RED_LUNCHCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8READING_CLEAN))


lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))


lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X2KRCETHCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(S2MTHPCTCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(S2RDGPCTCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(KAGECLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X12SESLCLEAN))


##ANOVAs to check for differences among profiles by school/place

a.race.profiles <- aov(X2KRCETHCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.race.profiles)

#Tukey
TukeyHSD(a.race.profiles, "profile", ordered = TRUE)

a.reading.profiles <- aov(S2RDGPCTCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.reading.profiles)

a.math.profiles <- aov(S2MTHPCTCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.math.profiles)

a.frl.profiles <- aov(X2FREE_OR_RED_LUNCHCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.frl.profiles)



#chi square to check for differences 

chisq.test(lpa.3.df.merge$profile, lpa.3.df.merge$S8ELRTLSCLEAN.F)
chisq.test(lpa.3.df.merge$profile, lpa.3.df.merge$S8ELIQDSCLEAN.F)

#don't have location variables in this dataframe 

#################################################################################################################################
###############################################################################################################################
#ADHD section 

#glm for adhd 

#frequencies for adhd 
#frequency and means for students with LD

ADHD.design.for.descriptives <- subset(designs.test, X8ADHD.for.impute.F=="1")

svymean(~X12SESLCLEAN + X2CLSNSSCLEAN + X2CNFLCTCLEAN + X1AGEENTCLEAN +  
          KAGECLEAN + X1EBRSTOTCLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1MATH_CLEAN
        + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN + X2FREE_OR_RED_LUNCHCLEAN + 
          X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN + X2FREE_OR_RED_LUNCHCLEAN +  S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F
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
prop.table(svytable(~X8LD.for.impute.F, design = ADHD.design.for.descriptives))

table(imp.data.1$X8ADHD.for.impute.F, imp.data.1$ASIANBCLEAN.F)
table(imp.data.1$X8ADHD.for.impute.F, imp.data.1$BLACKCLEAN.F)
table(imp.data.1$X8ADHD.for.impute.F, imp.data.1$X12LANGSTCLEAN.F)

table(imp.data.1$X8ADHD.for.impute.F, imp.data.1$ASIANBCLEAN.F)
table(K423$X8ADHD.for.impute.F, K423$BLACKCLEAN.F)

#number of students with ADHD 
svytable(~X8ADHD.for.impute.F, design = designs.test)

svytable(~X8ADHD.for.impute.F+X8LD.for.impute.F, design = designs.test)

table(K423$X8ADHD.for.impute.F, K423$X8LD.for.impute.F)

#model 1
glm.1.adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN,
                     design = designs.test, family=binomial(link = "logit"))
summary(glm.1.adhd)

#model 2: reading and math 
glm.2.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN,
                     design = designs.test, family="binomial")
summary(glm.2.adhd)

#model 3: add EF (NR and DCCS)

glm.3.adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN, design = designs.test, family="binomial")
summary(glm.3.adhd)


#model 4: add behavior 
glm.4.adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN , design = designs.test, family="quasibinomial")
summary(glm.4.adhd)

#with fewer behavior
glm.4.2adhd <- svyglm(X8ADHD.for.impute.F ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1MATH_CLEAN + X1READING_CLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN + X1ATTNFSCLEAN + 
                        X1TCHEXTCLEAN + X1TCHINTCLEAN + X2CNFLCTCLEAN + X1TCHCONCLEAN, design = designs.test, family="quasibinomial")
summary(glm.4.2adhd)


#just behavior 

glm.4.1.adhd <- svyglm(X8ADHD.for.impute.F ~ X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                       + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN , design = designs.test, family="quasibinomial")
summary(glm.4.1.adhd)




#model 5: add school level 
glm.5.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN +
                       X1MATH_CLEAN + X1READING_CLEAN 
                     + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                       X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                       X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                       S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                       X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN, design = designs.test, family="quasibinomial")
summary(glm.5.adhd)

#model 6: add school level 
glm.6.adhd <- svyglm(X8ADHD.for.impute.F ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN +
                       X1MATH_CLEAN + X1READING_CLEAN 
                     + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                       X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                       X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                       S8ELRTLSCLEAN.F + S8ELIQDSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                       X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN + X2REGIONCLEAN.F + X2LOCALECLEAN.F, design = designs.test, family="quasibinomial")
summary(glm.6.adhd)

#adhd 

#just factors
glm.8.adhd <- svyglm(X8ADHD.for.impute.F ~
                       X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                     + X1TCHINTCLEAN + X1TCHPERCLEAN + X1TCHCONCLEAN +
                       X2CLSNSSCLEAN + X2CNFLCTCLEAN, design = designs.test, family="quasibinomial")
summary(glm.8.adhd)

#adhd just reading and math

glm.9.adhd <- svyglm(X8ADHD.for.impute.F ~ X1MATH_CLEAN + X1READING_CLEAN, design = designs.test, family="quasibinomial")
summary(glm.9.adhd)

###############################################################################################################################

#ADHD LPA 

subset(imp.data.1, subset = (X8ADHD.for.impute.F==1))->ADHD_group_4th_LPA

ADHD_group_4th_LPA %>% 
  subset(select =c("X1NRWABLCLEAN", "X1ATTNFSCLEAN", "X1NRWABLCLEAN", "X2CNFLCTCLEAN")) %>% 
  estimate_profiles(n_profiles = 1:6) 


adhd.lpa.2 <- ADHD_group_4th_LPA %>% 
  subset(select =c("X1NRWABLCLEAN", "X1ATTNFSCLEAN", "X2CNFLCTCLEAN")) %>% 
  estimate_profiles(2) 
get_estimates(adhd.lpa.2)


print(get_estimates(adhd.lpa.2), n=24)

#make the chart 
plot_profiles(adhd.lpa.2)

#create a dataframe
adhd.lpa.2.df <- adhd.lpa.2$model_1_class_2$dff

#add to dataframe used for analysis 
adhd.lpa.2.df.merge <- cbind(ADHD_group_4th_LPA, adhd.lpa.2.df$Class)

#change name of class to profile 

names(adhd.lpa.2.df.merge)[names(adhd.lpa.2.df.merge)== 'adhd.lpa.2.df$Class'] <- 'profile'

#count in each group

table(adhd.lpa.2.df.merge$profile)

#look at demographics within group 

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count("Gender" = CHSEXCLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(HISPCLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(ASIANBCLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(BLACKCLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(WHITECLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(X12LANGSTCLEAN.F) %>% 
  mutate(prop = prop.table(n))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  count(P1NBIFSPCLEAN) %>% 
  mutate(prop = prop.table(n))



################################################
#mean 
adhd.lpa.2.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X2FREE_OR_RED_LUNCHCLEAN))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8READING_CLEAN))

adhd.lpa.2.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X8MATH_CLEAN))


adhd.lpa.2.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X2KRCETHCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(S2MTHPCTCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(S2RDGPCTCLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(KAGECLEAN))

lpa.3.df.merge %>% group_by(profile)%>%
  dplyr::summarise(Mean = mean(X12SESLCLEAN))


##ANOVAs to check for differences among profiles by school/place

a.race.profiles <- aov(X2KRCETHCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.race.profiles)

#Tukey
TukeyHSD(a.race.profiles, "profile", ordered = TRUE)

a.reading.profiles <- aov(S2RDGPCTCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.reading.profiles)

a.math.profiles <- aov(S2MTHPCTCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.math.profiles)

a.frl.profiles <- aov(X2FREE_OR_RED_LUNCHCLEAN ~ profile, data = lpa.3.df.merge)
summary.aov(a.frl.profiles)



#chi square to check for differences 

chisq.test(lpa.3.df.merge$profile, lpa.3.df.merge$S8ELRTLSCLEAN.F)
chisq.test(lpa.3.df.merge$profile, lpa.3.df.merge$S8ELIQDSCLEAN.F)

#don't have location variables in this dataframe 



##########################################################################################################################


###########################################################################################
#LA no LD 

#impute new dataset 
imputed.7.24 <- K423 %>% select(CHILDID, CHSEXCLEAN.F, X8LD.for.impute.F, X8ADHD.for.impute.F,X2REGIONCLEAN.F,  
                                X2PUBPRICLEAN.F, X2KRCETHCLEAN, X2LOCALECLEAN.F, X8SPEDCLEAN,
                                HISPCLEAN.F, ASIANBCLEAN.F, BLACKCLEAN.F, 
                                WHITECLEAN.F, X12LANGSTCLEAN.F, X12SESLCLEAN, X2CLSNSSCLEAN, X2CNFLCTCLEAN, X1AGEENTCLEAN, 
                                KAGECLEAN, X1EBRSTOTCLEAN, X1READING_CLEAN, X1DCCSTOTCLEAN, X1NRWABLCLEAN, X1MATH_CLEAN,
                                X1TCHAPPCLEAN, X1ATTNFSCLEAN, X1INBCNTCLEAN, X1TCHEXTCLEAN, X2FREE_OR_RED_LUNCHCLEAN, 
                                X1TCHINTCLEAN, X1TCHPERCLEAN, X1TCHCONCLEAN, P1NBIFSPCLEAN, X8MATH_CLEAN, X8READING_CLEAN,
                                S8ELRTLSCLEAN.F, S8ELIQDSCLEAN.F, S2RDGPCTCLEAN, S2MTHPCTCLEAN)
#check missing 

sort(sapply(imputed.7.24, function(x) { sum(is.na(x))}), decreasing = TRUE)

#impute with mice

MI.final.LANSE <- mice(imputed.7.24, m=5, maxit = 5, printFlag = TRUE, seed = 1210)

#after imputation 

summary(MI.final.LANSE)

MI.final.LANSE$imp$X1TCHEXTCLEAN
#shows means of all variables in each imputed dataset...shows means of cat variables, but these are not valid 
MI.final.LANSE$chainMean
MI.final.LANSE$imp$CHSEXCLEAN.F

#turn each imp into a dataframe 
imp.data.1.LANSE <- complete(MI.final.LANSE,1)
imp.data.2.LANSE <- complete(MI.final.LANSE,2)
imp.data.3.LANSE <- complete(MI.final.LANSE,3)
imp.data.4.LANSE <- complete(MI.final.LANSE,4)
imp.data.5.LANSE <- complete(MI.final.LANSE,5)

#missing in imputed datasets
sort(sapply(imp.data.1.LANSE, function(x) { sum(is.na(x))}), decreasing = TRUE)
# no missing

#find lowest 10th percentile 
quantile(imp.data.1.LANSE$X8READING_CLEAN, c(.10))
#10th percentile 107.43
quantile(imp.data.1.LANSE$X8MATH_CLEAN, c(.10))
#1st quartile is 88.16

#imp.data.1.with.LA <- imp.data.1 %>% 
 # mutate(LA_no_LD = ifelse(X8LD.for.impute.F == "0" & X8READING_CLEAN < 107.26 & X8MATH_CLEAN < 87.23,1,0))

imp.data.1.with.LA.chapter.4.0 <- imp.data.1.LANSE %>% 
  filter(X8READING_CLEAN < 107.43 & X8MATH_CLEAN < 88.16)
  
#count students in sped in the LA group, 4th grade 
table(imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
# (removed)

table(imp.data.1.with.LA.chapter.4.0$CHSEXCLEAN.F)

table(K423$X8SPEDCLEAN)
# (removed)

nrow(imp.data.1.with.LA.chapter.4.0)
#(removed)

#imp.data.1.with.LA.chapter.4 %>%
  #mutate(LA_no_sped = ifelse(X8SPEDCLEAN == "0",1,0))


#imp.data.1.with.LA.sped.yes <- imp.data.1 %>% 
  #mutate(LA_no_sped = ifelse(X8SPEDCLEAN == "1" & X8READING_CLEAN < 107.43 & X8MATH_CLEAN < 87.84,1,0))
#table(imp.data.1.with.LA.sped.yes)


#imp.data.1.with.LA.math <- imp.data.1 %>% 
  #mutate(LA_no_sped = ifelse(X8SPEDCLEAN == "0" & X8MATH_CLEAN < 87.84,1,0))

#table(imp.data.1.with.LA$LA_no_sped, imp.data.1.with.LA$CHSEXCLEAN.F)

#(removed)
#don"t add count! 

#####################################################


#########################################################################################

#descriptive stats

#1 = sped yes 

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X8READING_CLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X8READING_CLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X8MATH_CLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X8MATH_CLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1READING_CLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X1READING_CLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1MATH_CLEAN))


group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1DCCSTOTCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X1DCCSTOTCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1NRWABLCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X1NRWABLCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1TCHAPPCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X1TCHAPPCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X1TCHEXTCLEAN ))
t.test(imp.data.1.with.LA.chapter.4.0$X1TCHEXTCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(KAGECLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$KAGECLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X12SESLCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X12SESLCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)



group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(S2RDGPCTCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$S2RDGPCTCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(S2MTHPCTCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$S2MTHPCTCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X2FREE_OR_RED_LUNCHCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X2FREE_OR_RED_LUNCHCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  summarise(m=mean(X2KRCETHCLEAN))
t.test(imp.data.1.with.LA.chapter.4.0$X2KRCETHCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

#categorical variables
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(CHSEXCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$CHSEXCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(HISPCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$HISPCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(ASIANBCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$ASIANBCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(BLACKCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$BLACKCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(WHITECLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$WHITECLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(P1NBIFSPCLEAN)
chisq.test(imp.data.1.with.LA.chapter.4.0$P1NBIFSPCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(X12LANGSTCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$X12LANGSTCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

group_by(imp.data.1.with.LA.chapter.4.0, X8SPEDCLEAN) %>%
  count(S8ELRTLSCLEAN.F)
chisq.test(imp.data.1.with.LA.chapter.4.0$S8ELRTLSCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)

glm

#don't need below
table(imp.data.1.with.LA.chapter.4.0$ASIANBCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$BLACKCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$WHITECLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$HISPCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$X12LANGSTCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$P1NBIFSPCLEAN,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
table(imp.data.1.with.LA.chapter.4.0$S8ELRTLSCLEAN.F,imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)
###################################################################################
#logistic regression 

#recode no sped is reference group 
imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit=imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN
imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit[imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit==1] <- 2
imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit[imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit==0] <- 1
imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit[imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit==2] <- 0
table(imp.data.1.with.LA.chapter.4.0$sped.for.lansep.logit)
table(imp.data.1.with.LA.chapter.4.0$X8SPEDCLEAN)


#model 1
glm.1.LANSE <- glm(sped.for.lansep.logit ~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN, data = imp.data.1.with.LA.chapter.4.0, family="binomial")
summary(glm.1.LANSE)
exp(coef(glm.1.LANSE))

#model 2: EF
glm.2.LANSE <- glm(sped.for.lansep.logit ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN,
                   data = imp.data.1.with.LA.chapter.4.0, family="binomial")
summary(glm.2.LANSE)
exp(coef(glm.2.LANSE))

#model 4: add behavior 
glm.4.1.LANSE <- glm(sped.for.lansep.logit~ as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                       P1NBIFSPCLEAN + KAGECLEAN + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                       X1TCHAPPCLEAN + X1TCHEXTCLEAN, data = imp.data.1.with.LA.chapter.4.0, family="binomial")
summary(glm.4.1.LANSE)
exp(coef(glm.4.1.LANSE))

#model 5: add school level 

#glm.5.LANSE <- glm(X8SPEDCLEAN ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                     #+ as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                     #  P1NBIFSPCLEAN + KAGECLEAN +
                     #+ X1DCCSTOTCLEAN + X1NRWABLCLEAN +
                      # X1TCHAPPCLEAN + X1TCHEXTCLEAN 
                     #+ X1TCHINTCLEAN +
                     #  X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                     #  S8ELRTLSCLEAN.F + S2RDGPCTCLEAN + S2MTHPCTCLEAN +
                      # X2FREE_OR_RED_LUNCHCLEAN + X2KRCETHCLEAN, data = imp.data.1.with.LA.chapter.4.0, family="binomial")
#summary(glm.5.LANSE)
#exp(coef(glm.5.LANSE))

#multilevel 
glm.6.mm.LANSE <-glmer(X8SPEDCLEAN ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
  + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
    P1NBIFSPCLEAN + KAGECLEAN +
    + X1DCCSTOTCLEAN + X1NRWABLCLEAN +
    X1TCHAPPCLEAN + X1TCHEXTCLEAN +
     (1 |X2KRCETHCLEAN),data = imp.data.1.with.LA.chapter.4.0, family="binomial")
summary(glm.6.mm.LANSE)
#Warning messages:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.0752055 (tol = 0.001, component 1)
#                2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                                  Model is nearly unidentifiable: very large eigenvalue
#                                - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#                                - Rescale variables?

(1 |S8ELRTLSCLEAN.F) + (1 |S2RDGPCTCLEAN) + (1 |S2MTHPCTCLEAN) + (1 | X2FREE_OR_RED_LUNCHCLEAN) 


#try other glm
glm.6.mm.LANSE <-glmer(X8SPEDCLEAN ~  as.factor(CHSEXCLEAN.F) + as.factor(HISPCLEAN.F) + as.factor(ASIANBCLEAN.F) 
                       + as.factor(BLACKCLEAN.F) + as.factor(WHITECLEAN.F) + as.factor(X12LANGSTCLEAN.F) + X12SESLCLEAN + 
                         P1NBIFSPCLEAN + KAGECLEAN +
                         X1TCHAPPCLEAN + X1ATTNFSCLEAN + X1INBCNTCLEAN + X1TCHEXTCLEAN 
                       + X1TCHINTCLEAN +
                         X2CLSNSSCLEAN + X2CNFLCTCLEAN +
                         (1 |X2KRCETHCLEAN),data = imp.data.1.with.LA.chapter.4.0, family="binomial")
summary(glm.6.mm.LANSE)
