# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code depicts how outcome measures were reported in published results
# (as cure rates or ERR, etc.)


#----------
# Number of studies using Egg Reduction Rate as a primary outcome measure
# and type of ERR mean used

studyEggs <- table(sDB_study$D5_mean)
studyERRmean <- as.vector(studyEggs)  # type of mean used for ERR calculation
names(studyERRmean) <- c("arithmetic", "both", "geometric", "unclear")

# Any study using ERR but not CR?
index <- which(sDB_cohort$D4_ERR == 1 & sDB_cohort$D3_CR == 0)

sDB_cohort[index, "B5_reportMeans"] # only one, with little information available...
sDB_cohort[index, "D5_mean"]


#----------
# Number of studies comparing the intensity of infection before and after
# (but without expressing it as the reduction in mean of eggs)
length(which(sDB_study$D4_ERR == 0 & sDB_study$D4_intens == 1))

# Number of studies reporting a cure rate only
length(which(sDB_study$D4_ERR == 0 & sDB_study$D4_intens == 0 &
             sDB_study$D3_CR == 1))

# Number of studies reporting none of the above
length(which(sDB_study$D4_ERR == 0 & sDB_study$D4_intens == 0 &
             sDB_study$D3_CR == 0))


#----------
# Total using Cure Rate and Egg Reduction Rate, by cohort and study

cohortCR <- sum(sDB_cohort$D3_CR)
studyCR <- sum(sDB_study$D3_CR)

cohortERR <- sum(sDB_cohort$D4_ERR)
studyERR <- sum(sDB_study$D4_ERR)
