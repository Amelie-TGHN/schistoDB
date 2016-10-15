# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to assess the 'design quality' of studies, by counting
# the number of studies/cohorts that were registered, controlled, randomised
# and blind; and by examining how those design elements were implemented.


#----------
# Trial registries where the studies were registered, as specified in articles:
table(sDB_study$A1_trialRegistry)


#----------
# Type of study (efficacy, safety, PK/PD, other)
table(sDB_study$A2_efficacy)
table(sDB_study$A2_safety)
table(sDB_study$A2_pharma)

length(which(sDB_study$A2_efficacy == 1 & sDB_study$A2_safety == 1))
# All safety studies are also efficacy studies (as expected, because
# safety only studies were not eligible for the systematic review)

table(sDB_study$A2_other)

#----------
# Percentage of comparative (controlled), randomised, and blind studies

# by study
compA <- sum(sDB_study$A3_control)
randA <- sum(sDB_study$A4_random)
blindA <- sum(sDB_study$A5_blind)

studyDesign <- c(compA, randA, blindA)
names(studyDesign) <- c("control", "random", "blind")

n <- nrow(sDB_study)
studyDesign_percent <- round(studyDesign/n*100, 2)

# by cohort
compB <- sum(sDB_cohort$A3_control)
randB <- sum(sDB_cohort$A4_random)
blindB <- sum(sDB_cohort$A5_blind)

cohortDesign <- c(compB, randB, blindB)
names(cohortDesign) <- c("control", "random", "blind")

m <- nrow(sDB_cohort)
cohortDesign_percent <- round(cohortDesign/m*100, 2)

# Overall, summary:
design <- rbind(studyDesign, studyDesign_percent,
                cohortDesign, cohortDesign_percent)
print(design)
# Reminder: The 'control' category also includes 'comparative' studies
# (comparison between different groups, e.g. age groups)


#----------
# More precisely, type of comparative/controlled studies

ctrlCat <- c(levels(sDB_study$A3_ctrlCat1), levels(sDB_study$A3_ctrlCat2))
ctrlCat <- ctrlCat[!duplicated(ctrlCat)]
print(ctrlCat) # Summary of all levels of ctrlCat (1 and 2)

comparative <- c("healthy controls", "other (treatment history)",
                 "other (endemic context)")
# 'comparative' rather than controlled study

controlLow <- c("delayed treatment",  "untreated controls")
# controlled, but the control group received no intervention

controlHigh <- c("dose comparison", "drug comparison",
                 "placebo", "regimen comparison", "regimen + placebo")
# controlled against a placebo or comparator drug/dose/regimen

length(c(comparative, controlLow, controlHigh))==length(ctrlCat)
# expect TRUE if no category has been missed


# Which and how many controlled studies/cohorts used a comparator?
sDB_study$A3_controlHigh <- (sDB_study$A3_ctrlCat1 %in% controlHigh |
                             sDB_study$A3_ctrlCat2 %in% controlHigh)
study_controlHigh <- sum(sDB_study$A3_controlHigh)

sDB_cohort$A3_controlHigh <- (sDB_cohort$A3_ctrlCat1 %in% controlHigh |
                              sDB_cohort$A3_ctrlCat2 %in% controlHigh)
cohort_controlHigh <- sum(sDB_cohort$A3_controlHigh)


# Which studies were comparative rather than controlled?
sDB_study$A3_comparative <- (sDB_study$A3_ctrlCat1 %in% comparative |
                             sDB_study$A3_ctrlCat2 %in% comparative)
study_comparative <- sum(sDB_study$A3_comparative)
# Number of studies for which the 'quality of control' criterion does not apply


#----------
# Assumed quality of randomisation and blinding means

# Computer-generated randomisation sequence:
randMeth_study <- table(subset(sDB_study, A4_random == 1)$A4_randMethod)
study_computerRand <- unname(sum(randMeth_study) - randMeth_study["unclear"])

randMeth_cohort <- table(subset(sDB_cohort, A4_random == 1)$A4_randMethod)
cohort_computerRand <- unname(sum(randMeth_cohort) - randMeth_cohort["unclear"])


# At least double-blind (otherwise, single/assessor-blinded, or unclear): 
blindLev_study <- table(subset(sDB_study, A5_blind == 1)$A5_blindLevel)
study_multiBlind <- unname(sum(blindLev_study["double"], blindLev_study["triple"]))

blindLev_cohort <- table(subset(sDB_cohort, A5_blind == 1)$A5_blindLevel)
cohort_multiBlind <- unname(sum(blindLev_cohort["double"], blindLev_cohort["triple"]))


#----------
# Summary of 'high quality' designs (i.e. corresponding to
# current 'gold standards' of clinical trials):

studyDesign_high <- c(study_controlHigh, study_computerRand, study_multiBlind)
names(studyDesign_high) <- c("control", "random", "blind")
studyDesign_high_percent <- round(studyDesign_high/n*100, 2)

cohortDesign_high <- c(cohort_controlHigh, cohort_computerRand, cohort_multiBlind)
names(cohortDesign_high) <- c("control", "random", "blind")
cohortDesign_high_percent <- round(cohortDesign_high/m*100, 2)


# Overall summary of designs (all, as compared to 'high quality' ones):
studyDesign_qual <- cbind(studyDesign, studyDesign_percent,
                          studyDesign_high, studyDesign_high_percent)
cohortDesign_qual <- cbind(cohortDesign, cohortDesign_percent,
                           cohortDesign_high, cohortDesign_high_percent) 
print(studyDesign_qual)
print(cohortDesign_qual)


#----------
# Number of studies with one cohort only
u <- sDB_cohort$A0_S[which(duplicated(sDB_cohort$A0_S))]
length(sDB_study$A0_S) - length(unique(u))