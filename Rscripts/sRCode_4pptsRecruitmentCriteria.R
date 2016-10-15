# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to see which inclusion/exclusion criteria were used
# to select study participants.


#----------
# Inclusion criteria

sDB_cohort$G0_healthBin <- !is.na(sDB_cohort$G0_incluHealthy)
sDB_cohort$G1_morphoBin <- !is.na(sDB_cohort$G1_incluMorph)
sDB_cohort$G3_compliantBin <- !is.na(sDB_cohort$G3_incluCompliant)
sDB_cohort$G4_consentBin <- !is.na(sDB_cohort$G4_incluConsent)
sDB_cohort$G5_schoolBin <- !is.na(sDB_cohort$G5_incluSchool)
sDB_cohort$G6_residencyBin <- !is.na(sDB_cohort$G6_incluResidency)
sDB_cohort$G7_incluBin <- !is.na(sDB_cohort$G7_incluOther)

# Corresponding number of participants

pptsHealth <- tapply(sDB_cohort$D1_infected, sDB_cohort$G0_healthBin,
                     sum, na.rm=TRUE)["TRUE"]
pptsMorpho <- tapply(sDB_cohort$D1_infected, sDB_cohort$G1_morphoBin,
                     sum, na.rm=TRUE)["TRUE"]
pptsCompliant <- tapply(sDB_cohort$D1_infected, sDB_cohort$G3_compliantBin,
                        sum, na.rm=TRUE)["TRUE"]
pptsConsent <- tapply(sDB_cohort$D1_infected, sDB_cohort$G4_consentBin,
                      sum, na.rm=TRUE)["TRUE"]
pptsSchool <- tapply(sDB_cohort$D1_infected, sDB_cohort$G5_schoolBin,
                     sum, na.rm=TRUE)["TRUE"]
pptsResidency <- tapply(sDB_cohort$D1_infected, sDB_cohort$G6_residencyBin,
                        sum, na.rm=TRUE)["TRUE"]
pptsIncluOthers <- tapply(sDB_cohort$D1_infected, sDB_cohort$G7_incluBin,
                          sum, na.rm=TRUE)["TRUE"]

# Summary table

health <- c(sum(sDB_cohort$G0_healthBin), pptsHealth)
morpho <- c(sum(sDB_cohort$G1_morphoBin), pptsMorpho)
compliant <- c(sum(sDB_cohort$G3_compliantBin), pptsCompliant)
consent <- c(sum(sDB_cohort$G4_consentBin), pptsConsent)
school <- c(sum(sDB_cohort$G5_schoolBin), pptsSchool)
residency <- c(sum(sDB_cohort$G6_residencyBin), pptsResidency)
incluOthers <- c(sum(sDB_cohort$G7_incluBin), pptsIncluOthers)

incluCrit <- as.data.frame(rbind(health, morpho, compliant, consent,
                                 school, residency, incluOthers))
names(incluCrit) <- c("cohorts", "participants")

print(incluCrit)

write.csv(incluCrit, file="dataCache/inclusionCriteria.csv")

table(sDB_cohort$G7_incluOther)
indexMales <- grep("males only", sDB_cohort$G7_incluOther)
pptsMales <- sum(sDB_cohort$D1_infected[indexMales])


#----------
# Exclusion criteria

sDB_cohort$H1_malnutritionBin <- !is.na(sDB_cohort$H1_excluMalnutrition)
sDB_cohort$H0_illnessBin <- !is.na(sDB_cohort$H0_excluIllness)
sDB_cohort$H4_allergyBin <- !is.na(sDB_cohort$H4_excluAllergy)
sDB_cohort$H5_medicationBin <- !is.na(sDB_cohort$H5_excluMedication)
sDB_cohort$H7_participationBin <- !is.na(sDB_cohort$H7_excluParticipation)
sDB_cohort$H8_excluBin <- !is.na(sDB_cohort$H8_excluOther)

# Corresponding number of participants

pptsMalnutrition <- tapply(sDB_cohort$D1_infected, sDB_cohort$H1_malnutritionBin,
                           sum, na.rm=TRUE)["TRUE"]
pptsIllness <- tapply(sDB_cohort$D1_infected, sDB_cohort$H0_illnessBin,
                      sum, na.rm=TRUE)["TRUE"]
pptsAllergy <- tapply(sDB_cohort$D1_infected, sDB_cohort$H4_allergyBin,
                      sum, na.rm=TRUE)["TRUE"]
pptsMedication <- tapply(sDB_cohort$D1_infected, sDB_cohort$H5_medicationBin,
                         sum, na.rm=TRUE)["TRUE"]
pptsParticipation <- tapply(sDB_cohort$D1_infected, sDB_cohort$H7_participationBin,
                            sum, na.rm=TRUE)["TRUE"]
pptsExcluOthers <- tapply(sDB_cohort$D1_infected, sDB_cohort$H8_excluBin,
                          sum, na.rm=TRUE)["TRUE"]

# Summary table

malnutrition <- c(sum(sDB_cohort$H1_malnutritionBin), pptsMalnutrition)
illness <- c(sum(sDB_cohort$H0_illnessBin), pptsIllness)
allergy <- c(sum(sDB_cohort$H4_allergyBin), pptsAllergy)
medication <- c(sum(sDB_cohort$H5_medicationBin), pptsMedication)
participation <- c(sum(sDB_cohort$H7_participationBin), pptsParticipation)
excluOthers <- c(sum(sDB_cohort$H8_excluBin), pptsExcluOthers)

excluCrit <- as.data.frame(rbind(malnutrition, illness, allergy,
                                 medication, participation, excluOthers))
names(excluCrit) <- c("cohorts", "participants")

print(excluCrit)

write.csv(excluCrit, file="dataCache/exclusionCriteria.csv")

table(sDB_cohort$H8_excluOther)


#----------
# Pregnancy

table(sDB_cohort$F5_pregnancy)

indexPregnant <- grep("include", sDB_cohort$F5_pregnancy)
pptsPregnant <- sum(sDB_cohort$D1_infected[indexPregnant],
                    na.rm=TRUE)
# 452 pregnant women (and potentially more)

indexNotPregnant <- grep("exclude", sDB_cohort$F5_pregnancy)
pptsNotPregnant <- sum(sDB_cohort$D1_infected[indexNotPregnant],
                       na.rm=TRUE)


#----------
# Age range (potential target groups: pre-schoolers)

unknown <- which(sDB_cohort$F0_ageMin <= -1)
temp <- which(sDB_cohort$F0_ageMin < 6)
u <- which(temp %in% unknown)
preSchool <- temp[-u]

length(sDB_cohort$F0_ageMin[preSchool])
# 30 cohorts recruiting participants strictly >6 years old.