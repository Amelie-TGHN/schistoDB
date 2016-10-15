# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to look at the 'geographical' characteristics of cohorts
# i.e. in which country they were rectruited, which income group the country
# falls in, what urban/rural setting and type of site (school, health centre...)
# was used for recruiting participants, which species were endemic.


#----------
# Number of cohorts and participants recruited by country

countryCohorts <- table(sDB_cohort$B0_country)

sum(countryCohorts)
# Expect 104 if all cohorts have been assigned a country of recruitment

print(countryCohorts)

length(countryCohorts) # Number of different countries involved
# To be compared with number of endemic countries (approx. 78)


countryPpts <- tapply(sDB_cohort$D1_infected, sDB_cohort$B0_country,
                      sum, na.rm=TRUE)
sum(countryPpts, na.rm=TRUE) # Expect total number of IPD = 20517

print(countryPpts)

write.csv(countryPpts, file = "dataCache/countryPpts.csv", na="0")


#----------
# Table of cohort distribution by world region

# Install/Open the library to create Pivot tables with R
install.packages('reshape')
library(reshape)

# Extract the variables to be 'pivoted' and create a 'count' variable
# (each line = 1 cohort should be counted once)
worldReg <- sDB_cohort[, c("B0_country", "B0_region")]
n <- nrow(worldReg)
worldReg$B0_count <-  rep(1, n)
head(worldReg)

# Pivot table of countries by world region:
pivotWorld <- cast(worldReg, B0_country ~ B0_region,
                   value="B0_count")
head(pivotWorld)

colSums(pivotWorld)

write.csv(pivotWorld, file = "dataCache/worldRegion.csv", na="0")


#----------
# Number of cohorts per World Bank income group of country
incomeCohorts <- table(sDB_cohort$B0_income)
print(incomeCohorts)

# Number of cohorts per rural/urban setting
settingCohorts <- table(sDB_cohort$B1_setting)
print(settingCohorts)

# Number of cohorts per type of community of recruitment
communityCohorts <- table(sDB_cohort$B1_siteCat)
print(communityCohorts)


#----------
# Number of cohorts per endemic context

endemicityCohort <- table(sDB_cohort$B2_endemSpecies)
print(endemicityCohort)

endemLevelCohort <- table(sDB_cohort$B2_endem)
print(endemLevelCohort)


# Prevalence of infection at recruitment sites 

# Merging levels "0" and "0*", and "8" and "8*": 

sDB_cohort$B2_endemLevelsNum <- sDB_cohort$B2_endem

levels(sDB_cohort$B2_endemLevelsNum) <- list(
        "0"=c("0", "0*"), "1"=c("1"), "2"=c("2"),
        "3"=c("3"), "4"=c("4"), "5"=c("5"), "6"=c("6"),
        "7"=c("7"), "8"=c("8", "8*"), "9"=c("9")
        )

table(sDB_cohort$B2_endemLevelsNum)

# Transforming the factor variable into a numeric variable of prevalence (in %)
# to enable graphical representation (histogram):

sDB_cohort$B2_endemLevelsNum <- as.numeric(sDB_cohort$B2_endemLevelsNum)*10

hist(sDB_cohort$B2_endemLevelsNum, breaks=seq(from=0, to=100, by=10),
     main="Infection prevalence at cohort recruitment site(s)",
     xlab="Maximum prevalence among the site(s) of recruitment for the cohort (in %)",
     ylab="Number of cohorts (n=104)",
     col="slategrey", border="white")