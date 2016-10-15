# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# This R code enables to download all 5 (+ 0) schisto CSV tables (from Excel)
# into the R working environment, and to merge them into 1 workable dataset.

# The main output datasets are schistoDB (all 104 cohorts, all variables),
# sDB_cohort (all 104 cohorts and 'quantitative' variables, no 'text' variable),
# and sDB_study (all 90 studies but only 1 cohort per article,
# all 'quantitative' variables, no 'text' variable)

# All analyses were done in R version 3.1.3
# Preparing the working environment
closeAllConnections()
rm(list=ls())
setwd("C:/Users/Amélie JULE/Documents/Rwd_Current")


#----------
# Opening dataset "0.SearchResults" and checking appropriate downloading
sDB0 <- read.csv("sDB_0search.csv",
                 skip = 1, na.strings = c(" ", ""))
head(sDB0, n=2)

summary(sDB0)
nrow(sDB0) # expect 914 rows = unique references screened
names(sDB0) # expect 15 variables
# Check that no superfluous, empty column/row has been appended upon transfer


#----------
# Opening dataset "1.Design" and checking appropriate downloading
sDB1_design <- read.csv("sDB_1design.csv",
                        skip = 1, na.strings = c(" ", "", "N/A"))
head(sDB1_design, n=2)
names(sDB1_design) # 21 variables

sDB1_design <- sDB1_design[complete.cases(sDB1_design$A0_S), 1:21]
summary(sDB1_design)
nrow(sDB1_design) # expect 104 rows = cohorts


#----------
# Opening dataset "2.Setting" and checking appropriate downloading
sDB2_setting <- read.csv("sDB_2setting.csv",
                         skip = 1, na.strings = c(" ", "", "N/A",
						  "-9", "unknown"))
head(sDB2_setting, n=2)
names(sDB2_setting) # 16 variables

sDB2_setting <- sDB2_setting[complete.cases(sDB2_setting$A0_S), 1:16]
summary(sDB2_setting)
nrow(sDB2_setting) # expect 104 rows = cohorts


# Creating the 'endemic species' variable

# Searching the indexes of rows corresponding to each endemic setting:

h1 <- which(sDB2_setting$B2_species1 == "haematobium" &
            is.na(sDB2_setting$B2_species2))
h2 <- which(sDB2_setting$B2_species1 == "haematobium" &
            sDB2_setting$B2_species2 == "mansoni [absent]")
h3 <- which(sDB2_setting$B2_species1 == "haematobium" &
            sDB2_setting$B2_species2 == "mansoni [rare]")
h4 <- which(sDB2_setting$B2_species1 == "haematobium" &
            sDB2_setting$B2_species2 == "mansoni [partial data, rare]")
indexH <- sort(c(h1, h2, h3, h4))

m1 <- which(sDB2_setting$B2_species1 == "mansoni" &
            is.na(sDB2_setting$B2_species2))
m2 <- which(sDB2_setting$B2_species1 == "mansoni" &
            sDB2_setting$B2_species2 == "haematobium [absent]")
m3 <- which(sDB2_setting$B2_species1 == "mansoni" &
            sDB2_setting$B2_species2 == "haematobium [rare]")
m4 <- which(sDB2_setting$B2_species1 == "mansoni" &
            sDB2_setting$B2_species2 == "haematobium [partial data, rare]")
indexM <- sort(c(m1, m2, m3, m4))

mixed1 <- which(sDB2_setting$B2_species1 == "haematobium" &
                sDB2_setting$B2_species2 == "mansoni")
mixed2 <- which(sDB2_setting$B2_species1 == "haematobium" &
                sDB2_setting$B2_species2 == "mansoni [partial data]")
mixed3 <- which(sDB2_setting$B2_species1 == "mansoni" &
                sDB2_setting$B2_species2 == "haematobium")
mixed4 <- which(sDB2_setting$B2_species1 == "mansoni" &
                sDB2_setting$B2_species2 == "haematobium [partial data]")
indexMix <- sort(c(mixed1, mixed2, mixed3, mixed4))

indexJ <- which(sDB2_setting$B2_species1 == "japonicum")


sDB2_setting$B2_endemSpecies <- "endemicity"
sDB2_setting$B2_endemSpecies[indexH] <- "S. haematobium"
sDB2_setting$B2_endemSpecies[indexM] <- "S. mansoni"
sDB2_setting$B2_endemSpecies[indexMix] <- "mixed"
sDB2_setting$B2_endemSpecies[indexJ] <- 'S. japonicum'


#----------
# Opening dataset "3.Outcomes" and checking appropriate downloading
sDB3_outco <- read.csv("sDB_3outcomes.csv",
                       skip = 1, na.strings = c(" ", "", "N/A",
                                                "none", "not found"))
head(sDB3_outco, n=2)
names(sDB3_outco) # 23 variables

sDB3_outco <- sDB3_outco[complete.cases(sDB3_outco$A0_S), 1:23]
summary(sDB3_outco)
nrow(sDB3_outco) # expect 104 rows = cohorts


#----------
# Opening dataset "4.Participants" and checking appropriate downloading
sDB4_ppts <- read.csv("sDB_4participants.csv",
                      skip = 1, na.strings = c(" ", "", "N/A",
                                               "-9", "none"))
head(sDB4_ppts, n=2)
names(sDB4_ppts) # 30 variables

sDB4_ppts <- sDB4_ppts[complete.cases(sDB4_ppts$A0_S), 1:30]
summary(sDB4_ppts)
nrow(sDB4_ppts) # expect 104 rows = cohorts


#----------
# Opening dataset "5.Treatment" and checking appropriate downloading
sDB5_treat <- read.csv("sDB_5treatment.csv",
                       skip = 1, na.strings = c(" ", "",
                                                "-9", "-1"))
head(sDB5_treat, n=2)
names(sDB5_treat) # 27 variables

sDB5_treat <- sDB5_treat[complete.cases(sDB5_treat$A0_S), 1:27]
summary(sDB5_treat)
nrow(sDB5_treat) # expect 104 rows = cohorts


#----------
# Merging the separate CSV tables into one single R dataset

schistoDB <- merge(sDB1_design, sDB2_setting, by=c("X", "A0_S", "A0_C"))
schistoDB <- merge(schistoDB, sDB3_outco, by=c("X", "A0_S", "A0_C"))
schistoDB <- merge(schistoDB, sDB4_ppts, by=c("X", "A0_S", "A0_C"))
schistoDB <- merge(schistoDB, sDB5_treat, by=c("X", "A0_S", "A0_C"))

names(schistoDB)

#----------
# Deleting the X.comments variables, irrelevant for quantitative analyses

varList <- sort(names(schistoDB)) # Variables' list, in alphabetical order
commentVar <- varList[94:106]
length(commentVar) # 13 variables recording free text i.e. comments
u <- which(names(schistoDB) %in% commentVar)

sDB_cohort <- schistoDB[, -u]
names(sDB_cohort)


# Creating the dataset for analyses 'by study' (instead of 'by cohort')
sDB_study <- schistoDB[!duplicated(schistoDB$A0_S), -u]
sort(sDB_study$A0_S)


#----------
# Creating the datasets for analyses by species of study (regardless
# of endemic setting)

sDB_h1 <- schistoDB[schistoDB$B2_species1 == "haematobium" , -u]
sDB_hTemp <- schistoDB[complete.cases(schistoDB$B2_species2) , -u]
sDB_h2 <- sDB_hTemp[sDB_hTemp$B2_species2 == "haematobium" ,]
sDB_haema <- rbind(sDB_h1, sDB_h2)

sDB_m1 <- schistoDB[schistoDB$B2_species1 == "mansoni" , -u]
sDB_mTemp <- schistoDB[complete.cases(schistoDB$B2_species2) , -u]
sDB_m2 <- sDB_mTemp[sDB_mTemp$B2_species2 == "mansoni" ,]
sDB_manso <- rbind(sDB_m1, sDB_m2)

sDB_japon <- schistoDB[schistoDB$B2_species1 == "japonicum" , -u]


#----------
# Saving datasets that are necessary for R analysis

save(list=c("schistoDB", "sDB_cohort", "sDB_study",
            "sDB_haema", "sDB_manso", "sDB_japon"),
     file="C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.RData")