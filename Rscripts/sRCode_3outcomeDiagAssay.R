# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to see the technique that was used to process stools
# for the diagnosis of intestinal schistosomiasis, by species.
# Also enables to explore other tests performed as part of the study.


#----------
# Type of method used to process stool samples, for diagnosis (egg count)
# of intestinal schistosomiasis

# Re-grouping levels of 'intestinal schistosomiasis diagnostic technique'
# (iTech) factor:
namesTechnique <- c(as.character(sDB_cohort$C3_iTech1),
                    as.character(sDB_cohort$C3_iTech2))
namesTechnique <- namesTechnique[!duplicated(namesTechnique)]
namesTechnique <- sort(namesTechnique)


# level 0: other/unknown

hatching <- "hatching test"
concentration <- c("concentration", "concentration (formaldehyde)")
other <- "other"
unknown <- "unknown"

otherTech <- c(hatching, concentration, other, unknown)


# level 1: Kato-Katz

KK_unspecified <- c("Kato-Katz method", "Kato-Katz method + Kato thick smears",
                    "Kato-Katz thick smears")
KK_modified <- "Kato-Katz smears (modified)"
KK_40 <- c("Kato-Katz thick smears (41.7 mg)", "Kato-Katz thick smears (42 mg)",
           "Kato-Katz thick smears (42.7 mg)")
KK_50 <- "Kato-Katz thick smears (50 mg)"

KKtech <- c(KK_unspecified, KK_modified, KK_40, KK_50)


# level 2: Kato

Kato_unspecified <- c("Kato technique", "Kato thick smears",
                      "Kato technique (qualitative)")
Kato_25 <- "Kato thick smears (25 mg)"
Kato_40 <- c("Kato slides (41.7 mg)", "Kato thick smears (43.7 mg)")
Kato_50 <- "Kato thick smears (50 mg)"

KatoTech <- c(Kato_unspecified, Kato_25, Kato_40, Kato_50)


# Level 3: Katz

Katz_unspecified <- "Katz thick smears"


#----------
# Creating the variable containing the grouped levels, for S. mansoni: 

sDB_manso$C3_iCat1 <- sDB_manso$C3_iTech1
levels(sDB_manso$C3_iCat1) <- list(
        "KK (41.7, 42 or 42.7 mg)" = KK_40,
        "KK (50 mg)" = KK_50,
        "KK (modified)" = KK_modified,
        "KK (unspecified)" = KK_unspecified,
        "Kato (25 mg)" = Kato_25,
        "Kato (41.7 or 43.7 mg)" = Kato_40,
        "Kato (50 mg)" = Kato_50,
        "Kato (unspecified)" = Kato_unspecified,
        "Katz (unspecified)" = Katz_unspecified,
        "hatching test" = hatching,
        "concentration" = concentration,
        "other"= other,
        "unknown" = unknown)

sDB_manso$C3_iCat2 <- sDB_manso$C3_iTech2
levels(sDB_manso$C3_iCat2) <- list(
        "KK (41.7, 42 or 42.7 mg)" = KK_40,
        "KK (50 mg)" = KK_50,
        "KK (modified)" = KK_modified,
        "KK (unspecified)" = KK_unspecified,
        "Kato (25 mg)" = Kato_25,
        "Kato (41.7 or 43.7 mg)" = Kato_40,
        "Kato (50 mg)" = Kato_50,
        "Kato (unspecified)" = Kato_unspecified,
        "Katz (unspecified)" = Katz_unspecified,
        "hatching test" = hatching,
        "concentration" = concentration,
        "other"=other,
        "unknown" = unknown)

# Remark: When a Kato method is crossed with a KK method, the KK prevails
# and the second method is not taken into account (otherwise, it would be
# necessary to consider 3 different techniques because of only few cases...)

table(sDB_manso$C3_iCat1)
table(sDB_manso$C3_iCat2)

mansoPrimTech <- tapply(sDB_manso$D1_infected, sDB_manso$C3_iCat1,
                        sum, na.rm=TRUE)
mansoSecondTech <- tapply(sDB_manso$D1_infected, sDB_manso$C3_iCat2,
                          sum, na.rm=TRUE)

sum(mansoPrimTech, na.rm=TRUE) == sum(sDB_manso$D1_infected, na.rm=TRUE)
# expect TRUE


#----------
# Creating the variable containing the grouped levels, for S. japonicum: 

sDB_japon$C3_iCat1 <- sDB_japon$C3_iTech1
levels(sDB_japon$C3_iCat1) <- list(
        "KK (41.7, 42 or 42.7 mg)" = KK_40,
        "KK (50 mg)" = KK_50,
        "KK (modified)" = KK_modified,
        "KK (unspecified)" = KK_unspecified,
        "Kato (25 mg)" = Kato_25,
        "Kato (41.7 or 43.7 mg)" = Kato_40,
        "Kato (50 mg)" = Kato_50,
        "Kato (unspecified)" = Kato_unspecified,
        "Katz (unspecified)" = Katz_unspecified,
        "hatching test" = hatching,
        "concentration" = concentration,
        "other"=other,
        "unknown" = unknown)

sDB_japon$C3_iCat2 <- sDB_japon$C3_iTech2
levels(sDB_japon$C3_iCat2) <- list(
        "KK (41.7, 42 or 42.7 mg)" = KK_40,
        "KK (50 mg)" = KK_50,
        "KK (modified)" = KK_modified,
        "KK (unspecified)" = KK_unspecified,
        "Kato (25 mg)" = Kato_25,
        "Kato (41.7 or 43.7 mg)" = Kato_40,
        "Kato (50 mg)" = Kato_50,
        "Kato (unspecified)" = Kato_unspecified,
        "Katz (unspecified)" = Katz_unspecified,
        "hatching test" = hatching,
        "concentration" = concentration,
        "other"=other,
        "unknown" = unknown)

# Remark: When a Kato method is crossed with a KK method, the KK prevails
# and the second method is not taken into account (otherwise, it would be
# necessary to consider 3 different techniques because of only few cases...)

table(sDB_japon$C3_iCat1)
table(sDB_japon$C3_iCat2)

japonPrimTech <- tapply(sDB_japon$D1_infected, sDB_japon$C3_iCat1,
                        sum, na.rm=TRUE)
japonSecondTech <- tapply(sDB_japon$D1_infected, sDB_japon$C3_iCat2,
                          sum, na.rm=TRUE)

sum(japonPrimTech, na.rm=TRUE) == sum(sDB_japon$D1_infected, na.rm=TRUE)
# expect TRUE


#----------
# Overall summary table

intestinalPrimTech <- as.data.frame(cbind(mansoPrimTech, japonPrimTech))

write.csv(intestinalPrimTech,
          file="dataCache/intestinalPrimTech.csv")


#----------
# Exploring other tests that were also performed:

table(sDB_cohort$C4_testClinic)
table(sDB_cohort$C4_testLab)
table(sDB_cohort$C5_otherAssays)