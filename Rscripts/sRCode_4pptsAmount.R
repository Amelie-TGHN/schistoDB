# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enbales to estimate the size of each cohort, and the number
# of participants lost at follow-up.


#----------
# Trial size

n <- nrow(sDB_cohort)
m <- nrow(sDB_study)

# By cohort and by study:
cohortSize <- sDB_cohort$D1_infected
studySize <- as.vector(tapply(sDB_cohort$D1_infected, sDB_cohort$A0_S,
                              sum, na.rm=TRUE))   

# Percentage of 'small' trials (<200 participants)
cohortSmall <- cohortSize<200
sum(cohortSmall, na.rm=TRUE)/n*100
studySmall <- studySize<200
sum(studySmall, na.rm=TRUE)/m*100

summary(cohortSize)
summary(studySize)


# Graphs:

par(mfrow=c(1, 1))

hist(cohortSize, main="cohort size",
     xlim=c(0, 1200), xlab=NULL,
     ylim=c(0, 50), ylab = "number of cohorts",
     col="slategrey", border="white")

boxplot(cohortSize,
        horizontal=TRUE,
        col=c("slategrey"), border="black")


# High-resolution image:

setwd("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache")

tiff("Fig 6.tif", width = 5, height = 4, units = "in",
      compression = "lzw", res = 300)

        par(fig = c(0, 1, 0.1, 1),
            mar = c(5, 2.1, 0.1, 0.1) + 0.1)
        
        hist(cohortSize, main=NULL,
             xlim=c(0, 1200), xlab=NULL,
             ylim=c(0, 50), ylab=NULL,
             col="slategrey", border="white")
        
        par(fig = c(0, 1, 0, 0.3),
            mar = c(0, 2.1, 0, 0.1) + 0.1, new=TRUE)
        
        boxplot(cohortSize, axes = FALSE,
                horizontal=TRUE, ylim=c(0, 1200),
                col=c("slategrey"), border="black")

dev.off()


#----------
# Follow-up

sDB_cohort$D2_infectLowEstimate <- sDB_cohort$D2_followed

# D2_followed coded as '-1' means that the cohort was defined a posteriori,
# based on compliance to treatment, and thus that all included participants
# have by definition been followed up (i.e. D2_followed = D1_infected)

index1 <- which(sDB_cohort$D2_followed == -1)
values1 <- sDB_cohort$D1_infected[index1]
sDB_cohort$D2_infectLowEstimate <- replace(sDB_cohort$D2_infectLowEstimate,
                                           index1, values1)

# Approximation: imagining half of the participants got lost...
indexNA <- which(is.na(sDB_cohort$D2_followed))
valuesNA <- sDB_cohort$D1_infected[indexNA]*0.5
sDB_cohort$D2_infectLowEstimate <- replace(sDB_cohort$D2_infectLowEstimate,
                                           indexNA, valuesNA)

sum(sDB_cohort$D2_infectLowEstimate, na.rm=TRUE)
summary(sDB_cohort$D2_infectLowEstimate)
# Approximately 17501 participants have been followed-up.