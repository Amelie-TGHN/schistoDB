# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to characterise the type of Schistosoma infection
# that was required for inclusion in the study at baseline (including,
# whether acute or asymptomatic forms were preferred).


#----------
# Type of schistosomiasis disease considered

# Considering the cases of single and mixed endemicity separately:
table(sDB_cohort$B2_endemSpecies)

sDB_mixed <- sDB_cohort[sDB_cohort$B2_endemSpecies == "mixed" ,]
sDB_single <- sDB_cohort[sDB_cohort$B2_endemSpecies != "mixed" ,]
        

#----------
# Single endemicity

# Checking consistency of datasets:

t <- table(sDB_single$E0_infection)
t[t>0] # unexpected to see some cases of mixed species in inclusion criteria
sum(t)

i <- which(sDB_single$E0_infection == "not required; Sm+ (excluded)")
sDB_single$B2_species2[i] # OK: the species excluded was confirmed absent

j <- which(sDB_single$E0_infection == "unclear")
sDB_single$B2_species2[j] # OK: one species, but not clear if infection
                           # was required or not at baseline


#----------
# Creating the categories of possible infection(s) of interest...
# Single endemicity context

single_0 <- c("not required",
              "not required; Sh+ (excluded)", "not required; Sm+ (excluded)")
singleIndex_0 <- which(sDB_single$E0_infection %in% single_0)

singleH <- c("Sh+", "Sh+ (except control group)",
             "Sh+ (required) and Sm+ (excluded)")
singleJ <- c("Sj+")
singleM <- c("Sm+", "Sm+ (except control group)",
             "Sm+ (required) and Sh+ (excluded)")
single_1 <- c(singleH, singleJ, singleM)
singleIndex_1 <- which(sDB_single$E0_infection %in% single_1)

single_9 <- c("unclear")
singleIndex_9 <- which(sDB_single$E0_infection %in% single_9)


#----------
# Storing the created categories in a new factor variable:

sDB_single$E0_infectInterest <- as.character(sDB_single$E0_infection)

sDB_single$E0_infectInterest <- replace(sDB_single$E0_infectInterest,
                                        singleIndex_0, "others")
sDB_single$E0_infectInterest <- replace(sDB_single$E0_infectInterest,
                                        singleIndex_1, "1 Schistosoma species")
sDB_single$E0_infectInterest <- replace(sDB_single$E0_infectInterest,
                                        singleIndex_9, "unclear")

sDB_single$E0_infectInterest <- as.factor(sDB_single$E0_infectInterest)

table(sDB_single$E0_infectInterest)


#----------
# Mixed endemicity

t <- table(sDB_mixed$E0_infection)
t[t>0]


#----------
# Creating the categories of possible infection(s) of interest...

# Other (non-Schistosoma) infections considered in the study,
# and infections with 1 or several Schistosoma species allowed:

mixed_0b <- c("not required")
mixed_0c <- c("not required; Sh+ (excluded)", "not required; Sm+ (excluded)")

mixedIndex_0b <- which(sDB_mixed$E0_infection %in% mixed_0b)
mixedIndex_0c <- which(sDB_mixed$E0_infection %in% mixed_0c)


# 1 Schistosoma species only considered in the study,
# and infections with the other species allowed or not:

mixed_1a <- c("Sh+ (required) and Sm+ (tolerated)",
              "Sm+ (required) and Sh+ (tolerated)")
mixed_1b <- c("Sh+", "Sh+ (except control group)",
              "Sm+", "Sm+ (except control group)")
mixed_1c <- c("Sm+ (required) and Sh+ (excluded)",
              "Sh+ (required) and Sm+ (excluded)")

mixedIndex_1a <- which(sDB_mixed$E0_infection %in% mixed_1a)
mixedIndex_1b <- which(sDB_mixed$E0_infection %in% mixed_1b)
mixedIndex_1c <- which(sDB_mixed$E0_infection %in% mixed_1c)


# 2 Schistosoma species considered in the study,
# and concurrent infections with the other species allowed or not:

mixed_2a <- c("Sh+ and/or Sm+", "Sm+ and/or Sh+")

mixedIndex_2a <- which(sDB_mixed$E0_infection %in% mixed_2a)


#----------
# Storing the created categories in a new factor variable:

sDB_mixed$E0_infectInterest <- as.character(sDB_mixed$E0_infection)


sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_0b,
				       "others, S. co-infections excluded?")
sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_0c,
				       "others, S. co-infections excluded")

sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_1a,
				       "1 Schistosoma species, S. co-infections tolerated")
sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_1b,
				       "1 Schistosoma species, S. co-infections excluded?")
sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_1c,
				       "1 Schistosoma species, S. co-infections excluded")

sDB_mixed$E0_infectInterest <- replace(sDB_mixed$E0_infectInterest,
                                       mixedIndex_2a,
				       "2 Schistosoma species, S. co-infections tolerated")

#----------
# Re-merging sDB_single and sDB_mixed

sDB_endem <- rbind(sDB_single, sDB_mixed)
sDB_endem <- sDB_endem[order(sDB_endem$X) ,]

sDB_endem$A0_S == sDB_cohort$A0_S # expect TRUE

namesInfect <- names(table(sDB_endem$E0_infectInterest))
cohortInfect <- as.vector(table(sDB_endem$E0_infectInterest))
pptsInfect <- as.vector(tapply(sDB_endem$D1_infected, sDB_endem$E0_infectInterest,
                               sum, na.rm=TRUE))

summaryInfect <- as.data.frame(cbind(cohortInfect, pptsInfect))
row.names(summaryInfect) <- namesInfect
summaryInfect[order(row.names(summaryInfect)) ,]


#----------
# Type of schistosomiasis (2): symptoms and def. of cured/infected

tIncl <- table(sDB_cohort$E1_schistoInclude)
tExcl <- table(sDB_cohort$E1_schistoExclude)

sum(tExcl)