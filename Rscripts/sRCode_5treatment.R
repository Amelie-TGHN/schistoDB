# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enbales to calculate the number of participants initially assigned
# to each treatment group.

#----------
# Consistency test: is the information recorded in the 'total number treated
# and infected at baseline' variable (D1_infected), and in different variables
# for each assignment group (I4_treated1, 2, 3, 4) consistent in terms of the
# number of participants involved?

n <- nrow(sDB_cohort)

# Sum of participants in each cohort, obtained by adding participants from
# each assignment group:
u <- c()
for(i in 1:n) {
        s <- sum(sDB_cohort$I4_treated1[i], sDB_cohort$I4_treated2[i],
                 sDB_cohort$I4_treated3[i], sDB_cohort$I4_treated4[i],
                 na.rm=TRUE)
        u <- c(u, s)  
}
sDB_cohort$I4_sumTreated <- u

# Difference with the sum of participants in each cohort, as entered
# during data extraction:
v <- c()
for(i in 1:n) {
        s <- sum(sDB_cohort$I4_sumTreat[i], -sDB_cohort$D1_infected[i],
                 na.rm=TRUE)
        v <- c(v, s)  
}
sDB_cohort$I5_discrepancy <- v


sDB_cohort$X[which(sDB_cohort$I5_discrepancy != 0)]
sDB_cohort$I5_discrepancy[which(sDB_cohort$I5_discrepancy != 0)]
# Those numbers correspond to initially UNTREATED (and/or UNINFECTED) controls,
# not relevant here and thus not included in the total of participants

totalAssigned <- sum(sDB_cohort$I4_treated1, sDB_cohort$I4_treated2,
                     sDB_cohort$I4_treated3, sDB_cohort$I4_treated4,
                     na.rm=TRUE)
totalTreated <- totalAssigned - sum(sDB_cohort$I5_discrepancy)

totalTreated == sum(sDB_cohort$D1_infected, na.rm=TRUE)
# Expect TRUE if information recorded in the dataset is consistent


#----------
# All regimens and total participants assigned to those, by regimen

# What are the names of all possible broad categories of treatment
# (e.g. "PZQ (60 mg/kg)", wihtout differentiating regimens such as
# "once 60 mg/kg" and "twice 30 mg/kg within a day")?

treatCatNames <- c(levels(sDB_cohort$I0_regCat1),
                   levels(sDB_cohort$I0_regCat2),
                   levels(sDB_cohort$I0_regCat3),
                   levels(sDB_cohort$I0_regCat4))

treatCatNames <- treatCatNames[!duplicated(treatCatNames)]
treatCatNames <- sort(treatCatNames)
u <- which(treatCatNames == "N/A")
treatCatNames <- treatCatNames[-u]


# The 'transform.regCat' function transforms the raw, overall sDB dataset
# into a smaller dataset containing only relevant treatment/regimen information
# and considering all assignment groups independently of their original cohort
# (i.e. each row of the new dataset corresponds to an assignment group).

transform.regCat <- function(sDB_data = sDB_cohort) {
        
        # Which columns contain information about the detailed regimen?
        col1 <- grep("reg1", names(sDB_data))
        col2 <- grep("reg2", names(sDB_data))
        col3 <- grep("reg3", names(sDB_data))
        col4 <- grep("reg4", names(sDB_data))
        
        # Which columns contain information about the number of
        # participants treated and the regimen broad category?
        j1 <- which(names(sDB_data)=="I4_treated1" |
                    names(sDB_data)=="I0_regCat1")
        j2 <- which(names(sDB_data)=="I4_treated2" |
                    names(sDB_data)=="I0_regCat2")
        j3 <- which(names(sDB_data)=="I4_treated3" |
                    names(sDB_data)=="I0_regCat3")
        j4 <- which(names(sDB_data)=="I4_treated4" |
                    names(sDB_data)=="I0_regCat4")
        
        # Subsetting only relevant columns from the master 
        # sDB dataset (sDB_cohort, by default)
        treatGroup1 <- sDB_data[, c(col1, j1)]
        treatGroup2 <- sDB_data[, c(col2, j2)]
        treatGroup3 <- sDB_data[, c(col3, j3)]
        treatGroup4 <- sDB_data[, c(col4, j4)]
        
        # Giving the same names to variables in all sub-datasets,
        # to enable their merging
        names(treatGroup1) <- c("drug1", "drug2", "drug3",
                                "regCat", "treated")
        names(treatGroup2) <- c("drug1", "drug2", "drug3",
                                "regCat", "treated")
        names(treatGroup3) <- c("drug1", "drug2", "drug3",
                                "regCat", "treated")
        names(treatGroup4) <- c("drug1", "drug2", "drug3",
                                "regCat", "treated")
        
        # Finally, merging all 4 possible assignment groups
        # into 1 dataset (easier to manipulate), and sorting rows
        # in alphabetical order of each drug/dose in the regimen of interest
        treatGroup <- rbind(treatGroup1, treatGroup2, treatGroup3, treatGroup4)
        treatGroup <- treatGroup[order(treatGroup$regCat) ,]
        treatGroup <- treatGroup[order(treatGroup$drug3) ,]
        treatGroup <- treatGroup[order(treatGroup$drug2) ,]
        treatGroup <- treatGroup[order(treatGroup$drug1) ,]
        
        # Erasing empty assignment groups (superfluous columns in the original
        # dataset)...
        treatGroup <- treatGroup[which(treatGroup$regCat != "N/A") ,]
        
        n <- nrow(treatGroup)
        row.names(treatGroup) <- seq(1:n)
        treatGroup        
}

treatGroup <- transform.regCat()
sum(treatGroup$treated, na.rm=TRUE) == totalAssigned
# Expect TRUE


# The 'regimen.group' function takes the output of 'transform.regCat' and groups it,
# summing the number of participants who all received the exact same regimen
# (i.e. looking more specifically than by broad regimen category).

regimen.group <- function (treatGroup, rm.superfluous = TRUE) {
        
        if(rm.superfluous == TRUE) {
                superfluous <- c("(ALB)", "ALB", "ALB+iron",
                                 "(anti-malarials)",
                                 "(placebo)", "placebo")
                treatGroup$drug2[treatGroup$drug2 %in% superfluous] <- "N/A"
                treatGroup$drug3[treatGroup$drug3 %in% superfluous] <- "N/A" 
        }
        
        treatGrp <- aggregate(treatGroup$treated, FUN=sum, na.rm=TRUE,
                              by = list(drug1=treatGroup$drug1,
                                        drug2=treatGroup$drug2,
                                        drug3=treatGroup$drug3,
                                        regCat=treatGroup$regCat))
        
        treatGrp <- treatGrp[order(treatGrp$drug3) ,]
        treatGrp <- treatGrp[order(treatGrp$drug2) ,]
        treatGrp <- treatGrp[order(treatGrp$drug1) ,]
        treatGrp <- treatGrp[order(treatGrp$regCat) ,]
        
        treatGrp
}

treatGrp <- regimen.group(treatGroup)
sum(treatGrp$x, na.rm=TRUE) == totalAssigned
# Expect TRUE

totalAssigned-totalTreated # Number of participants who were either uninfected
# and/or did not receive treatment at baseline

write.csv(treatGrp, file="dataCache/treatGrp.csv")