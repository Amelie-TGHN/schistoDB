# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to see the number of cohorts, and corresponding
# participants that were diagnosed for infection with Schistosoma spp.
# by performing X times Y egg counts (X: number of lab test repeats,
# Y: number of independent samples)


#----------
# Function giving the number of participants and number of cohorts
# which were diagnosed using 'a' test repeats and 'b' independent samples: 

diag.RepSamp <- function(a, b, species) {
       
        if (species == "h") {
                index <- which(sDB_haema$C1_uRep == a & sDB_haema$C0_uSamp == b)
                s <- sum(sDB_haema$D1_infected[index], na.rm=TRUE)
                c <- length(index)
        }

        if (species == "m") {
                index <- which(sDB_manso$C1_iRep == a & sDB_manso$C0_iSamp == b)
                s <- sum(sDB_manso$D1_infected[index], na.rm=TRUE)
                c <- length(index)
        }

        if (species == "j") {
                index <- which(sDB_japon$C1_iRep == a & sDB_japon$C0_iSamp == b)
                s <- sum(sDB_japon$D1_infected[index], na.rm=TRUE)
                c <- length(index)
        }
        
        c(s, c)
}


table.RepSamp <- function(species) {
        
        if (species == "h") {
                repMax <- max(sDB_haema$C1_uRep, na.rm=TRUE)
                sampMax <- max(sDB_haema$C0_uSamp, na.rm=TRUE)
        }
        
        if (species == "m") {
                repMax <- max(sDB_manso$C1_iRep, na.rm=TRUE)
                sampMax <- max(sDB_manso$C0_iSamp, na.rm=TRUE)
        }
        
        if (species == "j") {
                repMax <- max(sDB_japon$C1_iRep, na.rm=TRUE)
                sampMax <- max(sDB_japon$C0_iSamp, na.rm=TRUE)
        }
        
        u <- c()
        v <- data.frame()
        
        for (i in 1:repMax) {
                for (j in 1:sampMax) {
                        n <- diag.RepSamp(i, j, species)
                        u <- c(i, j, n)
                        v <- rbind(v, u)
                }
        }        
        
        names(v) <- c("repeats", "samples", "participants", "cohorts")
        print(v)
}

table.RepSamp("h")
table.RepSamp("m")
table.RepSamp("j")

hTot <- sum(sDB_haema$D1_infected, na.rm=TRUE)
mTot <- sum(sDB_manso$D1_infected, na.rm=TRUE)
jTot <- sum(sDB_japon$D1_infected, na.rm=TRUE)
