# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to look at the time-points at which outcome was
# assessed for participants included and treated as part of the studies.


#----------
# Time of outcome assessment

# By cohort, in increasing order of first assessment
sDB_order <- sDB_cohort[order(sDB_cohort$D1_T3) ,]
sDB_order <- sDB_order[order(sDB_order$D1_T2) ,]
sDB_order <- sDB_order[order(sDB_order$D1_T1) ,]
sDB_cohort$D1_T1

summary(sDB_order$D1_T1, na.rm=TRUE)
summary(sDB_order$D1_T2, na.rm=TRUE)
summary(sDB_order$D1_T3, na.rm=TRUE)
summary(sDB_order$D1_Tlast, na.rm=TRUE)


# Graphical representation of outcome assessment time points

draw.timepoints <- function(schistoDB, x,
                            y = 60L, line = TRUE) {
        
        plot(0, 0, xlim = c(1, x), ylim = c(0, y),
             xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n")
        # initiates empty plotting area
        
        if(line == TRUE) {
                abline(h = 21, col = "lightgrey", lwd = 10)
                abline(h = 29, col = "lightgrey", lwd = 25)
                abline(h = 42, col = "lightgrey", lwd = 10)
        }
        
        par(new = TRUE)
        plot(schistoDB$D1_T1, xlim = c(1, x), ylim = c(0, y),
             xlab = "cohort index i",
             ylab = "time post-treatment (in days)",
             col = "firebrick4", type = "p", pch = 20)
        
        par(new = TRUE)
        plot(schistoDB$D1_T2, xlim = c(1, x), ylim = c(0, y),
             xaxt = "n", yaxt = "n", xlab = "", ylab = "",
             col = "firebrick3", type = "p", pch = 20)
        
        par(new = TRUE)
        plot(schistoDB$D1_T3, xlim = c(1, x), ylim = c(0, y),
             xaxt = "n", yaxt = "n", xlab = "", ylab = "",
             col = "firebrick2", type = "p", pch = 20)
        
        par(new = TRUE)
        plot(schistoDB$D1_Tlast, xlim = c(1, x), ylim = c(0, y),
             xaxt = "n", yaxt = "n", xlab = "", ylab = "",
             col = "firebrick1", type = "p", pch = 20)
}

# High-resolution image:

setwd("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache")

tiff("Fig 10.tif", width = 6, height = 4.5, units = "in",
     compression = "lzw", res = 300)
        
        draw.timepoints(sDB_order, x=105, y=59)
        par(new = TRUE)
        abline(v=8, lty=2, col = "red", lwd=1.65)
        # all cohorts, zooming on first 60 days
        # (y=59, otherwise the scale extends to 60+)

dev.off()


#----------
# Are there differences in time of outcome assessment, depending on species?

sDB_haema <- sDB_haema[order(sDB_haema$D1_T1) ,]
draw.timepoints(sDB_haema, x=50, y=59)

sDB_manso <- sDB_manso[order(sDB_manso$D1_T1) ,]
draw.timepoints(sDB_manso, x=65, y=59)

sDB_japon <- sDB_japon[order(sDB_japon$D1_T1) ,]
draw.timepoints(sDB_japon, x=12, y=59)


# Satistical test (X2)

# Preparing the distribution table (weekly distribution)

week.distrib <- function(a, b) {
        wk <- seq(from=a, to=b, by=0.5)
        
        Hwk <- length(which(sDB_haema$D1_T1 %in% wk)) +
               length(which(sDB_haema$D1_T2 %in% wk)) +
               length(which(sDB_haema$D1_T3 %in% wk)) +
               length(which(sDB_haema$D1_Tlast %in% wk))
        Mwk <- length(which(sDB_manso$D1_T1 %in% wk)) +
               length(which(sDB_manso$D1_T2 %in% wk)) +
               length(which(sDB_manso$D1_T3 %in% wk)) +
               length(which(sDB_haema$D1_Tlast %in% wk))
        Jwk <- length(which(sDB_japon$D1_T1 %in% wk)) +
               length(which(sDB_japon$D1_T2 %in% wk)) +
               length(which(sDB_japon$D1_T3 %in% wk)) +
               length(which(sDB_haema$D1_Tlast %in% wk))
        
        u <- c(Hwk, Mwk, Jwk)
        names(u) <- c("haematobium", "mansoni", "japonicum")
        u
}

u1 <- week.distrib(0.5, 7)
u2 <- week.distrib(7.5, 14)
u3 <- week.distrib(14.5, 21)
u4 <- week.distrib(21.5, 28)
u5 <- week.distrib(28.5, 35)
u6 <- week.distrib(35.5, 42)
u7 <- week.distrib(42.5, 49)
u8 <- week.distrib(49.5, 56)
u9 <- week.distrib(56.5, 63)

species.time <- as.data.frame(rbind(u1, u2, u3, u4, u5, u6, u7, u8, u9))

chisq.test(species.time) # few observations in each category
# thus X2 might be incorrect

fisher.test(species.time, workspace=40000000) # memory-intensive command!
# 4.10^7 seems to be the minimal workspace size which enables the calculation.
# Fischer's Exact Test p-value = 0.01835 < 0.05 (all 3 distributions considered)
# thus possible variations by species

fisher.test(species.time[, 1:2], workspace=20000000)
# Fischer's Exact Test p-value = 0.04697 < 0.05 (comparing only Sh and Sm,
# wich have more values), thus possible variations by species

chisq.test(species.time[, 1:2], simulate.p.value=TRUE, B=2000)


#----------
# Other observations on time of outcome assessment
# (including, outside of the 60-day scope)

T1 <- sDB_order$D1_T1
T2 <- sDB_order$D1_T2
T3 <- sDB_order$D1_T3
T4 <- sDB_order$D1_Tlast

# Latest (overall) follow-up?
max(c(T1, T2, T3, T4), na.rm=TRUE)/365


# Cohorts performing an assessmnet at 3 to 4 weeks
seq3to4Wks <- seq(21, 30, by=0.5)

indexT1 <- which(T1 %in% seq3to4Wks)
indexT2 <- which(T2 %in% seq3to4Wks)
indexT3 <- which(T3 %in% seq3to4Wks)
indexT4 <- which(T4 %in% seq3to4Wks)

index3to4wks <- c(indexT1, indexT2, indexT3, indexT4)
index3to4wks <- index3to4wks[!duplicated(index3to4wks)]

f <- length(index3to4wks)

# Corresponding percentage
n <- nrow(sDB_cohort)
f/n*100


# Participants for which outcome was assessed at 3 to 4 weeks
participants.3to4wks <- sDB_order$D1_infected[index3to4wks]
pptsTotal.3to4wks <- sum(participants.3to4wks, na.rm=TRUE)
pptsTotal.3to4wks/sum(sDB_cohort$D1_infected, na.rm=TRUE)*100


# Among those cohorts, number of cohorts which also have
# late (> 60 days) assessment time-points

sDB_3to4wks <- sDB_order[sort(index3to4wks), c("D1_T1", "D1_T2", 
                                               "D1_T3", "D1_Tlast")]

indexTemp1 <- which(sDB_3to4wks$D1_T1 > 60)
indexTemp2 <- which(sDB_3to4wks$D1_T2 > 60)
indexTemp3 <- which(sDB_3to4wks$D1_T3 > 60)
indexTemp4 <- which(sDB_3to4wks$D1_Tlast > 60)

index60sup <- c(indexTemp1, indexTemp2, indexTemp3, indexTemp4)
index60sup <- index60sup[!duplicated(index60sup)]

f60 <- length(index60sup)