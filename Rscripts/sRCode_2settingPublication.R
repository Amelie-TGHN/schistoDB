# 2016-05-17
# sDB_final analyses (PLoS NTDs)

# Run sRCode_0download.R before running this code, or load data:
closeAllConnections()
rm(list=ls())
load("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache/schistoDB.Rdata")

# This R code enables to calculate and graphically represent the time elapsed
# between the end of the study (data collection) and the publication or results.


#----------
# Time at which studies happened and were subsequently reported

# Formatting dates to R context

format.dates <- function(schisto) {
        rDates9 <- as.Date(schisto$B5_reportDate, format="%d/%m/%Y")
        correctDates9 <- format(rDates9, "%Y-%m-%d")
        schisto$B5_rDate9 <- as.Date(correctDates9)

        rDates0 <- as.Date(schisto$B3_studyStart, format="%d/%m/%Y")
        correctDates0 <- format(rDates0, "%Y-%m-%d")
        schisto$B3_rDate0 <- as.Date(correctDates0)

        rDates1 <- as.Date(schisto$B4_studyEnd, format="%d/%m/%Y")
        correctDates1 <- format(rDates1, "%Y-%m-%d")
        schisto$B4_rDate1 <- as.Date(correctDates1)
        
        schisto
}

sDB_cohort <- format.dates(schisto = sDB_cohort)
sDB_study <- format.dates(schisto = sDB_study)


# Delay in months:

delay.months <- function(start, end) {
        endDate <- as.POSIXlt(end)
        startDate <- as.POSIXlt(start)
        12*(endDate$year - startDate$year) + (endDate$mon - startDate$mon)
}

sDB_cohort$B5_reportDelay <- delay.months(sDB_cohort$B4_rDate1,
                                          sDB_cohort$B5_rDate9)
sDB_study$B5_reportDelay <- delay.months(sDB_study$B4_rDate1,
                                         sDB_study$B5_rDate9)

delayCohort <- summary(sDB_cohort$B5_reportDelay)
delayStudy <- summary(sDB_study$B5_reportDelay)

# Or, in years:
delayCohort/12
delayStudy/12


# Graphical representation, per year, semester or quarter (term):

yearBreaks <- seq(0, max(sDB_cohort$B5_reportDelay, na.rm=TRUE)+12, by=12)
semesterBreaks <- seq(0, max(sDB_cohort$B5_reportDelay, na.rm=TRUE)+6, by=6)
termBreaks <- seq(0, max(sDB_cohort$B5_reportDelay, na.rm=TRUE)+3, by=3)

plot.delay <- function(schisto, breakpoints, title, colour) {
        hist(schisto$B5_reportDelay, breaks=breakpoints,
             xlab=NULL, ylab=NULL,   
             ylim=c(0,20), xlim=c(0,120),
             main=title,
             col=colour, border="white")
}

# High-resolution image:

setwd("C:/Users/Amélie JULE/Documents/Rwd_Current/dataCache")

tiff("S1 Figure.tif", width = 5, height = 4, units = "in",
     compression = "lzw", res = 300)
        
        par(fig = c(0, 1, 0.1, 1),
            mar = c(5, 2.1, 0.1, 0.1) + 0.1)
        plot.delay(sDB_cohort, semesterBreaks, NULL, "slategrey")
        abline(v=36, lty=2, col = "red", lwd=1.65)
        abline(v=42, lty=2, col = "red", lwd=1.65)
        
        par(fig = c(0, 1, 0, 0.25),
            mar = c(0, 2.1, 0, 0.1) + 0.1, new=TRUE)
        boxplot(sDB_cohort$B5_reportDelay,
                horizontal=TRUE, ylim=c(0,120),
                col=c("slategrey"), border="black", axes=FALSE)

dev.off()

summary(sDB_cohort$B5_reportDelay)

# The chosen representation is 'by cohort', because in several cases,
# multi-centric studies opened and closed recruitment at different times
# (thus a more or less extent publication delay).

# Number of missing values
length(which(is.na(sDB_cohort$B4_studyEnd)))
length(which(is.na(sDB_cohort$B5_reportDate)))