
# Load all required data, packages, and functions -------------------------
source("clean.data.R")
source("descriptives.R")
source("reliability.check.R")
source("fleiss.EDM.rev.R")
source("select.EDM.R")
library("irr")
library("psych")

# Load the databases
timbre <- read.delim("timbre20140619 with checks.csv", header=T)
rhythm <- read.csv("20140714/rhythm20140714.csv")
timbreWPC <- read.delim("20140714/timbreWPC20140714.csv", header=T)
rhythmWPC <- read.delim("20140714/rhythmWPC20140714.csv", header=T)
general <- read.csv("general20140619.csv")


# Analysis of timbre ------------------------------------------------------

## Get descriptive statistics on the timbre data
# 2-point scale
desc.timbre.2 <- descriptives(data = timbre, polarize = TRUE)

# 4-point scale
desc.timbre.4 <- descriptives(data = timbre, polarize = FALSE)

## Use Fleiss' Kappa on random samples of the ratings
# 2-point scale
fleiss.timbre.2 <- fleiss.EDM(data = timbre, polarize = TRUE, iterations = 100, min.ratings = 3)

# 4-point scale
fleiss.timbre.4 <- fleiss.EDM(data = timbre, polarize = FALSE, iterations = 100, min.ratings = 3)

## Run Wilcoxon test on Timbre and Rhythm vs General similarity data
# Select the pairs present in the General similarity experiment
# 2-point scale
clean.timbre.2 <- clean.data.subjectwise(raw.data=reliability.check(timbre), conf.1.rm=F, polarize=T)
selection.timbre.2 <- clean.timbre.2[,c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)]

# 4-point scale
clean.timbre.4 <- clean.data.subjectwise(raw.data=reliability.check(timbre), conf.1.rm=F, polarize=F)
selection.timbre.4 <- clean.timbre.4[,c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)]

# Get the data for the General Similarity experiment
data.general <- general[general$Response.ID != 13 ,seq(22, 57, 2)]
colnames(data.general) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)

kappam.fleiss(t(data.general.2))


# 2-point scale
data.general.2 <- data.general
data.general.2[data.general.2 == 2] <- 1
data.general.2[data.general.2 == 3 | data.general.2 == 4] <- 2


# Compare the data for each case
wilc <- matrix(nrow = 0, ncol = 2)
for (i in 1:ncol(selection.timbre.2)){
        wlx <- wilcox.test(x = selection.timbre.2[,i], y = data.general.2[,i])
        wilc <- rbind(wilc, c(wlx$statistic, wlx$p.value))
}
rownames(wilc) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)


