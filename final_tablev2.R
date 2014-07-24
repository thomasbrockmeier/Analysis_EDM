# install.packages('psych') # <-- If you don't have the 'psych' package, uncomment this line.
library(psych)
# setwd("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/Analysis")

## Load the raw dataset --------------------------------------------------------------------------
# pairs <- read.csv(file.choose(), header=T)
pairs <- read.csv("pairs.csv", header=T)

# rhythm
# raw.data.r <- read.delim("rhythmcombined.csv", header=T)
raw.data.r <- read.delim(file.choose(), header=T)
# data.r = raw.data.r[raw.data.r$Status == "Complete" & raw.data.r[,25] <= 2  & raw.data.r[,407] <= 2, seq(25,408)] 
data.r = raw.data.r[raw.data.r$Status == "Complete", seq(25,408)] 

# data.r = raw.data.r[, seq(25,408)]

# timbre
# raw.data.t <- read.delim("timbrecombined.csv", header=T)
raw.data.t <- read.delim(file.choose(), header=T)
# data.t = raw.data.t[raw.data.t$Status == "Complete" & raw.data.r[,25] <= 2 & raw.data.r[,407] <= 2, seq(25,408)]
data.t = raw.data.t[raw.data.t$Status == "Complete", seq(25,408)]
# data.t = raw.data.t[, seq(25,408)]


## Separate ratings and confidence levels
# rhythm
my.data.r <- as.data.frame(data.r[,seq(1,384,2)])
# timbre
my.data.t <- as.data.frame(data.t[,seq(1,384,2)])

# rhythm
conf.r <- as.data.frame(data.r[,seq(2,384,2)])
# timbre
conf.t <- as.data.frame(data.t[,seq(2,384,2)])

## Number the pairs
# rhythm
colnames(my.data.r) <- c("RC1", seq(1,190), "RC2")
colnames(conf.r) <- c("RC1", seq(1,190), "RC2")

# timbre
colnames(my.data.t) <- c("RC1", seq(1,190), "RC2")
colnames(conf.t) <- c("RC1", seq(1,190), "RC2")

# Remove values with Confidence level 1
# rhythm
my.data.r[conf.r == 1] = NA
# timbre
my.data.t[conf.t == 1] = NA

## Descriptive Statistics
desc.r <- as.data.frame(describe(my.data.r))
desc.t <- as.data.frame(describe(my.data.t))

rt.matrix <- function(desc.r, desc.t, rhythm, timbre, pairs, len, category) {
        std <- 2
        if (rhythm == 'high'){
                thresh1 = 2.8
                thresh2 = 4
        } else if (rhythm == 'low') {
                thresh1 = 1
                thresh2 = 2.2
        } else if ( rhythm == 'mid') {
                thresh1 = 2.2
                thresh2 = 2.8
        }
        if (timbre == 'high'){
                thresh3 = 2.8
                thresh4 = 4
        } else if (timbre == 'low') {
                thresh3 = 1
                thresh4 = 2.2
        } else if ( timbre == 'mid') {
                thresh3 = 2.2
                thresh4 = 2.8
        }
        mean.r <- desc.r[desc.r$mean >= thresh1 & desc.r$mean <= thresh2,]
        mean.t <- desc.t[desc.t$mean >= thresh3 & desc.t$mean <= thresh4,]
        #   Get the means within a range
        pair.number <- intersect(rownames(mean.r), rownames(mean.t))
        if (length(pair.number) >= 10){
                while (length(pair.number) >= 10){
                        mean.r <- desc.r[desc.r$mean >= thresh1 & desc.r$mean <= thresh2 & desc.r$sd < std,]
                        mean.t <- desc.t[desc.t$mean >= thresh3 & desc.t$mean <= thresh4 & desc.t$sd < std,]
                        pair.number <- intersect(rownames(mean.r), rownames(mean.t))
                        std <- std - 0.05
                }
        }
        #   Generate the columns for the final table
        Segment1 <-  as.vector(pairs[pair.number,2])
        Segment2 <- as.vector(pairs[pair.number,3])
        Rhythm.n <- desc.r[pair.number,2]
        Rhythm.mean <- desc.r[pair.number,3]
        Rhythm.sd <- desc.r[pair.number,4]
        Timbre.n <- desc.t[pair.number,2]
        Timbre.mean <- desc.t[pair.number,3]
        Timbre.sd <- desc.t[pair.number,4]
        #   Make the final table
        fin <- cbind(pair.number, Segment1, Segment2, category, Rhythm.n, Timbre.n, Rhythm.mean, Timbre.mean, Rhythm.sd, Timbre.sd)
        if (rhythm == 'high' & timbre == 'high'){
                fin[order(Rhythm.sd - Timbre.sd),]
        }
}

hrht <- rt.matrix(desc.r, desc.t, 'high', 'high', pairs, 10, 'HRHT')
# hrlt <- rt.matrix(desc.r, desc.t, thresh1=2.8, thresh2=4, thresh3=1, thresh4=2.2, pairs, 'HRLT')
# lrht <- rt.matrix(desc.r, desc.t, thresh1=1, thresh2=2.2, thresh3=2.8, thresh4=4, pairs, 4, 'LRHT')
# lrlt <- rt.matrix(desc.r, desc.t, thresh1=1, thresh2=2.2, thresh3=1, thresh4=2.2, pairs, 4, 'LRLT')
# hrmt <- rt.matrix(desc.r, desc.t, thresh1=2.8, thresh2=4, thresh3=2.2, thresh4=2.8, pairs, 3, 'HRMT')
# mrht <- rt.matrix(desc.r, desc.t, thresh1=2.2, thresh2=2.8, thresh3=2.8, thresh4=4, pairs, 3, 'MRHT')
# lrmt <- rt.matrix(desc.r, desc.t, thresh1=1, thresh2=2.2, thresh3=2.2, thresh4=2.8, pairs, 3, 'LRMT')
# mrlt <- rt.matrix(desc.r, desc.t, thresh1=2.2, thresh2=2.8, thresh3=1, thresh4=2.2, pairs, 7, 'MRLT')
# 
# final <- rbind(hrht, hrlt, lrht, lrlt, hrmt, mrht, lrmt, mrlt)
# write.csv(final, 'final_list.csv')
