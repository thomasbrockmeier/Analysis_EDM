# install.packages("irr") # <-- If you don't have the "irr" package, uncomment this line.
require('irr')
setwd("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/Analysis")

## Load the raw dataset
# rhythm
# data.r <- read.csv("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/rhythmpilot.csv", header=F)
raw.data.r <- read.csv(file.choose(), header=T)
data.r = raw.data.r[raw.data.r$Status == "Complete", seq(25,404)]
# timbre
# data.t <- read.csv("~/Dropbox/Escuela/Master Brain and Cognitive Sciences/Internship/Research Project 2/R/timbrepilot.csv", header=F)
raw.data.t <- read.csv(file.choose(), header=T)
data.t = raw.data.t[raw.data.t$Status == "Complete", seq(25,404)]


## Remove confidence levels
# rhythm
my.data.r <- as.data.frame(t(data.r[,seq(1,380,2)]))
# timbre
my.data.t <- as.data.frame(t(data.t[,seq(1,380,2)]))

## List of confidence levels for each pair 
# rhythm
conf.r <- as.data.frame(t(data.r[,seq(2,380,2)]))
# timbre
conf.t <- as.data.frame(t(data.t[,seq(2,380,2)]))

## Number the pairs
# rhythm
rownames(my.data.r) <- seq(1,190)
rownames(conf.r) <- seq(1,190)

# timbre
rownames(my.data.t) <- seq(1,190)
rownames(conf.t) <- seq(1,190)


## Create a duplicate data set to polarize
# rhythm
my.data.r.p <- my.data.r
# timbre
my.data.t.p <- my.data.t

##Polarize scales--------------------------------------------------------------------------------
# 2-point scale
# rhythm
my.data.r.p[my.data.r.p == 2] = 1
my.data.r.p[my.data.r.p == 3] = 4
# timbre
my.data.t.p[my.data.t.p == 2] = 1
my.data.t.p[my.data.t.p == 3] = 4

## Remove NAs to analyze-------------------------------------------------------------------------
# (4-point scale)
# rhythm
# list removing NA's
lst.r <- apply(my.data.r, 1, function(x) x[!is.na(x)])
# maximum lenght
ll.r <- max(sapply(lst.r, length))
# combine 
ratings.r <- as.data.frame(t(sapply(lst.r, function(x) c(x, rep(NA, ll.r-length(x))))))

# timbre
# list removing NA's
lst.t <- apply(my.data.t, 1, function(x) x[!is.na(x)])
# maximum lenght
ll.t <- max(sapply(lst.t, length))
# combine 
ratings.t <- as.data.frame(t(sapply(lst.t, function(x) c(x, rep(NA, ll.t-length(x))))))

# (2-point scale)
# rhythm
# list removing NA's
lst.r.p <- apply(my.data.r.p, 1, function(x) x[!is.na(x)])
# maximum lenght
ll.r.p <- max(sapply(lst.r.p, length))
# combine 
ratings.r.p <- as.data.frame(t(sapply(lst.r.p, function(x) c(x, rep(NA, ll.r.p-length(x))))))

# timbre
# list removing NA's
lst.t.p <- apply(my.data.t.p, 1, function(x) x[!is.na(x)])
# maximum lenght
ll.t.p <- max(sapply(lst.t.p, length))
# combine 
ratings.t.p <- as.data.frame(t(sapply(lst.t.p, function(x) c(x, rep(NA, ll.t.p-length(x))))))
# ----------------------------------------------------------------------------------------------

## Analyze the general inter-rater concordance with Fleiss' Kappa (not reliable)
# 4-point scale
# rhythm
overall.kappa.r <- kappam.fleiss(ratings.r)
# timbre
overall.kappa.t <- kappam.fleiss(ratings.t)
# 2-point scale
# rhythm
overall.kappa.r.p <- kappam.fleiss(ratings.r.p)
# timbre
overall.kappa.t.p <- kappam.fleiss(ratings.t.p)
# ----------------------------------------------------------------------------------------------
## Analyze the inter-rater concordance for each pair with Fleiss' Kappa'
# 4-point scale
# rhythm
kappas.r <- data.frame()
agreement.r <- data.frame()
num.ratings.r <- data.frame()
for (i in seq(1,length(ratings.r[,1]))){
  rater.r <- t(na.exclude(t(ratings.r[i,])))
  n.r <- length(rater.r)
  k.r <- kappam.fleiss(rater.r)$value
  a.r <- agree(rater.r, tolerance = 0.5)$value
  kappas.r <- c(kappas.r, k.r)
  agreement.r <- c(agreement.r,a.r)
  num.ratings.r <- c(num.ratings.r, n.r)
}
kappas.r <- t(as.data.frame(kappas.r))
rownames(kappas.r) <- seq(1,190)
agreement.r <- t(as.data.frame(agreement.r))
rownames(agreement.r) <- seq(1,190)
num.ratings.r <- t(as.data.frame(num.ratings.r))
rownames(num.ratings.r) <- seq(1,190)

# timbre
kappas.t <- data.frame()
agreement.t <- data.frame()
num.ratings.t <- data.frame()
for (i in seq(1,length(ratings.t[,1]))){
  rater.t <- t(na.exclude(t(ratings.t[i,])))
  n.t <- length(rater.t)
  k.t <- kappam.fleiss(rater.t)$value
  a.t <- agree(rater.t, tolerance = 0.5)$value
  kappas.t <- c(kappas.t, k.t)
  agreement.t <- c(agreement.t,a.t)
  num.ratings.t <- c(num.ratings.t, n.t)
}
kappas.t <- t(as.data.frame(kappas.t))
rownames(kappas.t) <- seq(1,190)
agreement.t <- t(as.data.frame(agreement.t))
rownames(agreement.t) <- seq(1,190)
num.ratings.t <- t(as.data.frame(num.ratings.t))
rownames(num.ratings.t) <- seq(1,190)

# 2-point scale
# rhythm
kappas.r.p <- data.frame()
agreement.r.p <- data.frame()
num.ratings.r.p <- data.frame()
for (i in seq(1,length(ratings.r.p[,1]))){
  rater.r.p <- t(na.exclude(t(ratings.r.p[i,])))
  n.r.p <- length(rater.r.p)
  k.r.p <- kappam.fleiss(rater.r.p)$value
  a.r.p <- agree(rater.r.p, tolerance = 0.5)$value
  kappas.r.p <- c(kappas.r.p, k.r.p)
  agreement.r.p <- c(agreement.r.p,a.r.p)
  num.ratings.r.p <- c(num.ratings.r.p, n.r.p)
}
kappas.r.p <- t(as.data.frame(kappas.r.p))
rownames(kappas.r.p) <- seq(1,190)
agreement.r.p <- t(as.data.frame(agreement.r.p))
rownames(agreement.r.p) <- seq(1,190)
num.ratings.r.p <- t(as.data.frame(num.ratings.r.p))
rownames(num.ratings.r.p) <- seq(1,190)

# timbre
kappas.t.p <- data.frame()
agreement.t.p <- data.frame()
num.ratings.t.p <- data.frame()
for (i in seq(1,length(ratings.t.p[,1]))){
  rater.t.p <- t(na.exclude(t(ratings.t.p[i,])))
  n.t.p <- length(rater.t.p)
  k.t.p <- kappam.fleiss(rater.t.p)$value
  a.t.p <- agree(rater.t.p, tolerance = 0.5)$value
  kappas.t.p <- c(kappas.t.p, k.t.p)
  agreement.t.p <- c(agreement.t.p,a.t.p)
  num.ratings.t.p <- c(num.ratings.t.p, n.t.p)
}
kappas.t.p <- t(as.data.frame(kappas.t.p))
rownames(kappas.t.p) <- seq(1,190)
agreement.t.p <- t(as.data.frame(agreement.t.p))
rownames(agreement.t.p) <- seq(1,190)
num.ratings.t.p <- t(as.data.frame(num.ratings.t.p))
rownames(num.ratings.t.p) <- seq(1,190)
# ----------------------------------------------------------------------------------------------
## Create a table with the analysis
analysis <- cbind(num.ratings.r, kappas.r, agreement.r, kappas.r.p, agreement.r.p, num.ratings.t, kappas.t, agreement.t, kappas.t.p, agreement.t.p)
colnames(analysis) <- c("Number of ratings (rhythm)", "Fleiss' Kappa (rhythm, 4pt)", "Agreement (rhythm 4pt)", "Fleiss' Kappa (rhythm, 2pt)", "Agreement (rhythm 2pt)", "Number of ratings (timbre)", "Fleiss' Kappa (timbre, 4pt)", "Agreement (timbre 4pt)", "Fleiss' Kappa (timbre, 2pt)", "Agreement (timbre 2pt)") 
analysis[analysis == NaN] <- 1.0

## Write a CSV file with the results
write.csv(analysis, file = "Pilot analysis.csv")