## Timbre similarity -----------------------------------------------------------
source("clean.data.R")
source("descriptives.R")
source("reliability.check.R")
source("fleiss.EDM.R")
source("select.EDM.R")
library("irr")
library("psych")

# Load the database
timbre <- reliability.check(read.delim("timbre20140619 with checks.csv", header=T))
general <- read.csv("general20140619.csv")

# Clean timbre data and describe it
clean.timbre <- clean.data.subjectwise(raw.data=reliability.check(timbre), conf.1.rm=F, polarize=F)
timbre.desc <- descriptives(clean.timbre, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)
clean.conf.timbre <- confidence.values(timbre)
conf.timbre.desc <- descriptives(clean.conf.timbre, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)


# Separate musically trained and untrained
musically.trained <- timbre[timbre$Have.you.had == "Yes",]
musically.untrained <- timbre[timbre$Have.you.had == "No",]

# Run descriptive statistics on general, trained, and untrained datasets.
general.timbre <- descriptives(timbre)
trained.timbre <- descriptives(musically.trained)
untrained.timbre <- descriptives(musically.untrained)


# Run descriptives on confidence values and order them on "difficulty"
conf.desc <- descriptives(clean.conf.timbre, raw=F, conf.1.rm=F, polarize=F)
conf.desc <- conf.desc[order(conf.desc$mean, conf.desc$sd),]

# Compare with general similarity
data.general <- general[general$Response.ID != 13 ,seq(22, 57, 2)]
colnames(data.general) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
desc.general <- describe(data.general)
# Separate the pairs from the General Similarity in Timbre Similarity
selection.timbre <- clean.timbre[,c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)]
desc.selection.timbre <- descriptives(selection.timbre, raw = F)

fleiss.EDM(data.general)
fleiss.EDM(selection.timbre)

# Wilcoxon tests
wilcox.test(desc.general$sd, desc.selection.timbre$sd)

# # Fleiss' Kappa Timbre vs General
# fleiss.timbre.selec <- fleiss.EDM(selection.timbre, iterations = 200)
# fleiss.general <- fleiss.EDM(data.general, iterations = 200)

# Order them based on Standard Deviation
general.timbre.ordered <- general.timbre[order(general.timbre$sd),]
trained.timbre.ordered <- trained.timbre[order(trained.timbre$sd),]
untrained.timbre.ordered <- untrained.timbre[order(untrained.timbre$sd),]

# Run Fleiss' Kappa
fleiss.general.timbre <- fleiss.EDM(timbre, iterations=200)
fleiss.mt.timbre<- fleiss.EDM(musically.trained,iterations = 200)
fleiss.mut.timbre <- fleiss.EDM(musically.untrained, iterations = 200)

# Get HiFi vs LoFi situations
hifi.timbre <- timbre[(timbre$Headphones == "Headphones" | timbre$Professional.monitors == "Professional monitors") ,]
lofi.timbre <- timbre[(timbre$Earbuds == "Earbuds" | timbre$Laptop.speakers == "Laptop.speakers" | timbre$External.speakers == "External speakers") ,]

# Descriptives of HiFi and LoFi
hifi.desc <- descriptives(data=hifi.timbre)
lofi.desc <- descriptives(data=lofi.timbre)

# Fleiss' Kappa HiFi vs LoFi
fleiss.hifi <- fleiss.EDM(hifi.timbre, iterations = 200)
fleiss.lofi <- fleiss.EDM(lofi.timbre, iterations = 200)

# People who work with music vs people who don't
music.wrk.timbre <- timbre[timbre$Do.you.work == "Yes",]
music.notwrk.timbre <- timbre[timbre$Do.you.work == "No",]

mwt.desc <- descriptives(music.wrk.timbre)
mnwt.desc <- descriptives(music.notwrk.timbre)

# Fleiss' Kappa music workers vs non-musical workers
fleiss.mw <- fleiss.EDM(music.wrk.timbre, iterations = 200)
fleiss.notwrk <- fleiss.EDM(music.notwrk.timbre, iterations = 200)

# Familiarity with EDM
not.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Not familiar with it",]
nft.desc <- descriptives(not.familiar.timbre)
somewhat.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Somewhat familiar with it",]
sft.desc <- descriptives(somewhat.familiar.timbre)
very.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Very familiar with it",]
vft.desc <- descriptives(very.familiar.timbre)

wilcox.test(sft.desc$sd, vft.desc$sd)

fleiss.nft <- fleiss.EDM(not.familiar.timbre, iterations=200)
fleiss.sft <- fleiss.EDM(somewhat.familiar.timbre, iterations=200)
fleiss.vft <- fleiss.EDM(very.familiar.timbre, iterations=200)

# Separate between Listeners, Musicians/Producers, and DJs
listener.timbre <- timbre[timbre$Listener == "Listener",]
musician.timbre <- timbre[timbre$Musician.Producer == "Musician/Producer",]
dj.timbre <- timbre[timbre$DJ == "DJ",]

listen.desc <- descriptives(listener.timbre)
musician.desc <- descriptives(musician.timbre)
dj.desc <- descriptives(dj.timbre)

# Instrument check
harmonic.instrument <- timbre[timbre$instrument.check == 1,]

# Fleiss' Kappa of harmonic instrument players
fleiss.hip <- fleiss.EDM(harmonic.instrument, iterations = 200)

# Strategy check
correct.strategy <- timbre[timbre$Strategy.check == 1,]

# Fleiss' Kappa of strategies nvolving timbral elements
fleiss.tes <- fleiss.EDM(correct.strategy, iterations = 200)

# Look for the highest SDs and analyze them
timbre.desc.ordered <- timbre.desc[order(timbre.desc$sd, decreasing = TRUE),]
highest.sd.pairs <- timbre.desc.ordered[1:20,"PairNumber"]
highest.sd.data <- clean.timbre[,highest.sd.pairs]

# Plot the highest SDs
for (i in 1:ncol(highest.sd.data)) {
        hist(t(na.omit(highest.sd.data[i])), main = paste("Histogram of pair", colnames(highest.sd.data[i]), "(n=", timbre.desc[i,"n"], ")"), xlab = "Rating")
}
