## Timbre similarity -----------------------------------------------------------
source("clean.data.R")
source("descriptives.R")
source("reliability.check.R")
source("fleiss.EDM.R")
library("irr")
library("psych")

# Load the database
timbre <- reliability.check(read.delim("timbre20140619 (no dubious responses).csv", header=T))
general <- read.csv("general20140619.csv")


# Separate musically trained and untrained
musically.trained <- timbre[timbre$Have.you.had == "Yes",]
musically.untrained <- timbre[timbre$Have.you.had == "No",]

# Run descriptive statistics on general, trained, and untrained datasets.
general.timbre <- descriptives(timbre)
trained.timbre <- descriptives(musically.trained)
untrained.timbre <- descriptives(musically.untrained)

# Compare with general similarity
data.general <- general[general$Response.ID != 13 ,seq(22, 57, 2)]
colnames(data.general) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
desc.general <- describe(data.general)
# Separate the pairs from the General Similarity in Timbre Similarity
selection.timbre <- general.timbre[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]

# Order them based on Standard Deviation
general.timbre.ordered <- general.timbre[order(general.timbre$sd),]
trained.timbre.ordered <- trained.timbre[order(trained.timbre$sd),]
untrained.timbre.ordered <- untrained.timbre[order(untrained.timbre$sd),]

# Run Fleiss' Kappa on the three
fleiss.general.timbre <- fleiss.EDM(timbre, conf.1.rm=F, iterations=200)
fleiss.trained.timbre <- fleiss.EDM(musically.trained, conf.1.rm=F, iterations=200)
fleiss.untrained.timbre <- fleiss.EDM(musically.untrained, conf.1.rm=F, iterations=200)

# Get HiFi vs LoFi situations
hifi.timbre <- timbre[(timbre$Headphones == "Headphones" | timbre$Professional.monitors == "Professional monitors") ,]
lofi.timbre <- timbre[(timbre$Earbuds == "Earbuds" | timbre$Laptop.speakers == "Laptop.speakers" | timbre$External.speakers == "External speakers") ,]

# Descriptives of HiFi and LoFi
hifi.desc <- descriptives(data=hifi.timbre)
lofi.desc <- descriptives(data=lofi.timbre)

# Fleiss' Kappa of HiFi and LoFi
fleiss.hifi <- fleiss.EDM(hifi.timbre, conf.1.rm=F, polarize=F, iterations=200)
fleiss.lofi <- fleiss.EDM(lofi.timbre, conf.1.rm=F, polarize=F, iterations=200)

# People who work with music vs people who don't
music.wrk.timbre <- timbre[timbre$Do.you.work == "Yes",]
music.notwrk.timbre <- timbre[timbre$Do.you.work == "No",]

mwt.desc <- descriptives(music.wrk.timbre)
mnwt.desc <- descriptives(music.notwrk.timbre)

# Familiarity with EDM
not.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Not familiar with it",]
nft.desc <- descriptives(not.familiar.timbre)
somewhat.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Somewhat familiar with it",]
sft.desc <- descriptives(somewhat.familiar.timbre)
very.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Very familiar with it",]
vft.desc <- descriptives(very.familiar.timbre)

wilcox.test(sft.desc$sd, vft.desc$sd)

# Separate between Listeners, Musicians/Producers, and DJs
listener.timbre <- timbre[timbre$Listener == "Listener",]
musician.timbre <- timbre[timbre$Musician.Producer == "Musician/Producer",]
dj.timbre <- timbre[timbre$DJ == "DJ",]

listen.desc <- descriptives(listener.timbre)
musician.desc <- descriptives(musician.timbre)
dj.desc <- descriptives(dj.timbre)
