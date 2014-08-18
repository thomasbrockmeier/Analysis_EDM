## Timbre similarity -----------------------------------------------------------
source("clean.data.R")
source("descriptives.R")
source("reliability.check.R")
source("fleiss.EDM.R")
source("select.EDM.R")
source("plot.EDM.R")
library("irr")
library("psych")

# Load the database
timbre <- reliability.check(read.delim("timbre20140619 (no dubious responses).csv", header=T))
general <- read.csv("general20140619.csv")

# Clean timbre data and describe it
clean.timbre <- clean.data.subjectwise(raw.data=reliability.check(timbre), conf.1.rm=F, polarize=F)
timbre.desc <- descriptives(clean.timbre, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)
clean.conf.timbre <- confidence.values(timbre)
conf.timbre.desc <- descriptives(clean.conf.timbre, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)
# Plot relations between means and SDs
plot.EDM(timbre.desc, conf.timbre.desc, "Timbre similarity ratings vs Confidence values in timbre")

# Clean general data and describe it
clean.general <- clean.data.subjectwise(raw.data=general, conf.1.rm=F, polarize=F)      ## REMOVE OUTLIER!!!
general.desc <- descriptives(clean.general, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)
clean.conf.general <- confidence.values(general)
conf.general.desc <- descriptives(clean.conf.general, raw=FALSE, conf.1.rm=FALSE, polarize=FALSE)
plot.EDM(general.desc, conf.general.desc, "General similarity ratings vs Confidence \nvalues in general similarity")

# Compare timbre data with general data
timbre.subset <- select.EDM(dataset=timbre.desc)
plot.EDM(timbre.subset,general.desc, "Timbre pairs vs General pairs")
plot.EDM(select.EDM(conf.timbre.desc), conf.general.desc, "Confidence levels of Timbre vs General")

# Get EDM experts and do some more statistics
not.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Not familiar with it",]
nft.desc <- descriptives(not.familiar.timbre)
conf.nft.desc <- descriptives(confidence.values(not.familiar.timbre), raw=F, conf.1.rm=F, polarize=F)

somewhat.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Somewhat familiar with it",]
sft.desc <- descriptives(somewhat.familiar.timbre)
conf.sft.desc <- descriptives(confidence.values(somewhat.familiar.timbre), raw=F, conf.1.rm=F, polarize=F)

very.familiar.timbre <- timbre[timbre$How.familiar.are.you == "Very familiar with it",]
vft.desc <- descriptives(very.familiar.timbre, conf.1.rm=F)
conf.vft.desc <- descriptives(confidence.values(very.familiar.timbre), raw=F, conf.1.rm=F, polarize=F)

fleiss.EDM(very.familiar.timbre,conf.1.rm=F, polarize=F, iterations=200)

# Find out if EDM "experts" are more confident about their ratings.
wilcox.test(conf.nft.desc$mean, conf.vft.desc$mean)     # Not enough data to consider
wilcox.test(conf.sft.desc$mean, conf.vft.desc$mean)     # Significant difference

# Do EDM experts rate differently?
wilcox.test(sft.desc$mean, vft.desc$mean)       # No, they are just more confident about it.

# Look for people whose favorite genre is EDM (regardless of level of expertise)
fav.edm.timbre <- timbre[timbre$Electronic.What == "Electronic",]
fet.desc <- descriptives(fav.edm.timbre)
conf.fet.desc <- descriptives(confidence.values(fav.edm.timbre), raw=F)

fav.not.edm.timbre <- timbre[timbre$Electronic.What != "Electronic",]
fnet.desc <- descriptives(fav.not.edm.timbre)
conf.fnet.desc <- descriptives(confidence.values(fav.not.edm.timbre), raw=F)

# Are EDM fans more confident about their ratings?
wilcox.test(conf.fet.desc$mean, conf.fnet.desc$mean)    # Definitely not

# Do EDM fans rate differently than non-fans?
wilcox.test(fet.desc$mean, fnet.desc$mean)      # No

# Do EDM fans rate differently than EDM experts?
wilcox.test(vft.desc$mean, fet.desc$mean)       # No
# Are they more confident?
# wilcox.test()

# Do EDM fans rate differently than people who are somewhat familiar with it?
wilcox.test(sft.desc$mean, fet.desc$mean)       # No
