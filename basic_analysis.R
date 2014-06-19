general <- read.csv("general20140619.csv")
timbreWPC <- read.csv("timbreWPC.csv")
timbre <- read.delim("timbre20140606.csv", header=T)


library("irr")
library("psych")
# WPC
kappam.fleiss(ratings=t(timbreWPC[,seq(22, 57, 2)]))
icc(ratings=t(timbreWPC[,seq(22, 57, 2)]))

# General similarity
data.general <- general[general$Response.ID != 13 ,seq(22, 57, 2)]
colnames(data.general) <- c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)
desc.general <- describe(data.general)




kappam.fleiss(ratings=t(general[general$Response.ID != 13 ,seq(22, 57, 2)]))
icc(ratings=t(general[general$Response.ID != 13 ,seq(22, 57, 2)]), model="twoway",
    type="agreement" )
ICC(x=t(general[general$Response.ID != 13 ,seq(22, 57, 2)]))

