## This file has two functions to clean raw data obtained from the Survey Gizmo 
# platform, formatted according to the output containing the two reliability
# check pairs.

## The function "clean.data.pairwise" outputs the data with the subjects as variables
# and the pairs as cases. It takes answers from the raw data (disregarding
# the reliability check pairs), removes the ratings that were reported as "Not
# confident" if conf.1.rm = TRUE (default) and outputs a data frame with the 
# following columns:
#         Pair number
#         Track 1 name
#         Track 2 name
#         Each participant as a column
clean.data.pairwise <- function(raw.data, conf.1.rm = TRUE, tracknames = TRUE, polarize = FALSE){
#         Get the ratings from the raw data
        data = raw.data[, seq(27, 406, 2)]
#         Get the confidence levels for the ratings
        conf = raw.data[, seq(28, 406, 2)]
#         Remove the ratings reported as "Not confident"
        if (conf.1.rm == T) data[conf == 1] = NA
        if (polarize == TRUE) data[data == 2] <- 1; data[data == 3 | data == 4] <- 2
#         Number the pairs and transpose the database
        if (tracknames == TRUE) {
                tdata <- cbind(seq(1,190), t(x=data))
                colnames(tdata) <- c("PairNumber", seq(1, length(data[,1])))
        } else {
                tdata <- t(data)
        }
#         Load the pair list and merge it with the database
        if (tracknames == TRUE) {
                source("tracklist.R")
                merge(x=tracklist, y=tdata, by="PairNumber")
        } else {
                tdata
        }
}

## The function "clean.data.subjectwise" outputs the data with the pairs as variables
# and the subjects as cases. It takes answers from the raw data (disregarding
# the reliability check pairs), and removes the ratings that were reported as "Not
# confident" if conf.1.rm = TRUE (default). 
clean.data.subjectwise <- function(raw.data, conf.1.rm = TRUE, polarize = FALSE){
#         Get the ratings from the raw data
        data = raw.data[, seq(27, 406, 2)]
#         Get the confidence levels for the ratings
        conf = raw.data[, seq(28, 406, 2)]
#         Remove the ratings reported as "Not confident"
        if (conf.1.rm == T) data[conf == 1] = NA
        if (polarize == TRUE) data[data == 2] <- 1; data[data == 3 | data == 4] <- 2
#         Number the pairs (column-wise)
        colnames(data) <- seq(1,190)
        data
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014