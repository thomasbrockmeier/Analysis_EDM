icc.EDM <- function(data, conf.1.rm = TRUE, polarize = FALSE, iterations = 1000){
        require("psych")
        source("clean.data.R")
        source("reliability.check.R")
        dt <- clean.data.subjectwise(reliability.check(data), conf.1.rm=conf.1.rm, polarize=polarize)
        desc <- as.data.frame(describe(dt))
        tmin <- min(desc$n)
        fd <- as.data.frame(matrix(nrow=0, ncol=8))
        for (i in 1:iterations) {
                d <- dt[sample(ncol(dt)),]
                m2 <- t(apply(t(d), 1, function(x) x[order(is.na(x))])) # sort NAs to end of ea row
                t <- m2[, 1:tmin]
                icc <- ICC(t)
                if (i == 1) colnames(fd) = colnames(icc$results)
                fd <- rbind(fd, icc$results[1,])
        }
        print(summary(fd))
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014