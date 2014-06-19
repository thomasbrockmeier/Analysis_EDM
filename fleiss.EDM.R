fleiss.EDM <- function(data, conf.1.rm = TRUE, polarize = FALSE, iterations = 1000){
        require("irr")
        source("clean.data.R")
        source("reliability.check.R")
        dt <- clean.data.subjectwise(reliability.check(data), conf.1.rm=conf.1.rm, polarize=polarize)
        desc <- as.data.frame(describe(dt))
        tmin <- min(desc$n)
        fd <- as.data.frame(matrix(nrow=0, ncol=1))
        for (i in 1:iterations) {
                d <- dt[sample(ncol(dt)),]
                m2 <- t(apply(t(d), 1, function(x) x[order(is.na(x))])) # sort NAs to end of ea row
                t <- m2[, 1:tmin]
                fleiss <- kappam.fleiss(t)
                if (i == 1) colnames(fd) = colnames(fleiss$results)
                fd <- rbind(fd, fleiss$value)
        }
        print(paste("Minimum ratings: ", as.character(tmin)))
        print(describe(fd))
        
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014
