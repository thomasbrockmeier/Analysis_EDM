plot.EDM <- function(descriptives1, descriptives2, title){
        if (!identical(dim(descriptives1), dim(descriptives2))){
                stop("Both datasets must have the same dimensions.")
        }
        if (!is.character(title)){
                stop("Please input the title of your plot as a string.")
        }
        par(mfrow=c(2,2), oma=c(3,3,4,0), mar=c(4,2,1,1), las=1, cex=0.7)
        plot(x=scale(descriptives1$sd), y=scale(descriptives2$mean), main='Correlation between the Standard Deviation of the ratings and the mean "difficulty" of the pair', 
             xlab="SD of similarity ratings", ylab="Mean confidence level")
        lines(lowess(descriptives1$sd, descriptives2$mean), col="red")
        abline(lm(descriptives2$mean~descriptives1$sd), col="blue")
        
        plot(x=scale(descriptives1$mean), y=scale(descriptives2$mean), main='Correlation between the mean of the ratings and the mean "difficulty" of the pair', 
             xlab="Mean of similarity ratings", ylab="Mean confidence level")
        lines(lowess(descriptives1$mean, descriptives2$mean), col="red")
        abline(lm(descriptives2$mean~descriptives1$mean), col="blue")
        
        plot(x=scale(descriptives1$sd), y=scale(descriptives2$sd), main='Correlation between the SD of the ratings and the SD of the "difficulty" of the pair', 
             xlab="SD of similarity ratings", ylab="SD of confidence level")
        lines(lowess(descriptives1$sd, descriptives2$sd), col="red")
        abline(lm(descriptives2$sd~descriptives1$sd), col="blue")
        
        plot(x=scale(descriptives1$mean), y=scale(descriptives2$sd), main='Correlation between the mean of the ratings and the SD of the "difficulty" of the pair', 
             xlab="Mean of similarity ratings", ylab="SD of confidence level")
        lines(lowess(descriptives1$sd, descriptives2$mean), col="red")
        abline(lm(descriptives2$sd~descriptives1$mean), col="blue")
        
        title(main=title,outer=T, cex=1)
}