## Plot timbre relationships between rating means and SDs vs confidence level means and SDs----
plot(x=scale(timbre.desc$sd), y=scale(conf.timbre.desc$mean), main='Correlation between the Standard Deviation of the ratings\nand the mean "difficulty" of the pair', 
     xlab="SD of similarity ratings", ylab="Mean confidence level")
lines(lowess(timbre.desc$sd, conf.timbre.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(timbre.desc$sd), y=scale(conf.timbre.desc$mean)), cex=0.7)

plot(x=scale(timbre.desc$mean), y=scale(conf.timbre.desc$mean), main='Correlation between the mean of the ratings\nand the mean "difficulty" of the pair', 
     xlab="Mean of similarity ratings", ylab="Mean confidence level")
lines(lowess(timbre.desc$mean, conf.timbre.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(timbre.desc$mean), y=scale(conf.timbre.desc$mean)), cex=0.7)

plot(x=scale(timbre.desc$sd), y=scale(conf.timbre.desc$sd), main='Correlation between the SD of the ratings\nand the SD of the "difficulty" of the pair', 
     xlab="SD of similarity ratings", ylab="SD of confidence level")
lines(lowess(timbre.desc$sd, conf.timbre.desc$sd), col="red")
legend("topright", "Correlation:", cor(x=scale(timbre.desc$sd), y=scale(conf.timbre.desc$sd)), cex=0.7)

plot(x=scale(timbre.desc$mean), y=scale(conf.timbre.desc$sd), main='Correlation between the mean of the ratings\nand the SD of the "difficulty" of the pair', 
     xlab="Mean of similarity ratings", ylab="SD of confidence level")
lines(lowess(timbre.desc$sd, conf.timbre.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(timbre.desc$mean), y=scale(conf.timbre.desc$sd)), cex=0.7)



## Plot general relationships between rating means and SDs vs confidence level means and SDs----
plot(x=scale(general.desc$sd), y=scale(conf.general.desc$mean), main='Correlation between the SD of the ratings\nand the mean "difficulty" of the pair', 
     xlab="SD of similarity ratings", ylab="Mean confidence level")
lines(lowess(general.desc$sd, conf.general.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(general.desc$sd), y=scale(conf.general.desc$mean)), cex=0.7)

plot(x=scale(general.desc$mean), y=scale(conf.general.desc$mean), main='Correlation between the mean of the ratings\nand the mean "difficulty" of the pair', 
     xlab="Mean of similarity ratings", ylab="Mean confidence level")
lines(lowess(general.desc$mean, conf.general.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(general.desc$mean), y=scale(conf.general.desc$mean)), cex=0.7)

plot(x=scale(general.desc$sd), y=scale(conf.general.desc$sd), main='Correlation between the SD of the ratings\nand the SD of the "difficulty" of the pair', 
     xlab="SD of similarity ratings", ylab="SD of confidence level")
lines(lowess(general.desc$sd, conf.general.desc$sd), col="red")
legend("topright", "Correlation:", cor(x=scale(general.desc$sd), y=scale(conf.general.desc$sd)), cex=0.7)

plot(x=scale(general.desc$mean), y=scale(conf.general.desc$sd), main='Correlation between the mean of the ratings\nand the SD of the "difficulty" of the pair', 
     xlab="Mean of similarity ratings", ylab="SD of confidence level")
lines(lowess(general.desc$sd, conf.general.desc$mean), col="red")
legend("topright", "Correlation:", cor(x=scale(general.desc$mean), y=scale(conf.general.desc$sd)), cex=0.7)


