rm(list=ls())
N <- 1e5
set.seed(1)
medias <- numeric(N)
amostras <- runif(N)
for (i in 1:N) {
	medias[i] <- mean(amostras[1:i])
}

png('black hole.png') # png win
par(las = 1, bty = 'l', fig=c(0, 1, 0, 1))
plot(log10(1:N), medias, type='l', xlab='log(sample number)', ylab='Estimated mean')
lines(log10(1e4:1e5), medias[1e4:1e5], col='red', lwd=2) # red line means we are approaching convergence
abline(h=.5) # real mean
par(fig=c(.64, 1, .52, .85), new=T)
hist(medias[1e4:1e5], main='\"Start\" of CLT', ylab='', xlab='', yaxt='n', xaxt='n') # central limit theorem is starting to be show here and so on.
dev.off()

