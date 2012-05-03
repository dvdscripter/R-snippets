# This show why sample variance and standard deviation should be scaled to 1/(n - 1)
rm(list = ls()) # clear the environment variables
N <- 10000 # number of std's to be calculated
n <- 10 # sample size
mu <- 100 # population mean
std <- 5 # standard deviation mean (graphic label this is STD)
set.seed(1) # setting seed to be possible to repeat this graphic in any computer
std1 <- numeric(N) # allocating space for std's
std2 <- numeric(N) # same above
for (i in 1:N) {
	s <- rnorm(n, mu, std)
	std1[i] <- sqrt((sum( (s-mean(s))^2 ))/(n - 1))
	std2[i] <- sqrt((sum( (s-mean(s))^2 ))/(n))
}
png("std.png")
par(mfrow = c(2, 1), las = 1, lwd = 1.5, mar = c(4, 4, 1, 2) + 0.1)
hist(std1 - std, main = '', col = c(rep('black', 8), rep('white', 7)), border = c(rep('white', 8), rep('black', 7)), ann = F, xaxt = 'n', xlim = c(-4, 4))
Axis(side = 1, at = seq(-4, 4, 2), labels = F)
mtext(expression( sqrt(frac(1, N-1) * sum((x[i]-bar(x)),i==1,N)^2 ) - STD), side = 1, line = 3)
mtext("Frequency", side = 2, las = 0, line = 3)
abline(v = 0, col = 'red', lwd = 2)
text(-3.3, 1000, paste((length(which(std1-std<0))/N)*100, "%", sep = ''))
par(mar = c(5.5, 4, 1, 2) + 0.1)
hist(std2-std, main = '', col = c(rep('black', 8), rep('white', 7)), border = c(rep('white', 8), rep('black', 7)), ann = F, xlim= c (-4, 4))
mtext(expression( sqrt(frac(1, N) * sum((x[i]-bar(x)),i==1,N)^2 ) - STD), side = 1, line = 4)
mtext("Frequency", side = 2, las = 0, line = 3)
abline(v = 0, col = 'red', lwd = 2)
text(-3.3, 1000, paste((length(which(std2-std<0))/N)*100, "%", sep = ''))
dev.off()

