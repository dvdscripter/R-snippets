rm(list=ls())
x <- rnorm(100)
N <- 10000
m <- numeric(N)
v <- numeric(N)
m[1] <- 4
v[1] <- 10
oL <- -2*log(prod(dnorm(x, m[1], v[1])))

for (i in 2:N) {
	tm <- m[i - 1] + runif(1, -10, 10)
	repeat {
		tv <- v[i - 1] + runif(1, -5, 5)
		if (tv > 0) {
			break
		}
	}
	L <- -2*log(prod(dnorm(x, tm, tv)))
	if (L < oL) {
		m[i] <- tm
		v[i] <- tv
		oL <- L
	} else {
		m[i] <- m[i - 1]
		v[i] <- v[i - 1]
	}
}
png('parâmetros.png', res = 100)
par(bty='l', las = 1, mfrow = c(2, 1), mar = c(0, 4, 2, 2) + 0.1)
plot(1:N, m, ylab = expression(mu), xlab='', type='l', xaxt='n')
Axis(side = 1, at = seq(0, N, 2000), labels = F)
abline(h = 0, lty = 2)
par(mar=c(5, 4, 1, 2) + 0.1)
plot(1:N, v, ylab=expression(sigma), xlab='Iteração', type='l', ylim=c(0, max(v)))
abline(h = 1, lty = 2)
dev.off()
m[N]
v[N]
