# this code shows how to do some type of maximum likelihood parameter estimation.
# draw a sample (n=100) from N(0, 1) and try to estimate mean and standard deviation.
rm(list=ls()) # clean environment
x <- rnorm(100) # sampling, can increase
N <- 10000 # number of iterations, increase it for more precise estimative/convergence
m <- numeric(N)
v <- numeric(N)
# starting values
m[1] <- 4
v[1] <- 10
oL <- -2*log(prod(dnorm(x, m[1], v[1])))
# starting values end

for (i in 2:N) {
	tm <- m[i - 1] + runif(1, -10, 10) # proposed new mean based in random walk (disturbe previous value)
	repeat {
		tv <- v[i - 1] + runif(1, -5, 5) # proposed new sd, run until get > 0 sd
		if (tv > 0) {
			break
		}
	}
	L <- -2*log(prod(dnorm(x, tm, tv))) # calc -2 * ln(Likelihood) (our brains understand positive numbers more easy)
	if (L < oL) { # check if new values are better them old ones, if so set it 
		m[i] <- tm
		v[i] <- tv
		oL <- L
	} else { # else repeat old values again
		m[i] <- m[i - 1]
		v[i] <- v[i - 1]
	}
}

# graphing
png('parâmetros.png', res = 100)
par(bty='l', las = 1, mfrow = c(2, 1), mar = c(0, 4, 2, 2) + 0.1)
plot(1:N, m, ylab = expression(mu), xlab='', type='l', xaxt='n')
Axis(side = 1, at = seq(0, N, 2000), labels = F)
abline(h = 0, lty = 2)
par(mar=c(5, 4, 1, 2) + 0.1)
plot(1:N, v, ylab=expression(sigma), xlab='Iteração', type='l', ylim=c(0, max(v)))
abline(h = 1, lty = 2)
dev.off()
# graphing end
# estimated values
m[N]
v[N]
