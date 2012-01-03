rm(list=ls())
oa <- 5
ob <- 0.4
sdev <- 0.5
x <- 1:30
y <- numeric(30)
for(i in 1:30) {
	y[i] <- rnorm(1, oa + ob * x[i], sdev)
}
N <- 100000
a <- numeric(N)
b <- numeric(N)
v <- numeric(N)
a[1] <- runif(1)
b[1] <- runif(1)
v[1] <- runif(1)
oL <- -2 * log(prod( dnorm(y, a[1] + b[1] * x, v[1]) ))

for (i in 2:N) {
	ta <- a[i - 1] + runif(1, -5, 5) 
	tb <- b[i - 1] + runif(1, -5, 5)

	repeat {
		tv <- v[i - 1] + runif(1, -1, 1) 
		if (tv > 0) {
			break
		}
	}
	L <- -2 * log(prod( dnorm(y, ta + tb * x, tv) ))
	if (L < oL) { 
		a[i] <- ta		
		b[i] <- tb
		v[i] <- tv
		oL <- L
	} else { 
		a[i] <- a[i - 1]
		b[i] <- b[i - 1]
		v[i] <- v[i - 1]
	}
}

par(bty = 'l', las = 1)
plot(x, y)
abline(a=a[N], b=b[N], lty = 2) # estimated line
abline(a=oa, b=ob) # real line

