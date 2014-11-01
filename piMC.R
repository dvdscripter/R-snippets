# q = 4 * r2
# c = pi * r2
# 
# p = c/q
# 
# p = pi * r2 / 4 * r2
# p = pi / 4
# 
# pi = p * 4

drawCircle = function(centerPosition, radius, ...) {
	N = 1000
	circle = matrix(0, nrow = N, ncol = 2)
	count = 1
	for (i in seq(0, 2 * pi, length.out=N)) {
		circle[count, ] =	c(centerPosition[1] + radius * cos(i),
												centerPosition[2] + radius * sin(i)
											 )
		count = count + 1
	}
	lines(circle, ...)
}
png('quadrado e circulo.png', 1000, 1000)
plot(NULL, xlim = c(0, 10), ylim = c(0, 10))
drawCircle(c(5, 5), 2)
abline(v = c(5 + 2, 5 - 2))
abline(h = c(5 + 2, 5 - 2))
dev.off()

png('quadrado, circulo e lançamentos.png', 1000, 1000)
plot(NULL, xlim = c(0, 10), ylim = c(0, 10))
drawCircle(c(5, 5), 2)
abline(v = c(5 + 2, 5 - 2))
abline(h = c(5 + 2, 5 - 2))

N = 10000
inside = 0
outside = 0
piv = NULL
for(i in 1:N) {
	point = runif(2, 3, 7)
	if ( c( dist( rbind(c(5, 5), point) ) ) > 2 ) {
		outside = outside + 1
		points(point[1], point[2], col = 'blue', pch = 16, cex = .5)
	} else {
		inside = inside + 1
		points(point[1], point[2], col = 'red', pch = 16, cex = .5)
	}
	# pi = p * 4
	piv[i] = inside/N * 4
}
dev.off()