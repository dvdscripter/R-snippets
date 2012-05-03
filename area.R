#  area.R
#
#  Copyright (c) 2012, David Valentim Dias <dvdscripter@gmail.com>
#
#  All rights reserved.
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#  
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above
#    copyright notice, this list of conditions and the following disclaimer
#    in the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of the David Valentim Dias nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



# Executando
# dados=data.frame(plot_id=rep(1,10),azimute=rep(0,10), segmento=rep(10, 10), remove=rep(0, 10))
# pdf('gráficos com as áreas.pdf')
# doArea(dados, plotting=TRUE)
# dev.off()

# Abra o pdf na pasta de trabalho e MÁGICA 

# Exemplo de tabela
#   plot_id azimute segmento remove
#1        1       0       10      0
#2        1       0       10      0
#3        1       0       10      1
#4        1       0       10      0
#5        1       0       10      0
#6        1       0       10      0
#7        1       0       10      0
#8        1       0       10      0
#9        1       0       10      0
#10       1       0       10      0





# Função responsável por criar a linha central
#	A tabela de entrada deve ter os seguintes atributos:
#	<azimute> e <segmento>
#	Ex.:
#	plot_id			azimute	segmento	remove
#	M01_TN_0500	170			10				0
#	M01_TN_0500	160			10				0
#	M01_TN_0500	155			10				0
#	M01_TN_0500	155			10				1
#	M01_TN_0500	160			10				0
#	M01_TN_0500	160			10				0
#	...
#	M01_TN_0500	30			10				0
#	M01_TN_0500	40			10				0
#	M01_TN_1500	225			10				0
#	M01_TN_1500	220			10				1
#	M01_TN_1500	230			10				0
#	M01_TN_1500	245			10				0
#	...

# Sendo que o id da parcela é o objeto usado para identificar a origem de cada 
#		azimute e segmento.
# A coluna <remove> marca se esse segmento sera removido por algum motivo do calculo área da parcela
#	A saída da função é uma lista contendo as coordenadas para plotagem e os ângulos
#		em radianos.

doCentraline <- function(data) {
	xys <- data.frame(x=0, y=0)
	thetas <- NULL
	for (i in 2:nrow(data)) {
		theta <- data$azimute[i - 1] * pi/180
		xys[i,] <- c(xys[i-1, 1] + data$segmento[i] * cos(theta), 
								 xys[i-1, 2] + data$segmento[i] * sin(theta))
		thetas[i - 1] <- theta
	}
	list(coordenadas = xys, angulos = c(thetas, thetas[length(thetas)]), remove = data$remove)	
}

# Função corretora de ângulos
# Se maior que 360 remova 360
# Se menor que 0 adicione 360
# Ex.: 400 - 360 = 40
#			 -5  + 360 = 355
correctAng <- function(x) {
	if (is.na(x)) { 
		return(NA)
	} else if (x > (2 * pi)) {
		return (x - 2 * pi)
	} else if (x < 0) {
		return (x + 2 * pi)
	} else {
		x
	}
}

# Função que calcula o ângulo utilizando a lei dos cossenos
ang <- function(coord) {
	ang3 <- NULL
	if (all(coord[,2] == coord[1,2])) {
		if (coord[2,1] > coord[3,1]) {
			return(c(NA, pi))
		} else {
			return(c(NA, 0))
		}
	}
	for (i in 2:(nrow(coord) - 1)) {
		b <- dist(coord[c(i - 1, i),])
		cc <- dist(coord[c(i, i+1),])
		a <- dist(coord[c(i - 1, i + 1),])
		ang3[i] <- acos((b^2 + cc^2 - a^2) / (2 * b * cc))
		if (is.na(ang3[i])) {			
			return(c(NA, pi))
		}
	}
	ang3
}

# Função que verifica se o ângulo é menor que <angle> e retorna um vetor 
#		contendo TRUE onde ocorre o ângulo menor que <angle>.
angLess <- function(coord, angle) {
	col <- rep(FALSE, nrow(coord))
	ang3 <- NULL
	for (i in 2:(nrow(coord) - 1)) {
		b <- dist(coord[c(i - 1, i),])
		cc <- dist(coord[c(i, i+1),])
		a <- dist(coord[c(i - 1, i + 1),])
		ang3[i] <- acos((b^2 + cc^2 - a^2) / (2 * b * cc))
		if (is.na(ang3[i])) { 
			next
		}
		if (ang3[i] < (angle*pi/180)) {
			col[(i):(i+1)] <- TRUE
		}
	}
	col
}

# Função que verifica se existe cruzamentos em dois segmentos de linha
# referência http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
findd <- function(a, b) {
	if (is.na(a) || is.na(b)) return(FALSE)
	deno <- (b[2,2] - b[1,2]) * (a[2,1] - a[1,1]) - (b[2,1] - b[1,1]) * (a[2,2] - a[1,2])
	if (deno == 0) return(FALSE)
	ua <- (  (b[2,1] - b[1,1]) * (a[1,2] - b[1,2]) - (b[2,2] - b[1,2]) * (a[1,1] - b[1,1]) ) / deno
	ub <- (  (a[2,1] - a[1,1]) * (a[1,2] - b[1,2]) - (a[2,2] - a[1,2]) * (a[1,1] - b[1,1]) ) / deno
	if (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1) return(TRUE)
	return(FALSE)
}

# Função auxiliar para o calculo das áreas. Não deve ser chamada manualmente, apenas por outra função (doArea)
Support <- function(top, sut) {
	topSupportPoints <- data.frame(x = 0, y = 0)
	for (j in 1:(nrow(top)-1)) {
		if (j == 1 ) {
			c4 <- dist(rbind(top[j,],sut[j,],top[j+1,]))[-3]
			switch(
						which.min(c4),
				{
					topSupportPoints = rbind(topSupportPoints, top[j,], sut[j,])
				},
				{
					topSupportPoints = rbind(topSupportPoints, top[j,], top[j+1,])
				}
			)
		} else {
			c4 <- dist(rbind(sut[j-1,],top[j,],top[j+1,], sut[j,]))[-c(1,6)]
			switch(
						which.min(c4),
				# 1
				{
					topSupportPoints = rbind(topSupportPoints, sut[j - 1, ], top[j + 1,])
				},
				# 2
				{
					topSupportPoints = rbind(topSupportPoints,sut[j - 1,], sut[j,])
				},
				# 3
				{
					topSupportPoints = rbind(topSupportPoints,top[j,], top[j + 1,])
				},
				# 4
				{
					topSupportPoints = rbind(topSupportPoints,top[j,], sut[j,])
				}
			)
		}
	}
	topSupportPoints[-1,]
}

# Função que informa se o poligno é côncavo ou convexo
# Importante para saber se o ângulo formula uma entrada ou não no poligno
concave <- function(a, b, c) {
	# -1 = concave
	#  1 = convex
	#  0 = no side, colinear
	d = ((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
	if ( d > 0 ) {
		return(1)
	} else if ( d < 0) {
		return(-1)
	}
	return (0)
}

# Calcula a área da parcela excluído os segmentos com ângulo menor que 
#		<lessAngle> e que <remove> for igual a 1 (um). O parametro <d> pode assumir 'top' , 'bot' e 'both'.
#	'both': area de ambos os lados da parcela
# 'top': area do lado 'superior'
# 'bot': area do lado 'superior'
# 'sut': pontos de suporte para o lado norte fornecidos por doArea
# 'sub': pontos de suporte para o lado norte fornecidos por doArea
calcArea <- function(trilha, top, bot, sut, sub, lessAngle, d = 'both') {
	conc = NULL
	for (i in 2:(nrow(trilha$coordenadas) - 1 ) ) {
		if (all(trilha$angulos[(i-1):(i)] == trilha$angulos[i] )) {
			conc[i] = 0
		} else {
			conc[i] = concave(trilha$coordenadas[i-1,],trilha$coordenadas[i,],trilha$coordenadas[i+1,])
		}
	}
	stop = Support(top, sut)
	sbot = Support(bot, sub)
	
	P <- angLess(trilha$coordenadas, lessAngle)
	area <- 0
	for (i in 1:(nrow(trilha$coordenadas) - 1)) {
		if (!all(P[i:(i+1)] == T) && trilha$remove[i+1] == 0 ) {
			if ( d == 'both') {
				return(calcArea(trilha, top, bot, sut, sub, lessAngle, d = 'top') +
				calcArea(trilha, top, bot, sut, sub, lessAngle, d = 'bot'))
			}
			if (d == 'top' ) {
				if ( i > 1 && i < (nrow(trilha$coordenadas) - 1) ) {
					if (conc[i] == -1 && conc[i+1] == -1) { # se o proximo e o atual são concavos
						tempr = rbind(trilha$coordenadas[i:(i+1),], stop[((i+1)*2-2):(i*2-1),], trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],top[i,]))^2 * ang(rbind(stop[(i+1)*2-2,],trilha$coordenadas[i+1,],stop[(i+1)*2-1,]))[2] )/2
					} else if (conc[i] == -1 && conc[i+1] > -1 ) { # se o proximo não é concavo mas o atual é
						tempr = rbind(trilha$coordenadas[i:(i+1),],top[i+1,],stop[i*2-1,],trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else if (conc[i] > -1 && conc[i+1] == -1 ) { #se o proximo é concavo mas o atual não
						tempr = rbind(trilha$coordenadas[i:(i+1),],stop[(i+1)*2-2,], top[i,], trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],top[i,]))^2 * ang(rbind(stop[(i+1)*2-2,],trilha$coordenadas[i+1,],stop[(i+1)*2-1,]))[2] )/2
					} else { # se nenhum deles é concavo
						tempr = rbind(trilha$coordenadas[i:(i+1),], top[(i+1):i,], trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					}
				} else if ( i == 1) {
					if (conc[i+1] == -1) {
						tempr = rbind(trilha$coordenadas[i:(i+1),],stop[(i+1)*2-2,],top[i,],trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],top[i,]))^2 * ang(rbind(stop[(i+1)*2-2,],trilha$coordenadas[i+1,],stop[(i+1)*2-1,]))[2] )/2
					} else {
						tempr = rbind(trilha$coordenadas[i:(i+1),],top[(i+1):i,],trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					}
				} else if (i == (nrow(trilha$coordenadas) - 1) ) {
					if (conc[i] == -1) {
						tempr = rbind(trilha$coordenadas[i:(i+1),],top[i+1,], stop[i*2-1,],trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else {
						tempr = rbind(trilha$coordenadas[i:(i+1),],top[(i+1):i,], trilha$coordenadas[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					}
				}
			} else if (d == 'bot') {
				if ( i > 1 && i < (nrow(trilha$coordenadas) - 1) ) {
					if (conc[i] == -1 && conc[i+1] == -1 ) { # se o proximo e o atual são concavos
						tempr = rbind(bot[i:(i+1),], trilha$coordenadas[(i+1):i,], bot[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else if (conc[i] == -1 && conc[i+1] > -1) { # se o proximo não é concavo mas o atual é
						tempr = rbind(bot[i,], sbot[(i+1)*2-2,],trilha$coordenadas[(i+1):i,],bot[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],bot[i,]))^2 * ang(rbind(sbot[(i+1)*2-2,],trilha$coordenadas[i+1,],sbot[(i+1)*2-1,]))[2] )/2
					} else if (conc[i] > -1 && conc[i+1] == -1 ) { #se o proximo é concavo mas o atual não
						tempr = rbind(sbot[i*2-1,],bot[i+1,],trilha$coordenadas[(i+1):i,],sbot[i*2-1,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else { # se nenhum deles é concavo
						tempr = rbind(sbot[i*2-1,], sbot[(i+1)*2-2,], trilha$coordenadas[(i+1):i,], sbot[i*2-1,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],top[i,]))^2 * ang(rbind(sbot[(i+1)*2-2,],trilha$coordenadas[i+1,],sbot[(i+1)*2-1,]))[2] )/2
					}
				} else if ( i == 1) {
					if (conc[i+1] == -1) {
						tempr = rbind(bot[i:(i+1),], trilha$coordenadas[(i+1):i,], bot[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else {
						tempr = rbind(bot[i,], sbot[(i+1)*2-2,], trilha$coordenadas[(i+1):i,], bot[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2 + ( dist(rbind(trilha$coordenadas[i,],top[i,]))^2 * ang(rbind(sbot[(i+1)*2-2,],trilha$coordenadas[i+1,],sbot[(i+1)*2-1,]))[2] )/2
					}
				} else if (i == (nrow(trilha$coordenadas) - 1) ) {
					if (conc[i] == -1) {
						tempr = rbind(bot[i:(i+1),], trilha$coordenadas[(i+1):i,], bot[i,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					} else {
						tempr = rbind(sbot[i*2-1,], bot[i+1,], trilha$coordenadas[(i+1):i,], sbot[i*2-1,])
						ta <- sum(tempr[1:(nrow(tempr)-1), 1] * tempr[2:nrow(tempr), 2]) - sum(tempr[1:(nrow(tempr)-1), 2] * tempr[2:nrow(tempr), 1])
						area = area + ta/2
					}
				}
			}
		}	
	}
	area
}

# Cria o gráfico da parcela parâmetros semelhantes a calcArea incluindo o resultado de calcArea no parâmetro <area>
plotParcela <- function(parcela, top, bot, lessAngle, nparcela, d = 'both', area) {
	plot(NULL, xlim = c(-300, 300), ylim = c(-300, 300), main = paste(nparcela, round(area, 2)))
	P <- angLess(parcela$coordenadas, lessAngle)
	for (i in 1:(nrow(parcela$coordenadas) - 1)) {
		if (!all(P[i:(i+1)] == T) && parcela$remove[i] == 0 ) {
			lines(parcela$coordenadas[i:(i+1),], col='red')
			if (d == 'both') {
				lines(top[i:(i+1),])
				lines(bot[i:(i+1),])
			} else if (d == 'top') {
				lines(top[i:(i+1),])
			} else if (d == 'bot') {
				lines(bot[i:(i+1),])
			}
		}	
	}
}

# <data> tabela de dados (veja os comentários em doCentraline)
# <width> largura de cada um dos lados da parcela
# <lessAngle> ângulo (graus) de corta para remoção do segmento 
# <d> se for 'top' calcula a área apenas para o lado 'norte', se se for 'bot' 
#	 calcula a área apenas para o lado 'sul' e se for 'both' para ambos os lados
#	<plotting> plotar as áreas? ([T]RUE, [F]ALSE)
doArea <- function(data, width = 5, lessAngle = 70, d = 'both', plotting = FALSE) {
	central <- by(data, data[, 1], doCentraline)
	area <- NULL
	for (i in 1:length(central)) {
		top <- bot <- data.frame(x = 0, y = 0)
		topp <- bott <- sut <- sub <- final <- data.frame(x = NA, y = NA)
		if (any(is.na(central[[i]]$angulos))) next
		for (j in 1:nrow(central[[i]]$coordenadas)) {
			top[j, 1] <- central[[i]]$coordenadas[j, 1] + width * cos(correctAng(central[[i]]$angulos[j] + pi / 2))
			top[j, 2] <- central[[i]]$coordenadas[j, 2] + width * sin(correctAng(central[[i]]$angulos[j] + pi / 2))
			bot[j, 1] <- central[[i]]$coordenadas[j, 1] + width * cos(correctAng(central[[i]]$angulos[j] - pi / 2))
			bot[j, 2] <- central[[i]]$coordenadas[j, 2] + width * sin(correctAng(central[[i]]$angulos[j] - pi / 2))
			if (j != nrow(central[[i]]$coordenadas)) {
				sut[j, 1] <- central[[i]]$coordenadas[j + 1, 1] + width * cos(correctAng(central[[i]]$angulos[j] + pi / 2))
				sut[j, 2] <- central[[i]]$coordenadas[j + 1, 2] + width * sin(correctAng(central[[i]]$angulos[j] + pi / 2))
				sub[j, 1] <- central[[i]]$coordenadas[j + 1, 1] + width * cos(correctAng(central[[i]]$angulos[j] - pi / 2))
				sub[j, 2] <- central[[i]]$coordenadas[j + 1, 2] + width * sin(correctAng(central[[i]]$angulos[j] - pi / 2))
			}
		}

		
		for (j in 1:(nrow(top) - 1)) {
			if (is.na(central[[i]]$angulos[j]) || is.na(central[[i]]$angulos[j+1]) ) break
			x <- central[[i]]$coordenadas[j + 1, ]
			x[1] <- x[1] + 10
			p1 <- ang( rbind(x, central[[i]]$coordenadas[j + 1, ], sut[j, ])  )[2]
			p2 <- ang( rbind(x, central[[i]]$coordenadas[j + 1, ], top[j + 1, ])  )[2]
			if ( is.na(p1) || is.na(p2)) { 
				next
			}
			if ( sut[j, 2] < x[2]  ) {
				p1 <- 2 * pi - p1
			}
			if ( top[j+1, 2] < x[2] ) {
				p2 <- 2 * pi - p2
			}
			mp <- mean(c(p1, p2))
			topp[j+1, ] <- c(central[[i]]$coordenadas[j + 1, 1] + width * cos(mp), central[[i]]$coordenadas[j + 1, 2] + width * sin(mp))
			bott[j + 1, ] <- c(central[[i]]$coordenadas[j + 1, 1] - width * cos(mp), central[[i]]$coordenadas[j + 1, 2] - width * sin(mp))
		}
		topp[1, ] <- top[1,]
		bott[1, ] <- bot[1,]
		for (j in 1:(nrow(topp) - 1) ) {
			if (findd(topp[j:(j + 1),],bott[j:(j + 1),]) ) {
				temp <- topp[j + 1,]
				topp[j + 1,] <- bott[j + 1,]
				bott[j + 1,] <- temp
			}
		}
		area[i] <- calcArea(central[[i]], topp, bott, sut, sub, lessAngle, d)
		if ( ! any(is.na(central[[i]]$angulos)) && plotting == T ) {
			plotParcela(central[[i]], topp, bott, lessAngle, names(central[i]), d, area[i])
		}
		
	}
	area
}



