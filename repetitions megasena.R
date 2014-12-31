N = 100000
sorteios = replicate(N, sample(1:60, size=6))
true = 0
#rep_list = list()
for (i in 1:(ncol(sorteios) - 1)) {
	temp = sorteios[, i] %in% sorteios[, i + 1]
	if (any(temp)) { 
		# rep_list[[paste(i)]] = sorteios[temp, i]
		# ^ create a list of the repition numbers.
		true = true + 1
	}
}
true/ncol(sorteios)
