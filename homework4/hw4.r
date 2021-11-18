estimate_y = function(coeffs, x)
{
	x %*% coeffs[-1] + coeffs[1]
}

estimate_s = function(y, x)
{
	m = nrow(x)
	n = ncol(x)
	coeffs = coefficients(lm(y~x))
	y_estimate = estimate_y(coeffs, x)

	s = sum( (y - y_estimate)^2 ) / (m - n - 1)
	s
}

get_measures = function(y, x, s)
{
	x = as.matrix(x)
	y = as.matrix(y)

	m = nrow(x)
	n = ncol(x)
	coeffs = coefficients(lm(y~x))

	y_estimate = estimate_y(coeffs, x)
	y_mean = mean(y)

	rss = sum( (y - y_estimate)^2 )
	tss = sum( (y - y_mean)^2 )

	r = 1 - rss/tss
	ra = 1 - (1 - r) * ( (m - 1) / (m - n - 1) )
	cp = rss/s - (m - 2*(n+1))

	rbind(rss, r, ra, cp)
}

best_subset = function(y, x)
{
	s = estimate_s(y, x)
	all_preds = 1:ncol(x)

	best_rss = best_r = best_ra = best_cp = c()	

	for(pred in all_preds)
	{
		combs = combn(all_preds, pred)
		combs_v = if(ncol(combs) > 1) 2:ncol(combs) else 1

		best_comb_measures = get_measures(y, x[, combs[,1]], s)
		best_rss_comb = best_r_comb = best_ra_comb = best_cp_comb = combs[,1]

		for(comb in combs_v)
		{
			local_measures = get_measures(y, x[, combs[, comb]], s)

			if(local_measures[1] < best_comb_measures[1])
			{
				best_comb_measures[1] = local_measures[1]
				best_rss_comb = combs[, comb]
			}
			if(local_measures[2] > best_comb_measures[2])
			{
				best_comb_measures[2] = local_measures[2]
				best_r_comb =  combs[, comb]
			}
			if(local_measures[3] > best_comb_measures[3])
			{
				best_comb_measures[3] = local_measures[3]
				best_ra_comb = combs[, comb]
			}
			if(abs(local_measures[4] - (pred+1)) < best_comb_measures[4])
			{
				best_comb_measures[4] = abs(local_measures[4] - (pred+1))
				best_cp_comb = combs[, comb]
			}
		}

		best_rss = rbind(best_rss, cbind(best_comb_measures[1], list(best_rss_comb)))
		best_r = rbind(best_r, cbind(best_comb_measures[2], list(best_r_comb)))
		best_ra = rbind(best_ra, cbind(best_comb_measures[3], list(best_ra_comb)))
		best_cp = rbind(best_cp, cbind(best_comb_measures[4], list(best_cp_comb)))
	}

	rbind(list(best_rss), list(best_r), list(best_ra), list(best_cp))
}

plot_data = function(x, y, file_name)
{
	pdf(file=paste0(
			"D:\\Facultate\\",
			"Capitole Speciale de Inteligenta Artificiala\\",
			"homework4\\graphs\\",
			file_name, ".pdf"),
			width=8, height=8)
	plot(x, y, type = "l", lty = 1,
		main=file_name,
		xlab="dimensions",
		ylab=file_name)
	dev.off()
}

plot_and_save = function(data)
{
	xs = 1:nrow(data[[1]])
	
	plot_data(xs, unlist(data[[1]][,1]), "RSS")
	plot_data(xs, unlist(data[[2]][,1]), "R_Squared")
	plot_data(xs, unlist(data[[3]][,1]), "R_Adj")
	plot_data(xs, unlist(data[[4]][,1]), "Cp")
}

ex1 = function()
{
	data = read.table(
			paste0(
				"D:\\Facultate\\",
				"Capitole Speciale de Inteligenta Artificiala\\",
				"homework4\\data\\house.dat"),
			header=T, sep="", dec=".")
	y = data[,1]
	x = data.matrix(data[,-1])

	data = best_subset(y, x)
	plot_and_save(data)
	data
}