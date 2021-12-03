estimate_y = function(y, x)
{
	coeffs = coefficients(lm(y~x))

	x %*% coeffs[-1] + coeffs[1]
}

estimate_s = function(y, x)
{
	m = nrow(x)
	n = ncol(x)
	y_estimate = estimate_y(y, x)

	s = sum( (y - y_estimate)^2 ) / (m - n - 1)
	s
}

get_measures = function(y, x, s)
{
	x = as.matrix(x)
	y = as.matrix(y)

	m = nrow(x)
	n = ncol(x)

	y_estimate = estimate_y(y, x)
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

forward_selection = function(y, x, significance)
{
	remaining_preds = 1:ncol(x)
	significant_preds = c()

	while(length(remaining_preds) != 0)
	{
		most_significant = NULL
		most_significance = 2
		
		for(pred in remaining_preds)
		{
			s = coefficients(
				summary(
					lm(y~x[,c(significant_preds, pred)])
				))[-1, 4]
			pred_significance = s[length(s)]
			
			if(pred_significance <= significance && 
				pred_significance < most_significance)
			{
				most_significant = pred
				most_significance = pred_significance
			}
		}

		if(is.null(most_significant))
			break
		
		remaining_preds = remaining_preds[
			remaining_preds != most_significant]
		significant_preds = c(significant_preds, most_significant)
	}
	
	significant_preds
}

get_all = function(y, x, cp_index)
{
	data = best_subset(y, x)
	data[[4]][cp_index,][[2]]
}

ex1 = function()
{
	data = read.table(
			paste0(
				"D:\\Facultate\\",
				"Capitole Speciale de Inteligenta Artificiala\\",
				"homework5\\data\\house.dat"),
			header=T, sep="", dec=".")
	y = data[,1]
	x = data.matrix(data[,-1])
	
	#all = get_all(y, x, 6)
	forward_selection(y, x, 0.05)
}