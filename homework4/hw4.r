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
	s = estimate_s(y, x)

	get_measures(y, x[,1:2], s)
}