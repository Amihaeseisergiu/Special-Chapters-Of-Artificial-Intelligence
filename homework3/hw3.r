ex1 = function()
{
	data = read.table("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\homework3\\data\\alcool.dat", header=T, sep="", dec=".")
	
	#1
	x = data[,2]
	y = data[,3]
	plot(x, y, pch=20, main='Scatter plot alcohol',
		xlab='Median alcohol consumption',
		ylab='Hearth disease related deaths')
	
	#2
	#raw formula
	x_mean = mean(x)
	y_mean = mean(y)
	r = sum( (x-x_mean) * (y-y_mean) ) /
	    (sqrt(sum( (x-x_mean)^2 )) * sqrt(sum( (y-y_mean)^2 )))
	message("My formula's result: ", r)

	#r function
	message("R formula's result: ", cor(x,y))
}

ex2 = function()
{
	data = read.table("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\homework3\\data\\iq.dat", header=T, sep="", dec=".")

	#1
	x = data[,2]
	y = data[,3]

	plot(x, y, pch=20, main='Scatter plot IQ',
		xlab='IQ',
		ylab='Grade')

	#2
	x_mean = mean(x)
	y_mean = mean(y)
	b1 = sum( (x-x_mean) * (y-y_mean) )/sum( (x-x_mean)^2 )
	b0 = y_mean - b1*x_mean
	abline(b0, b1, lwd=3, col='red')

	message("Grade estimate for 115 IQ: ", b0 + b1*115)
	message("Grade estimate for 130 IQ: ", b0 + b1*130)
}

ex3 = function(m, a, b, xmin, xmax, sigma)
{
	x = runif(m, min=xmin, max=xmax)
	e = rnorm(m, 0, sigma)
	y = a + b*x + e
	cbind(x, y)
}

ex4 = function(obs)
{
	x = obs[,1]
	y = obs[,2]
	
	x_mean = mean(x)
	y_mean = mean(y)
	b = sum( (x-x_mean) * (y-y_mean) )/sum( (x-x_mean)^2 )
	a = y_mean - b*x_mean
	
	y_estimate = a + b*x
	sse = sum( (y - y_estimate)^2 )
	s = sqrt(sse / (length(y) - 2))
	sb = s / sqrt(sum( (x-x_mean)^2 ))
	t = 2.571
	
	blow = b - t*sb
	bhigh = b + t*sb

	sa = sb*sqrt( sum(x^2) / length(x) )
	alow = a - t*sa
	ahigh = a + t*sa
	
	cbind(c(a,b), c(blow, bhigh), c(alow, ahigh))
}

plot_and_save = function(m, a, b, xmin, xmax, sigma, id)
{
	pdf(file=paste("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\homework3\\graphs\\graph_", id, ".pdf", sep=""),
		width=8, height=8
	)
	
	obs = ex3(m, a, b, xmin, xmax, sigma)
	res = ex4(obs)

	plot(obs[,1], obs[,2], pch=20,
		main=paste('a: ', res[1,1], ', b: ', res[2,1], '\n',
			'CI b: [', res[1,2], ' , ', res[2,2], ']\n',
			'CI a: [', res[1,3], ' , ', res[2,3], ']', sep=""),
		xlab='x',
		ylab='y')

	abline(a, b, lwd=3, col='red')
	abline(res[1,1], res[2,1], lwd=2, col='green')

	dev.off()
}

ex5 = function()
{
	#a
	plot_and_save(100, 10, 0.8, -200, 200, 1.5, 'a')

	#b
	plot_and_save(10, 10, 0.8, -5, 5, 1, 'b')

	#c
	plot_and_save(10000, 10, 0.8, -5, 5, 1, 'c')

	#d
	plot_and_save(10, 10, 0.8, 5, 5.2, 1, 'd')

	#e
	plot_and_save(10000, 10, 0.8, 5, 5.2, 1, 'e')

	#f
	plot_and_save(10, 10, 0.8, 5, 5.2, 0.01, 'f')

	'done'
}