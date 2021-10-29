ex1 = function()
{
	data = read.table("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\tema3\\data\\alcool.dat", header=T, sep="", dec=".")
	
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
	data = read.table("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\tema3\\data\\iq.dat", header=T, sep="", dec=".")

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