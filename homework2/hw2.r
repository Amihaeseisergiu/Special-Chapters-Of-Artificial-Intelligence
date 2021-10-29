ex1 = function(a, b)
{
	plot(log2, a, b, col="red", lwd=2, main = "f(x)= log2 x, x in [a,b]")
}

ex2 = function()
{
	for(pr in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
	{
		pdf(file=
			paste("D:\\Facultate\\Capitole Speciale de Inteligenta Artificiala\\homework2\\plots\\binom_", pr, ".pdf", sep=""),
			width=4, height=4
		   )
		
		x = 0:20
		plot(x, dbinom(x, size=20, prob=pr),
     		     	type="h",
     			main=paste("Binomial Distribution (n=20, p=", pr, ")", sep=""),
     			ylab="Probability",
     			xlab ="# Successes",
     			lwd=3)
		dev.off()
	}
}

ex3 = function()
{
	x = seq(-7, 7, .1)
	y = dnorm(x, mean=0, sd=.5)
	plot(x, y, type="l", col="red", lwd=2)
	par(new=T)
	y = dnorm(x, mean=0, sd=1)
	plot(x, y, type="l", col="green", lwd=2, yaxt='n', xaxt='n')
	par(new=T)
	y = dnorm(x, mean=0, sd=2)
	plot(x, y, type="l", col="blue", lwd=2, yaxt='n', xaxt='n')
}

CLT = function(n)
{
	vret = vector("double", 1000)
	for(i in 1:1000)
	{
		v = runif(n, 0, 20)
		vret[i] = mean(v)
	}
	vret
}

ex3b = function()
{
	par(mfrow=c(2,2))

	for(n in c(1, 5, 10, 100))
	{
		y = CLT(n)
		hist(y, nclass=50, col="red", main=paste("Histogram uniform of", n))
	}

}

CLT2 = function(n)
{
	vret = vector("double", 1000)
	for(i in 1:1000)
	{
		v = rbinom(n, 20, 0.1)
		vret[i] = mean(v)
	}
	vret
}

ex3c = function()
{
	par(mfrow=c(2,2))

	for(n in c(1, 5, 10, 100))
	{
		y = CLT2(n)
		hist(y, nclass=50, col="blue", main=paste("Histogram binomial of", n))
	}
}