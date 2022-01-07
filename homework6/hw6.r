plot_variance_plot = function(pc)
{
	pdf(file=paste0(
			"D:\\Facultate\\",
			"Capitole Speciale de Inteligenta Artificiala\\",
			"homework6\\graphs\\Variance Plot.pdf"),
			width=8, height=8)
	
	comp_names = paste(rep("Comp", length(pc$sdev)), seq(1, length(pc$sdev)))
	
	barplot(pc$sdev^2, names=comp_names,
		ylab = "Variance",
		main = "Variance Plot")

	dev.off()

}

plot_screeplot = function(pc)
{
	pdf(file=paste0(
			"D:\\Facultate\\",
			"Capitole Speciale de Inteligenta Artificiala\\",
			"homework6\\graphs\\Screeplot.pdf"),
			width=8, height=8)

	plot(pc$sdev^2, type = "l", col="blue",
		xlab = "# Principal Components",
		ylab = "Cumulative Variance",
		main = "Principal Components Screeplot")
	points(pc$sdev^2, pch=16)

	abline(h = 1, col="green", lty=1)
	legend("topright", legend=c("Eigenvalue = 1"),
      	col=c("green"), lty=1, cex=1, bty="n")

	dev.off()
}

plot_cumulative_variance = function(pc)
{
	pdf(file=paste0(
			"D:\\Facultate\\",
			"Capitole Speciale de Inteligenta Artificiala\\",
			"homework6\\graphs\\Cumulative Variance Plot.pdf"),
			width=8, height=8)
	
	pc_percentages = 100 * cumsum(pc$sdev^2 / sum(pc$sdev^2))
	plot(pc_percentages, col="blue",
		 type="l",
		 xlab = "# Principal Components",
		 ylab = "% of explained variance",
		 main = "Cumulative Variance Plot")
	points(pc_percentages, pch=16)

	dev.off()
}

plot_biplot = function(pc)
{
	pdf(file=paste0(
			"D:\\Facultate\\",
			"Capitole Speciale de Inteligenta Artificiala\\",
			"homework6\\graphs\\Biplot.pdf"),
			width=8, height=8)

	biplot(pc, cex = 0.5)
	abline(h = 0)
	abline(v = 0)

	dev.off()
}

pca = function(centered=T, scaled=F)
{
	data = read.table(
			paste0(
				"D:\\Facultate\\",
				"Capitole Speciale de Inteligenta Artificiala\\",
				"homework6\\data\\swiss.txt"),
			header=T, sep="", dec=".")

	data = scale(data, center=centered, scale=scaled)

	pc = prcomp(data)

	plot_variance_plot(pc)
	plot_screeplot(pc)
	plot_cumulative_variance(pc)
	plot_biplot(pc)

	pc_eigen_var = pc$sdev^2
	pc_eigen_gt_1 = length(pc_eigen_var[pc_eigen_var > 1])
	eigenvectors = t(pc$rotation)
	sd_variables = diag(var(data))
	sd_pc = pc$sdev
	loadings = (eigenvectors / sd_variables) * sd_pc

	print("Eigenvectors:")
	print(eigenvectors)
	print("Correlation Coefficients:")
	print(loadings[seq(1, pc_eigen_gt_1),])
}