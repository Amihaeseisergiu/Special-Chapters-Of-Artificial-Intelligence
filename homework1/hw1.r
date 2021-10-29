#ex1

x=c(1, 8, 2, 6, 2, 8, 8, 5, 5, 5)
print(sum(x)/10)
log2(x)
max(x)-min(x)
y = (x - 5)/2.624669
print(y)
print(mean(y))
print(sd(y))

#ex2
factura = c(46, 33, 39, 37, 36, 30, 48, 32, 49, 35, 30, 48)
print(sum(factura))
print(min(factura))
print(max(factura))
nr_luni = length(factura[factura > 40])
print(nr_luni)
print(100 * nr_luni/length(factura))

#ex3
z<-scan()
print(min(z))
print(max(z))
print(mean(z))
print(median(z))
print(sd(z))
sort(z)
print(z)
norm = (z - mean(z))/sd(z)
print(norm)