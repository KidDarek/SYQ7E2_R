tiszta <- function(a,p){
s <- 0	
n <- 0
	for( i in 1:length(a))
	{
	n <- n + a[i]
	}
	for( i in 1:length(a))
	{
	s <- s + ((a[i]-(n*p[i]))^2)/(n*p[i])
	}
return(s)
}

a <- c(83,91,122,107,74,123)
p <- c(1/6,1/6,1/6,1/6,1/6,1/6)
tiszta(a,p)