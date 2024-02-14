thisfunction <- function(x){
	s <- 0
	n <- length(x)
	for (i in 1:n) {
  		s<- s + x[i]
	}
	print("Value: ")
	print(s/n)

	s2 <- 0
	med <- s/n
	for (i in 1:n) {
  		s2 <- s2 + (x[i]-med)^2
	}
	sn <- 1/(n-1) * s2
	print("Value2: ")
	print(sn)
}

thisfunction(c(1,2,4,5,2,7))


