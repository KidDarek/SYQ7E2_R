lin<- function(x, y){
	s<- 0
	a<- y
	sum<- 0
	for (i in 1:length(x)) {
  		s<- a*(x[i]^y-1)
		print(paste("Value: " , a , "*" , x[i] , "^", y-1))
		print(s)
		sum<- sum + s
	}
	print("value of sum: ")
	print(sum)
}

lin(rnorm(100),5)