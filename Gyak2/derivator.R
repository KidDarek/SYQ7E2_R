der <- function(x,y){
	s<- 0
	a<- 1
	sum<- 0
	for (i in 1:y) {
  		s<- a*(x^y)
		print(paste("Value: " , a , "x" , "^", y))
		print(s)
		a<- y
		y<- y-1
		sum<- sum + s
	}
	print("value: ")
	print(sum)
}

der(2,6)
