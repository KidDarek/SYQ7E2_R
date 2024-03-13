szum <- function(x,h){
    s <- 0
    n <- length(x)
    for(i in 1:n){
        s <- s + x[i]^h
    }
    return(s)
}
 
szum2 <- function(x,y,h){
    s <- 0
    n <- length(x)
    for(i in 1:n){
        s <- s + x[i]^h * y[i]
    }
    return(s)
}
 
deter <- function(m){
	d <- m[1]*m[4] - m[2]*m[3]
    return(d)
}

matrixdeter <- function(m){
	a1<-m[1]
	a2<-m[4]
	a3<-m[7]
	m1<- m[-c(1), -c(1)]
	m2<- m[-c(1), -c(2)]
	m3<- m[-c(1), -c(3)]
	s<- a1*deter(m1) - a2*deter(m2) + a3*deter(m3)
	return(s)
}

a2MatrixCreator <- function(x,y)
{
	m <- matrix(, nrow = 3, ncol = 3)
	m[1] <- szum2(x,y,2)
	m[2] <- szum2(x,y,1)
	m[3] <- szum2(x,y,0)
	m[4] <- szum(x,3)
	m[5] <- szum(x,2)
	m[6] <- szum(x,1)
	m[7] <- szum(x,2)
	m[8] <- szum(x,1)
	m[9] <- szum(x,0)
	return(m)

}

a1MatrixCreator <- function(x,y)
{
	m <- matrix(, nrow = 3, ncol = 3)
	m[1] <- szum(x,4)
	m[2] <- szum(x,3)
	m[3] <- szum(x,2)
	m[4] <- szum2(x,y,2)
	m[5] <- szum2(x,y,1)
	m[6] <- szum2(x,y,0)
	m[7] <- szum(x,2)
	m[8] <- szum(x,1)
	m[9] <- szum(x,0)
	return(m)

}

a0MatrixCreator <- function(x,y)
{
	m <- matrix(, nrow = 3, ncol = 3)
	m[1] <- szum(x,4)
	m[2] <- szum(x,3)
	m[3] <- szum(x,2)
	m[4] <- szum(x,3)
	m[5] <- szum(x,2)
	m[6] <- szum(x,1)
	m[7] <- szum2(x,y,2)
	m[8] <- szum2(x,y,1)
	m[9] <- szum2(x,y,0)
	return(m)

}

secondaryMatrixCreator <- function(x)
{
	m <- matrix(, nrow = 3, ncol = 3)
	m[1] <- szum(x,4)
	m[2] <- szum(x,3)
	m[3] <- szum(x,2)
	m[4] <- szum(x,3)
	m[5] <- szum(x,2)
	m[6] <- szum(x,1)
	m[7] <- szum(x,2)
	m[8] <- szum(x,1)
	m[9] <- szum(x,0)
	return(m)

}

 
#kell az a2 a1 a0
#y legyen 5x^2 + 3x + 7

x<- c(1,2,3,4)
y<- 5*x^2+3*x+7
linreg <- function(x,y){
    a2m <- matrixdeter(a2MatrixCreator(x,y)) / matrixdeter(secondaryMatrixCreator(x))
    a1m <- matrixdeter(a1MatrixCreator(x,y)) / matrixdeter(secondaryMatrixCreator(x))
    a0m <- matrixdeter(a0MatrixCreator(x,y)) / matrixdeter(secondaryMatrixCreator(x))

    print(a2m)
    print(a1m)
    print(a0m)

	plot(x,y,"b")
}

linreg(x,y)