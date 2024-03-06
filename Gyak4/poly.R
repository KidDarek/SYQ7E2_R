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
    return(m[1]*m[4] - m[2]*m[3])
}

matrixdeter <- fuction(m){
	a1<-m[1]
	a2<-m[2]
	a3<-m[3]
	m1<- m[-c(1), -c(1)]
	m2<- m[-c(1), -c(2)]
	m3<- m[-c(1), -c(3)]
	s<- a1*deter(m1) - a2*deter(m2) +a3*deter(m3)
	return(s)
}
 
#kell az a2 a1 a0
linreg <- function(x,y){
    n <- length(x)
    sx <- szum(x)
    sxy <- szum2(x,y)
    sy <- szum(y)
    sxx <- szum2(x,x)
 
    b <- deter(sx,sxy,sx,sy) / deter(sxx,-sx,sx,n)
    a <- deter(sxy,-sx,sy,n) / deter(sxx,-sx,sx,n)
    print(a)
    print(b)
	px<- c(0,1)
	py<- (a*px+b)
	print(py)

 	plot(px,py,"l")
	par(new=TRUE)
 	plot(x,y)
	return(c(a,b))
}
a<- 2
b<- 3
n<- 4 
x <- seq(1,4,0.01)
    y <- a*exp(b*x)
	lny<- log(y)
result<- linreg(x,lny)
px<- seq(1,4,0.05)
py<- result[1]*exp(result[2]*px)
plot(-px,py,"l")
par(new=TRUE)
plot(x,y)