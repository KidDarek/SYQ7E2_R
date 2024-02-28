szum <- function(a){
    s <- 0
    n <- length(a)
    for(i in 1:n){
        s <- s + a[i]
    }
    return(s)
}
 
 
szum2 <- function(a,b){
    s <- 0
    n <- length(a)
    for(i in 1:n){
        s <- s + a[i]* b[i]
    }
    return(s)
}
 
deter <- function(a,b,c,d){
    return(a*d - b*c)
}
 
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