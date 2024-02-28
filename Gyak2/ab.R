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
 
linreg <- function(){
    n <- 5000
    x <- rnorm(n)
    y <- x + rnorm(n)
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
}
 
linreg()