custom_chisq <- function(m, n) {
    y <- vector(length = 2*n)
    for(i in 1:m){
        u1<-runif(n,0,1)
        u2<-runif(n,0,1)
        z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
        z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
        z<-c(z1,z2)
        x<-z^2
        y<-y+x
    }
    y
}

custom_f <- function(v1, v2, n) {
    X1 <- custom_chisq(v1,n)
    X2 <- custom_chisq(v2,n)
    (X1/v1)/(X2/v2)
}
y <- custom_f(4,8,1000)

integral_mc <- function(formula, n, a, b) {
    x <- runif(n,a,b)
    y <- mean(formula(x))
    (b-a) * y
}

integral_mc(function(x) x^2, 500000, 0, 2)