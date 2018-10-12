#use plot(density(y)) or plot(ecdf(y))

#uniform - normal std
n <- 100
u1<-runif(n,0,1)
u2<-runif(n,0,1)
z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
z<-c(z1,z2)
plot(density(z),xlab='x',ylab='p')
x<-seq(min(z),max(z),0.1)
y<-dnorm(x,0,1)
lines(x,y)

#normal std - normal
sgm <- 5
mu <- 10
u1<-runif(n,0,1)
u2<-runif(n,0,1)
z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
z<-c(z1,z2)
x<-sgm*z+mu

#normal std - chisq(1)
u1<-runif(n,0,1)
u2<-runif(n,0,1)
z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
z<-c(z1,z2)
y<-z^2

#chisq(1) - chisq(m)
n<-100
m<-10
y<-vector(length = 2*n)
for(i in 1:m){
    u1<-runif(n,0,1)
    u2<-runif(n,0,1)
    z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
    z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
    z<-c(z1,z2)
    x<-z^2
    y<-y+x
}

#uniform - exp(lambda)
lambda <- 2
x<-runif(n,0,1)
y<-1/lambda*-log(x)

#uniform - gamma(r,lambda)
n <- 1000
lambda <- 2
r <- 10
y<-vector(length = n)
for(i in 1:r) {
    x<-runif(n,0,1)
    x<-1/lambda*-log(x)
    y <- y+x
}

#bivariat normal
n<-1000
mux<-30
muy<-40
sdx<-4
sdy<-5
r<-0.5
dist_mat<-matrix(0,n,2)
u1<-runif(n,0,1)
u2<-runif(n,0,1)
dist_mat[,1]<-mux+sdx*u1
dist_mat[,2]<-muy+r*sdy*u1+sdy*sqrt(1-r^2)*u2
#plot(dist_mat[,1], dist_mat[,2], type = 'p')

#menggunakan cholesky decomposition
bvnorm <- function(mu,sigma){
    r <- length(mu)
    l_cho <- t(chol(sigma)) 
    z <- rnorm(r)
    return(l_cho %*% z + mu)
}
x <- matrix(0,nrow=2,ncol=1000)
for(i in 1:1000){
    x[,i] = bvnorm(c(0,0),rbind(c(1.0, 0.9),c(0.9,1.0)))
}