rm(list = ls())
lcg_custom <- function(seed, a, c, m, n = 10) {
    x <- vector(length = n)
    x[1] <- seed
    for (i in 2:n) {
        x[i] <- (a * x[i-1] + c) %% m
    }
    x
}
lcg_custom(5, 2, 6, 7)

#pada interval 0,1
lcg_cont <- function(seed, a, c, m, n=10) {
    x <- vector(length = n)
    x[1] <- seed
    for (i in 2:n) {
        val <- (a * x[i-1] + c) %% m
        x[i] <- val/m
    }
    x
}

test_generate <- function(data, m = 6) {
    
}
#number of test
m <- 6
ydat<-matrix(0,m,5)
#set column 2 as mean theoritic
ydat[,2]<-10
#set column 3 as var theoritic
ydat[,3]<-5
#set col-1 to var size, col-4 mean, col-5 var
for(i in 1:m){
    n<-10*(i+4)
    ydat[i,1]<-n
    y<-rnorm(n,10,sqrt(5))
    ydat[i,4]<-mean(y)
    ydat[i,5]<-var(y)
}
plot(ydat[,1],ydat[,4],type='b',xlab='n',ylab='ragam dan
     mean',ylim=c(3,12))

lines(ydat[,1],ydat[,2])

points(ydat[,1],ydat[,5])
lines(ydat[,1],ydat[,5])
lines(ydat[,1],ydat[,3])

m<-6
#par set mar=margin of vector length 4, mfrow=multi panel plot
par(mfrow=c(2,3),mar=rep(3,4))
ydat<-matrix(0,m,5)
for(i in 1:m){
    n<-10*(i+4)
    ydat[i,1]<-n
    y<-rnorm(n,10,sqrt(5))
    ydat[i,4]<-mean(y)
    ydat[i,5]<-var(y)
    xd<-seq(min(y),max(y),0.1)
    #create cumulative from xd
    yd<-dnorm(xd,10,sqrt(5))
    plot(density(y))
    #plot line from theoritical normal
    lines(xd,yd)
}