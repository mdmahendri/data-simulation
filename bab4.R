#unif - eksponensial
n <- 1000
beta <- 5
x <- runif(n,0,1)
y <- -beta * log(1-x)
teta<-1/beta #konversi ke teta untuk menguji
y1<-rexp(n,teta)