formate <- function(x) {
    format(x, digits = 3, nsmall = 3)
}

newton.raphson  <- function(func, dfunc, xinit, n, error) {
    x <- numeric(n)
    fx <- numeric(n)
    dfx <- numeric(n)
    abs.fx <- numeric(n)
    x[1] <- xinit
    
    cat('n', 'x', 'f(x)', 'df(x)', 'abs(f(x))', sep = '\t')
    cat('\n')
    for (i in 1:n) {
        fx[i] <- func(x[i])
        dfx[i] <- dfunc(x[i])
        abs.fx[i] <- abs(fx[i])
        cat(i, formate(x[i]), formate(fx[i]),
            formate(dfx[i]), formate(abs.fx[i]), sep = '\t')
        cat('\n')
        if (abs.fx[i] < error) break
        
        x[i+1] <- x[i] - (fx[i]/dfx[i])
    }
    cat('akar terletak di ', x[i], ' dengan nilai ', fx[i])
}

# SOAL.1
newton.raphson(
    function(x) x - exp(-x),
    function(x) 1 + exp(-x),
    0.5, 10, 0.0001
)

# SOAL.2
newton.raphson(
    function(x) x^3 + x^2 -3*x - 3,
    function(x) 3*x^2 + 2*x - 3,
    0.5, 10, 0.0001
)