
#density plots and skewness test

N <- 5000
x <- rnorm(N)

hist(x,
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1,
     col='lightblue', xlab=' ', ylab=' ', axes=F,
     main='Normal')
lines(density(x,bw=1), col='red', lwd=3)



N <- 5000
x <- rnbinom(N, 100, .9)

hist(x,
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1,
     col='lightblue', xlab=' ', ylab=' ', axes=F,
     main='Left Skewed')
lines(density(x,bw=1), col='red', lwd=3)

N <- 5000
  n <- 500
p <- .99
x <- rbinom(N,n,p)
hist(x,
     xlim = c(min(x), max(x)),
     probability = TRUE,
     nclass = max(x) - min(x) + 1,
     col = 'lightblue',
     main = 'Right Skewed')
lines(density(x,bw=1), col = 'red', lwd = 3)


shapiro.test(x)
