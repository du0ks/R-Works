#Autocorrelation function in R. (Time Series, for r1 and r2) 

AutoCorr <- function(vec){
xc <- vec - mean(vec)
denom <- sum(xcˆ2)
n <- length(vec)
r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
out <- list(r1 = r1, r2 = r2)
return(out)
}
