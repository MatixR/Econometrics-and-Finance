# Calculate implied volatility

# option price data
option_price<-read.csv("D:/MFE/Courses/R wrokshop/week4/Lab/OptionPrices.csv",head=T)

# Parameters for BS model
T <- 30/252
r <- .01
sigma <- .25
s0 <- 204.24
K_v <- option_price[,c("Strike")]
actual_price_v<- option_price[,c("Price")]
N=nrow(option_price)

# calc implied vol using BS equation
implied_vol<-vector(mode = "numeric", length = N)
for (i in 1:N) {
K<-K_v[i]
actual_price<-actual_price_v[i]
  f <- function(sigma){
    d1 <- (log(s0/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
    d2 <- (log(s0/K)+(r-sigma^2/2)*T)/(sigma*sqrt(T))
    
    call_price <- s0*pnorm(d1)-exp(-r*T)*K*pnorm(d2)
    
    diff_price <- call_price - actual_price
  }
  
  implied_vol[i] <- uniroot(f,c(0,10))
}

implied_vol=as.vector(implied_vol)

# Test case Yahoo
YahooVol<-option_price[,c("YahooVol")]

par(mfrow = c(2, 1))
plot(K_v,YahooVol)
plot(K_v,implied_vol)