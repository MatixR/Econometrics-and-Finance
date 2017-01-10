# option price simulation
option_par<-read.csv("./optionsdata.csv",head=T)

# stock price at time T under GBM
S_T<-function(S0,r,sigma,T,z) {
  ST <- S0*exp((r-sigma^2/2)*T+z*sigma*sqrt(T));
  ST
}

# get par for BS
N=length(option_par[,1])
S0 <-option_par[,c("S0")]
r <-option_par[,c("r")]
sigma <-option_par[,c("sigma")]
T <-option_par[,c("T")]
K <- option_par[,c("K")]

# simulate stock price paths
z<-rnorm(10000)
sim_opr<-vector(mode = "numeric", length = 10)

for (i in 1:N) {
  sim_ST <- sapply(z,
                   function(t) {
                     S_T(S0[i],r[i],sigma[i],T[i],t)
                   })

  # mean of option prices for each path
sim_opr[i] <- mean(exp(-r[i]*T[i])*pmax(0,sim_ST-rep(K[i],10000)))
}
sim_opr

# compare with closed form solution
myBSPrice <- function(s0,K,r,sigma,T) {
  d1 <- (log(s0/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- (log(s0/K)+(r-sigma^2/2)*T)/(sigma*sqrt(T))
   call_price <- s0*pnorm(d1)-exp(-r*T)*K*pnorm(d2)
  
  call_price
}

BS_opr<-vector(mode = "numeric", length = 10)
for (i in 1:N) {
  BS_opr[i] <- myBSPrice(S0[i],K[i],r[i],sigma[i],T[i])
}
BS_opr
