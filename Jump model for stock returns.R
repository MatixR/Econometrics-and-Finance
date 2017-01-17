
# Model!: normal distribution returns (no jumps)
# generated 600 observations from normal distribution
n=600
r_sim<-data.frame(t=1:600,r=rnorm(n, mean = 0.008, sd = 0.063))

# plot the simulated series
library(ggplot2)
ggplot(r_sim,aes(x=t,y=r)) + geom_line(colour="blue") +
  geom_hline(yintercept=c(0.008+4*0.063,0.008-4*0.063))+
  ggtitle("simulated returns")+
  theme(plot.title = element_text(size=10, face="bold"))

# plot the histogram of simulated returns and check the tail characteristics
ggplot(r_sim,aes(r)) + geom_histogram(aes(y =..density..),binwidth = 0.01)+
  geom_density(col="blue")  +
  labs(title="Histogram for simulated returns") +
  labs(x="returns", y="density")+
  theme(plot.title = element_text(size=10, face="bold"))


# The distribution of stock returns has fat tails. As a result, we need models that 
# deliver fat-tailed distributions (e.g., to price options on stocks). One way of
# doing this is to introduce jumps in the model.

# Consider a Bernoulli-distributed variable:Bt=1 with probability p and 0 with (1-p)
# and two independent Standard Normal variables xt and zt. Define the jump as: jt = bt*zt
# The stock returns model becomes: rt=xt+jt*zt

# set the parameters
mu <- 0.012
sigma <- 0.05
p <- 0.15
mu_J <- -0.03
sigma_J <- 0.1

#moments of bt
bt_mu<-p
bt_var<-p*(1-p)
bt_2<-p
bt_3<-p
bt_4<-p
#moments of zt
zt_mu<-mu_J 
zt_var<-sigma_J^2
zt_2<-mu_J^2+zt_var
zt_3<-mu_J^3+3*mu_J*zt_var
zt_4<-mu_J^4+6*mu_J^2*zt_var+3*zt_var^2
#moments of xt
xt_mu<-mu 
xt_var<-sigma^2
xt_2<-mu^2+xt_var
xt_3<-mu^3+3*mu*xt_var
xt_4<-mu^4+6*mu^2*xt_var+3*xt_var^2
#moments of rt
rt_mu<- xt_mu+bt_mu*zt_mu
rt_var<-xt_var + bt_var*zt_var+bt_var*zt_mu^2+zt_var*bt_mu^2
rt_2<-rt_mu^2+rt_var
rt_3<- xt_3+3*xt_2*zt_mu*bt_mu+3*xt_mu*zt_2*bt_2+bt_3*zt_3
rt_4<-xt_4-4*xt_3*bt_mu*zt_mu-4*rt_mu*bt_3*zt_3+6*rt_2*bt_2*xt_2+bt_4*zt_4
rt_skew<-(rt_3-3*rt_mu*rt_var-rt_mu^3)/(rt_var)^(3/2)
rt_kur<-(rt_4-3*rt_mu^4-4*rt_3*rt_mu+6*rt_2*rt_mu^2)/(rt_var^2)

#generated 600 observations from jump model
zt <- rnorm(n, mean = mu_J, sd = sigma_J)
bt<-rbinom(n,1,p)
xt<-rnorm(n, mean = mu, sd = sigma)
r_jump_sim <- data.frame(t=1:600,r=xt + bt*zt)

# calc the moments from derived formulas and sample moments
library(moments)
f_moments <-data.frame(rt_mu,rt_var,rt_skew,rt_kur)
colnames(f_moments) <- c("mean","variance","skewness","kurtosis")
s_moments <-data.frame(mean(r_jump_sim$r),var(r_jump_sim$r),
                       skewness(r_jump_sim$r),kurtosis(r_jump_sim$r))
colnames(s_moments) <- c("mean","variance","skewness","kurtosis")
rt_moments<-rbind(f_moments,s_moments)
rownames(rt_moments) <- c("moments derived from formulas","sample moments")
rt_moments 

# plot the simulated jump series
ggplot(r_jump_sim,aes(x=t,y=r)) + geom_line(colour="blue") +
  geom_hline(yintercept=c(rt_mu+4*sqrt(rt_var),rt_mu-4*sqrt(rt_var)))+
  ggtitle("simulated returns")+
  theme(plot.title = element_text(size=10, face="bold"))

# plot the histogram of simulated returns from jump process and 
#check the tail characteristics
ggplot(r_jump_sim,aes(r)) + geom_histogram(aes(y =..density..),
                                           binwidth = 0.01)+
  geom_density(col="blue")+
  labs(title="Histogram for simulated returns") +
  labs(x="returns", y="density")+
  theme(plot.title = element_text(size=10, face="bold"))

