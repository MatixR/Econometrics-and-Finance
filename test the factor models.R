#load the data
Portfolio25<-read.csv("./Portfolios25.csv",head=TRUE)
FFfactors<-read.csv("./F-F_Research_Data_5_Factors.csv",head=TRUE)

# calc excess return for each portfolio
Portfolio_FF<-merge(Portfolio25,FFfactors,by="X")
Portfolio_FF[, c(2:26)]<-Portfolio_FF[, c(2:26)] - Portfolio_FF[, 32]

#melt the data and regress each portfolio excess return on factors
library(reshape2)
Portfolio_FF_reg <- melt(Portfolio_FF, id.vars=
                           c("X","Mkt.RF","SMB","HML","RMW","CMA","RF"), 
                         value.name="pf.RF")

# get alpha and residuals from time series regression 
library(plyr)
reg_alpha<-dlply(Portfolio_FF_reg , .(variable), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);summary(out)$coef[1]})
alpha<-as.matrix(as.data.frame(reg_alpha))

reg_resid<-dlply(Portfolio_FF_reg , .(variable), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);
resid(out)})
resid<-as.data.frame(reg_resid)

# chi squared test
T<-length(Portfolio_FF[,1])
Factors.mean<-as.matrix(colMeans(FFfactors[,2:6]))
Factors.cov<-cov(FFfactors[,2:6])
sigma<-cov(resid)
chi.squared<-T*(1+t(Factors.mean)%*%solve(Factors.cov)%*%
                  Factors.mean)^(-1)*(alpha%*%solve(sigma)%*%t(alpha))
chi.test<-data.frame(test.statistic=chi.squared,
                     critial.value=qchisq(.95, df=25),
                     p.value=pchisq(chi.squared, df=25,lower.tail = FALSE),
                     row.names="chi-squared test")

# exact F-test
F<-(T-25-5)/25*(1+t(Factors.mean)%*%solve(Factors.cov)%*%
                  Factors.mean)^(-1)* (alpha%*%solve(sigma)%*%t(alpha))
F.test<-data.frame(test.statistic=F,critial.value=qf(.95, 25,T-25-5),
                   p.value=pf(F, 25,T-25-5,lower.tail=FALSE),
                   row.names="exact F test")

# print test results
Test<-rbind(chi.test,F.test)
library(pander)
pander(Test,digits = 4, 
       caption = 'Test H0: alphas are jointly zero ')

#2
#cross-sectional OLS regressions
# estimate beta from time series regression
reg_beta<-dlply(Portfolio_FF_reg , .(variable), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);summary(out)$coef[2:6]})
beta<-t(as.data.frame(reg_beta))
colnames(beta)<-names(FFfactors[,2:6])

Portfolio.mean<-data.frame(Portfolio=apply(Portfolio25[,-1],2,mean))
Cross_reg<-cbind(Portfolio.mean,beta)
Cross_out<-lm(Portfolio~Mkt.RF+SMB+HML+RMW+CMA, data=Cross_reg)
Cross_riskprice<-Cross_out$coefficients

#Fama-MacBeth regressions
#merge the data and run cross sectional regression at ach time period
beta_reg<-data.frame(beta,variable=names(beta[,1]))
FamaMacBeth_reg <- merge(Portfolio_FF_reg[,c("X","variable","pf.RF")], 
                         beta_reg, by = "variable",all.x = TRUE)

FMB_out<-t(as.data.frame(dlply(FamaMacBeth_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);out$coefficients})))
FMB_riskprice<-apply(FMB_out,2,mean)

# cross setional standard errors
#no intercept
#riskprice_cov<-1/T*(solve(t(beta)%*%beta)%*%t(beta)%*%sigma%*%beta%*%
#                     solve(t(beta)%*%beta)+Factors.cov)
#riskprice_cov.Shanken<-1/T*(ShankenF*solve(t(beta)%*%beta)%*%
#                     (t(beta)%*%sigma%*%beta)%*%solve(t(beta)%*%beta)+Factors.cov)

# include intercept
x<-cbind(rep(1,25),beta)
Factors.cov1<-cbind(rep(0,6),rbind(rep(0,5),Factors.cov))
riskprice_cov.CS<-1/T*((solve(t(x)%*%x)%*%t(x)%*%sigma%*%x%*%
                          solve(t(x)%*%x))+Factors.cov1)
riskprice_sd.CS<-sqrt(diag(riskprice_cov.CS))

#Shanken correction
ShankenF<-as.numeric(1+t(Factors.mean)%*%solve(Factors.cov)%*% Factors.mean)
riskprice_cov.Shanken<-1/T*((ShankenF*solve(t(x)%*%x)%*%
                               (t(x)%*%sigma%*%x)%*%solve(t(x)%*%x))+Factors.cov1)
riskprice_sd.Shanken<-sqrt(diag(riskprice_cov.Shanken))

# FamaMacBeth standard errors
riskprice_sd.FMB<-1/sqrt(T)*apply(FMB_out,2,sd)

# compare the estimated risk price for two method
riskprice<-t(data.frame(Estimated.CS=Cross_riskprice,
                        StdEerror.CS=riskprice_sd.CS,
                        StdEerror.CS.Shanken= riskprice_sd.Shanken,
                        Estimated.FamaMacBeth=FMB_riskprice,
                        StdEerror.FBM=riskprice_sd.FMB))

pander(riskprice,digits = 3, emphasize.rows = c(1,4),
       caption = 'Lambda estimates of Cross sectional regression and Fama-MacBeth  ')

#crosectional r.squared
r.squared.CS<-data.frame(CS.rsquared=summary(Cross_out)$r.squared)
pander(r.squared.CS,digits = 4, 
       caption = 'Cross-sectional r squared')

# test H0:alphas=0
Cross_alpha<-Cross_out$residuals
alpha_var.CS<-1/T*ShankenF*((diag(x = 1, 25, 25)-x%*%solve(t(x)%*%x)%*%t(x))%*%
                              sigma%*%((diag(x = 1, 25, 25)-x%*%solve(t(x)%*%x)%*%t(x))))
chi.squared.CS<-t(Cross_alpha)%*%solve(alpha_var.CS,tol = 1e-19)%*%Cross_alpha
chi.test.CS<-data.frame(test.statistic=chi.squared.CS,critial.value=qchisq(.95, df=25-5-1),
                        p.value=pchisq(chi.squared.CS, df=25-5-1,lower.tail = FALSE),row.names="OLS")

FMB_alpha<-t(as.data.frame(dlply(FamaMacBeth_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);out$residuals})))
FMB_alpha_mean<-apply(FMB_alpha,2,mean)
alpha_var.FMB<-1/T*apply(FMB_alpha,2,var)
chi.squared.FMB<-sum(FMB_alpha_mean^2/alpha_var.FMB)
chi.test.FMB<-data.frame(test.statistic=chi.squared.FMB,critial.value=qchisq(.95, df=25-5-1),
                         p.value=pchisq(chi.squared.FMB, df=25-5-1,lower.tail = FALSE),row.names="Fama-MacBeth")

Test2<-rbind(chi.test.CS,chi.test.FMB)
pander(Test2,digits = 4, 
       caption = 'Test H0: alphas are zero by OLS and Fama-MacBeth')

#3
#load the 30 portfolios data
Portfolio30<-read.csv("./Portfolios30.csv",head=TRUE)

# calc excess return for each portfolio
Portfolio30_FF<-merge(Portfolio30,FFfactors,by="X")
Portfolio30_FF[, c(2:31)]<-Portfolio30_FF[, c(2:31)] - Portfolio30_FF[, 37]

#melt the data and regress each portfolio excess return on factors
Portfolio30_FF_reg <- melt(Portfolio30_FF, id.vars=c("X","Mkt.RF","SMB","HML","RMW","CMA","RF"), 
                           value.name="pf.RF")

reg30_beta<-dlply(Portfolio30_FF_reg , .(variable), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);summary(out)$coef[2:6]})
beta30<-t(as.data.frame(reg30_beta))
colnames(beta30)<-names(FFfactors[,2:6])

#Fama-MacBeth regressions
beta30_reg<-data.frame(beta30,variable=names(beta30[,1]))
FamaMacBeth30_reg <- merge(Portfolio30_FF_reg[,c("X","variable","pf.RF")], 
                           beta30_reg, by = "variable",all.x = TRUE)

FMB30_out<-t(as.data.frame(dlply(FamaMacBeth30_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);out$coefficients})))
FMB30_riskprice<-apply(FMB30_out,2,mean)

riskprice_sd30.FMB<-1/sqrt(T)*apply(FMB30_out,2,sd)
FMB30.com<-t(data.frame(Estimated.coef=FMB30_riskprice,StdError=riskprice_sd30.FMB))
pander(FMB30.com,digits = 4, 
       caption = ' Results of Fama-MacBeth cross-sectional regressions')

#crosectional r.squared
Portfolio30.mean<-data.frame(Portfolio=apply(Portfolio30[,-1],2,mean))
Cross30_reg<-cbind(Portfolio30.mean,beta30)
Cross30_out<-lm(Portfolio~Mkt.RF+SMB+HML+RMW+CMA, data=Cross30_reg)
r.squared30.CS<-data.frame(CS.rsquared=summary(Cross30_out)$r.squared)
pander(r.squared30.CS,digits = 4, 
       caption = 'Cross-sectional r squared')

# test H0:alpha=0
FMB30_alpha<-t(as.data.frame(dlply(FamaMacBeth30_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ Mkt.RF+SMB+HML+RMW+CMA, data=df);out$residuals})))
FMB30_alpha_mean<-apply(FMB30_alpha,2,mean)
alpha_var.FMB30<-1/T*apply(FMB30_alpha,2,var)
chi.squared.FMB30<-sum(FMB30_alpha_mean^2/alpha_var.FMB30)
chi.test.FMB30<-data.frame(test.statistic=chi.squared.FMB30,
                           critial.value=qchisq(.95, df=30-1-1), p.value=pchisq(chi.squared.FMB30,
                                                                                df=30-1-1,lower.tail = FALSE),row.names="Test alpha")

pander(chi.test.FMB30,digits = 4, 
       caption = 'Test H0: alphas are zero by Fama-MacBeth')

# test CAPM
#time series regression on market factor
reg_Mkt<-dlply(Portfolio30_FF_reg , .(variable), function(df) 
{out<-lm(pf.RF ~ Mkt.RF, data=df);summary(out)$coef[2]})
beta_Mkt<-t(as.data.frame(reg_Mkt))
colnames(beta_Mkt)<-"beta"

#Fama-MacBeth regressions
beta_Mkt_reg<-data.frame(beta_Mkt,variable=names(beta_Mkt[,1]))
FamaMacBeth_Mkt_reg <- merge(Portfolio30_FF_reg[,c("X","variable","pf.RF")], 
                             beta_Mkt_reg, by = "variable",all.x = TRUE)
# forcasting regression without intercept
FMB_Mkt_out<-t(as.data.frame(dlply(FamaMacBeth_Mkt_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ lag(beta)-1, data=df);out$coefficients})))
FMB_Mkt<-apply(FMB_Mkt_out,2,mean)
Mkt_sd<-1/sqrt(T)*apply(FMB_Mkt_out,2,sd)

# forcasting regression with intercept
FMB_Mkt.int_out<-t(as.data.frame(dlply(FamaMacBeth_Mkt_reg  , .(X), function(df) 
{out<-lm(pf.RF ~ lag(beta), data=df);out$coefficients})))
FMB_Mkt.int<-apply(FMB_Mkt.int_out,2,mean)
Mkt.int_sd<-1/sqrt(T)*apply(FMB_Mkt.int_out,2,sd)

Mkt.com<-t(data.frame(Estimated.coef=FMB_Mkt,StdError=Mkt_sd))
pander(Mkt.com,digits = 4, 
       caption = ' Fama-MacBeth regressions without intercept')
Mkt.int.com<-t(data.frame(Estimated.coef=FMB_Mkt.int,StdError=Mkt.int_sd))
pander(Mkt.int.com,digits = 4, 
       caption = ' Fama-MacBeth regressions with intercept')

# plot 3 time-series
data.plot<-cbind(FFfactors[,1:2],FMB_Mkt_out,FMB_Mkt.int_out[,2])
colnames(data.plot)<-c("Date","Mkt","Est.noint","Est.int")
library(zoo)
data.plot[,1]<-as.Date(as.yearmon(as.character(data.plot[,1]),"%Y%m"))
library(ggplot2)
ggplot(data.plot, aes(Date)) + 
  geom_line(aes(y = Mkt, colour = "Mkt")) + 
  geom_line(aes(y = Est.noint, colour = "Est.noint"))+
  geom_line(aes(y = Est.int, colour = "Est.int"))+
  ylab("lambda estimates") +
  ggtitle("Portfolio returns from different specifications and excess market returns")

risk.Mkt_cor<-cor(data.plot[,2:4])
pander(risk.Mkt_cor,digits = 4, 
       caption = ' correlations between 3 time series')