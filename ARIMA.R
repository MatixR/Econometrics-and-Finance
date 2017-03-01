#load the data
PPI<-read.csv("./PPIFGS.csv",head=TRUE)
PPI$DATE<-as.Date(as.character(PPI$DATE),"%m/%d/%Y")
# calc different versions
diff_PPI<-diff(PPI$PPI)
logPPI<-log(PPI$PPI)
diff_logPPI<-diff(logPPI)
PPI4<-cbind(PPI[-1,],diff_PPI,logPPI=logPPI[-1],diff_logPPI)
# graph with four subplots
library(ggplot2)
library(grid)
library(gridExtra)
source("C:/Users/Cathy/Documents/R/multiplot.R")
p1<-qplot(PPI4[,1], PPI4[,2],xlab = "Date",ylab="PPI",geom="line")+geom_line(color="blue",size=0.1)
p2 <- qplot(PPI4[,1], PPI4[,3],xlab = "Date",ylab="diff_PPI",geom="line")+geom_line(color="blue",size=0.1)
p3<-qplot(PPI4[,1], PPI4[,4],xlab = "Date",ylab="log(PPI)",geom="line")+geom_line(color="blue",size=0.1)
p4 <- qplot(PPI4[,1], PPI4[,5],xlab = "Date",ylab="diff_log(PPI)",geom="line")+geom_line(color="blue",size=0.1)

plots<-list(p1,p2,p3,p4)
layout <- matrix(1:4, nrow = 2, byrow = TRUE)
multiplot(plotlist = plots, layout = layout)

library(zoo)
library(xts)
yt<-xts(diff_logPPI,order.by=PPI[-1,1])
acf(yt,lag.max=12)
pacf(yt,lag.max=12)

#Specify different models
library(pander)
arma1<-arima(yt,order=c(3,0,0))
arma2<-arima(yt,order=c(0,0,3))
arma3<-arima(yt,order=c(3,0,3))
arma4<-arima(yt,order=c(3,0,6))

pander(arma1,digits = 2)
cat("\n")
pander(arma2,digits = 2)
cat("\n")
pander(arma3,digits = 2)
cat("\n")
pander(arma4,digits = 2,split.cells = 2)

# Plot the residuls from different models
library(ggfortify)
q1<-autoplot(as.zoo(arma1$residuals),geom="line")+
  labs(x = "Date", y = "AR(3)")
q2<-autoplot(acf(arma1$residuals,plot=FALSE))

q3<-autoplot(as.zoo(arma2$residuals),geom="line")+
  labs(x = "Date", y = "MA(3)")
q4<-autoplot(acf(arma2$residuals,plot=FALSE))

q5<-autoplot(as.zoo(arma3$residuals),geom="line")+
  labs(x = "Date", y = "ARMA(3,3)")
q6<-autoplot(acf(arma3$residuals,plot=FALSE))

q7<-autoplot(as.zoo(arma4$residuals),geom="line")+
  labs(x = "Date", y = "ARMA(3,6)")
q8<-autoplot(acf(arma4$residuals,plot=FALSE))

qplots<-list(q1,q2,q3,q4,q5,q6,q7,q8)
qlayout <- matrix(1:8, nrow = 4, byrow = TRUE)
multiplot(plotlist = qplots, layout = qlayout)

# test for different models
arma1_test<-data.frame(Qstatistic_8=
                         Box.test(arma1$residuals,lag=8,type="Ljung-Box",fitdf=3)$statistic,
                       Qstatistic_12=
                         Box.test(arma1$residuals,lag=12,type="Ljung-Box",fitdf=3)$statistic,
                       AIC=AIC(arma1),BIC=BIC(arma1))

arma2_test<-data.frame(Qstatistic_8=
                         Box.test(arma2$residuals,lag=8,type="Ljung-Box",fitdf=3)$statistic,
                       Qstatistic_12=
                         Box.test(arma2$residuals,lag=12,type="Ljung-Box",fitdf=3)$statistic,
                       AIC=AIC(arma2),BIC=BIC(arma2))
arma3_test<-data.frame(Qstatistic_8=
                         Box.test(arma3$residuals,lag=8,type="Ljung-Box",fitdf=6)$statistic,
                       Qstatistic_12=
                         Box.test(arma3$residuals,lag=12,type="Ljung-Box",fitdf=6)$statistic,
                       AIC=AIC(arma3),BIC=BIC(arma3))
arma4_test<-data.frame(Qstatistic_8=
                         Box.test(arma4$residuals,lag=8,type="Ljung-Box",fitdf=9)$statistic,
                       Qstatistic_12=
                         Box.test(arma4$residuals,lag=12,type="Ljung-Box",fitdf=9)$statistic,
                       AIC=AIC(arma4),BIC=BIC(arma4))
arma_test<-rbind(arma1_test,arma2_test,arma3_test,arma4_test)
row.names(arma_test)<-c("ARMA(3,0)","ARMA(0,3)","ARMA(3,3)","ARMA(3,6)")
pander(arma_test)

#Use data before 2005 to estimate
yt_est<-yt[which(time(yt)<as.Date("2005-12-31"))]
arma1_est<-arima(yt_est,order=c(3,0,0))
arma2_est<-arima(yt_est,order=c(0,0,3))
arma3_est<-arima(yt_est,order=c(3,0,3))
arma4_est<-arima(yt_est,order=c(3,0,6))

# use 4 models to forecast
n=length(yt)-length(yt_est)
arma1_for<-predict(arma1_est, n.ahead = n)$pred
arma2_for<-predict(arma2_est, n.ahead = n)$pred
arma3_for<-predict(arma3_est, n.ahead = n)$pred
arma4_for<-predict(arma4_est, n.ahead = n)$pred 

# calc the mean of squared prediction error
yt_act<-yt[which(time(yt)>as.Date("2005-12-31"))]
arma1_MSPE<-1/n*sum((arma1_for-yt_act)^2)
arma2_MSPE<-1/n*sum((arma2_for-yt_act)^2)
arma3_MSPE<-1/n*sum((arma3_for-yt_act)^2)
arma4_MSPE<-1/n*sum((arma4_for-yt_act)^2)
# assuming there is no predictability in yt
rw_for<-lag(yt,lag=1)
rw_for<-rw_for[which(time(rw_for)>as.Date("2005-12-31"))]
rw_MSPE<-1/n*sum((rw_for-yt_act)^2)

arma_MSPE<-data.frame(arma1=arma1_MSPE,arma2=arma2_MSPE,
                      arma3=arma3_MSPE,arma4=arma4_MSPE,
                      rw=rw_MSPE)
pander(arma_MSPE,caption="Mean of squared prediction error")
