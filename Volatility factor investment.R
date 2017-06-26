#FinalExame_604884150

library(data.table)
library(dplyr)
library(zoo)


setwd("D:\\MFE\\Courses\\Quantitative Asset Management\\HW\\Final exam")

## stock return
CRSP_Stocks_Daily<-data.table(read.csv("./CRSP_Stocks_Daily.csv",
                                       header=T, stringsAsFactors = T))
CRSP_Stocks_Daily$date<-as.Date(as.character(CRSP_Stocks_Daily$date), "%Y%m%d")

#time period
CRSP_Stocks_Daily[, c("Year",'Month') := list(as.integer(year(date)),as.integer(month(date)))]
CRSP_Stocks_Daily<-CRSP_Stocks_Daily[(Year>=2013 & Year<=2015) | (Year==2012 & Month==12)] 

#exchange code
CRSP_Stocks_Daily <- CRSP_Stocks_Daily[ EXCHCD %in% c(1,2,3)]

## missing value
CRSP_Stocks_Daily$RET<-as.numeric(as.character(CRSP_Stocks_Daily$RET))
CRSP_Stocks_Daily$DLRET<-as.numeric(as.character(CRSP_Stocks_Daily$DLRET))
CRSP_Stocks_Daily[,RET:=ifelse(RET%in% c(-44,-55,-66,-77,-88,-99),NA,RET)]
CRSP_Stocks_Daily[,DLRET:=ifelse(RET%in% c(-55,-66,-88,-99),NA,DLRET)]
# delisting return
CRSP_Stocks_Daily[,Ret:=ifelse(is.na(DLRET),RET,ifelse(is.na(RET),
                                                       DLRET,(1+RET)*(1+DLRET)-1))]
CRSP_Stocks_Daily[,obs:=sum(!is.na(Ret)), by=c('Year','Month','PERMNO')]
CRSP_Stocks_Daily<-CRSP_Stocks_Daily[obs>17]


## Monthly stock data
CRSP_Stocks_Monthly<-data.table(read.csv("./CRSP_Stocks_Monthly.csv",
                                         header=T, stringsAsFactors = T))
CRSP_Stocks_Monthly$date<-as.Date(as.character(CRSP_Stocks_Monthly$date), "%Y%m%d")
CRSP_Stocks_Monthly[, c("Year",'Month') := list(as.integer(year(date)),as.integer(month(date)))]
CRSP_Stocks_Monthly<-CRSP_Stocks_Monthly[(Year>=2013 & Year<=2015) | (Year==2012 & Month==12)]
CRSP_Stocks_Monthly<- CRSP_Stocks_Monthly[ EXCHCD %in% c(1,2,3)]

## missing value
CRSP_Stocks_Monthly$RET<-as.numeric(as.character(CRSP_Stocks_Monthly$RET))
CRSP_Stocks_Monthly$DLRET<-as.numeric(as.character(CRSP_Stocks_Monthly$DLRET))
CRSP_Stocks_Monthly[,RET:=ifelse(RET%in% c(-44,-55,-66,-77,-88,-99),NA,RET)]
CRSP_Stocks_Monthly[,DLRET:=ifelse(RET%in% c(-55,-66,-88,-99),NA,DLRET)]
# delisting return
CRSP_Stocks_Monthly[,Ret:=ifelse(is.na(DLRET),RET,ifelse(is.na(RET),
                                                         DLRET,(1+RET)*(1+DLRET)-1))]
CRSP_Stocks_Monthly<-CRSP_Stocks_Monthly[(Year>=2013 & Year<=2015) | (Year==2012 & Month==12)]

## market cap
CRSP_Stocks_Monthly[,Mkt_Cap:=abs(PRC)*SHROUT/1000]
## maket value at the beginning of t
setorder(CRSP_Stocks_Monthly,PERMNO,date)
CRSP_Stocks_Monthly[,lag_Mkt_Cap := shift(Mkt_Cap),by='PERMNO']
CRSP_Stocks_Monthly<-CRSP_Stocks_Monthly[!is.na(lag_Mkt_Cap) &!is.na(Ret) ]

##total volatility
CRSP_Stocks_Daily <- CRSP_Stocks_Daily[,Vol:=sd(Ret),by=c("Year", "Month", "PERMNO")]

Vol_Factor<-unique(CRSP_Stocks_Daily [,c("Year", "Month", "PERMNO", "Vol")])
setorder(CRSP_Stocks_Daily,PERMNO,Year,Month)
Vol_Factor[,lag_vol:=shift(Vol), by=c("PERMNO")]

Vol_Factor<-Vol_Factor[!is.na(lag_vol)]

#vol decile
Vol_Factor[,Vol_decile:=cut(lag_vol,breaks=c(-Inf, 
                                             quantile(lag_vol, probs = seq(0.2,0.8,by = 0.2)), Inf), 
                            labels=1:5),by=.(Year,Month)]
Vol_Factor<-Vol_Factor[,.(Year, Month, PERMNO, Vol_decile)]

CRSP_Factor<-merge(CRSP_Stocks_Monthly, Vol_Factor, by=c("Year", "Month", "PERMNO"), all.x = T)
CRSP_Factor<-CRSP_Factor[!is.na(Vol_decile)]

CRSP_Factor[, log_lag_Mkt_Cap :=log(lag_Mkt_Cap)]
CRSP_Factor[, VOL_lag_Mkt_Cap :=sum(lag_Mkt_Cap,na.rm =TRUE),
            by = .(Year,Month,Vol_decile)]
CRSP_Factor[, Month_lag_Mkt_Cap :=sum(lag_Mkt_Cap,na.rm =TRUE),
            by = .(Year,Month)]

# calc vol decile return
CRSP_Factor[, VOL_Ret := sum(lag_Mkt_Cap*Ret/VOL_lag_Mkt_Cap,na.rm =TRUE),
            by=.(Year,Month,Vol_decile)]

CRSP_Factor[, Mkt.Share := VOL_lag_Mkt_Cap/Month_lag_Mkt_Cap]
CRSP_Factor[ , size:=mean(log_lag_Mkt_Cap), by=c("Year", "Month", "Vol_decile")]

Vol_Port<-unique(CRSP_Factor[,.(Year, Month, Vol_decile, VOL_Ret, Mkt.Share, size)])

# statistics of vol decile portfolios
VOL_Summary<- Vol_Port[ , .(Mean=round(mean(VOL_Ret,na.rm=T)*100,2),
                            Std.Dev.=round(sd(VOL_Ret,na.rm=T)*100,2),
                            Mkt.Share=paste(round(mean(Mkt.Share)*100,1),"%",sep=""),
                            size=round(mean(size),2)), by=c("Vol_decile")]

#FF factors
FF_Factors<-data.table(read.csv("./F-F_Research_Data_Factors_Monthly.csv",header=T,check.names = FALSE))
FF_Factors$date<-as.Date(as.yearmon(as.character(FF_Factors$date),"%Y%m"))
FF_Factors[,c(2:5)]<- FF_Factors[,c(2:5)]/100
FF_Factors[, c("Year",'Month') := list(as.integer(year(date)),as.integer(month(date)))]
FF_Factors<-FF_Factors[Year>=2013 & Year<=2015]

Vol_FF <- merge(Vol_Port, FF_Factors, by=c("Year", "Month"), all.x = TRUE)
#stock excess returns
Vol_FF[,Ret.RF:=VOL_Ret-RF]

Alpha_CAPM <- Vol_FF[,.( CAPM.alpha=round(coef(lm(Ret.RF~ Mkt.RF))[1]*100,digits=2)),
                     by=c("Vol_decile")]
Alpha_T_CAPM<- Vol_FF[, .(CAPM.t.stat=round(summary(
  lm(Ret.RF~ Mkt.RF))$coefficients[1,3],digits=2)),
  by=c("Vol_decile")]

Alpha_FF3 <- Vol_FF[, .( FF3.alpha=round(coef(lm(Ret.RF~ Mkt.RF+ SMB + HML))[1]*100,digits=2)),
                    by=c("Vol_decile")]

library(sandwich)
library(lmtest)
# Alpha_NW_FF3<-Vol_FF[,.(coeftest(lm(Ret.RF ~ Mkt.RF + SMB + HML), 
#                                           vcov. = NeweyWest)[1,3]), by=c("Vol_decile")]
Alpha_T_FF3<-Vol_FF[,.(FF3.t.stat=round(summary(
  lm(Ret.RF ~ Mkt.RF + SMB + HML))$coefficients[1,3],digits=2)), by=c("Vol_decile")]

VOL_Summary<-merge(VOL_Summary,Alpha_CAPM)
VOL_Summary<-merge(VOL_Summary,Alpha_T_CAPM)
VOL_Summary<-merge(VOL_Summary,Alpha_FF3)
VOL_Summary<-merge(VOL_Summary,Alpha_T_FF3)
f<-function(a,b) {paste(a,"[",b,"]", collapse = "\n")}
VOL_Summary[ ,CAPM.Alpha:=apply(VOL_Summary,1,function(x) f(x[6],x[7]))]
VOL_Summary[ ,FF3.Alpha:=apply(VOL_Summary,1,function(x) f(x[8],x[9]))]
setnames(VOL_Summary,"Vol_decile","Rank")
#report repplicate results
library(pander)
pander(VOL_Summary[,c(1:5,10,11)],caption="Panel A: Portfolios Sorted by Total Volatility")


#FF factors dailly
FF_Factors_Daily<-data.table(read.csv("./F-F_Research_Data_Factors_daily.csv",header=T,check.names = FALSE))
FF_Factors_Daily$date<-as.Date(as.character(FF_Factors_Daily$date),"%Y%m%d")
FF_Factors_Daily[,c(2:5)]<- FF_Factors_Daily[,c(2:5)]/100
FF_Factors_Daily[, c("Year",'Month') := list(as.integer(year(date)),as.integer(month(date)))]
FF_Factors_Daily<-FF_Factors_Daily[(Year>=2013 & Year<=2015) | (Year==2012 & Month==12),c(1:5)]

#idosyncratic volatility
CRSP_Factor_Daily<- merge(CRSP_Stocks_Daily, FF_Factors_Daily, by=c("date"), all.x=T)
CRSP_Factor_Daily[,Ret.RF:=Ret-RF]
IdoVol_Factor <- CRSP_Factor_Daily[,c(res.std = as.list(sd(resid(
  lm(Ret.RF ~ Mkt.RF + SMB + HML))))), by=c("Year", "Month", "PERMNO")]

setorder(IdoVol_Factor,PERMNO,Year,Month)
IdoVol_Factor [,lag_IdoVol:=shift(res.std ), by=c("PERMNO")]

IdoVol_Factor <-IdoVol_Factor [!is.na(lag_IdoVol)]
#idosyncratic vol decile
IdoVol_Factor[,IdoVol_decile:=cut(lag_IdoVol,breaks=c(-Inf, 
                                                      quantile(lag_IdoVol, probs = seq(0.2,0.8,by = 0.2)), Inf), 
                                  labels=1:5),by=.(Year,Month)]

CRSP_IdoFactor<-merge(CRSP_Stocks_Monthly, IdoVol_Factor, by=c("Year", "Month", "PERMNO"), 
                      all.x = T)
CRSP_IdoFactor<-CRSP_IdoFactor[!is.na(IdoVol_decile)]

CRSP_IdoFactor[, log_lag_Mkt_Cap :=log(lag_Mkt_Cap)]
CRSP_IdoFactor[, VOL_lag_Mkt_Cap :=sum(lag_Mkt_Cap,na.rm =TRUE),
               by = .(Year,Month,IdoVol_decile)]
CRSP_IdoFactor[, Month_lag_Mkt_Cap :=sum(lag_Mkt_Cap,na.rm =TRUE),
               by = .(Year,Month)]

# calc vol decile return
CRSP_IdoFactor[, Ido_Ret := sum(lag_Mkt_Cap*Ret/VOL_lag_Mkt_Cap,na.rm =TRUE),
               by=.(Year,Month,IdoVol_decile)]

CRSP_IdoFactor[, Mkt.Share := VOL_lag_Mkt_Cap/Month_lag_Mkt_Cap]
CRSP_IdoFactor[ , size:=mean(log_lag_Mkt_Cap), by=c("Year", "Month", "IdoVol_decile")]

IdoVol_Port<-unique(CRSP_IdoFactor[,.(Year, Month, IdoVol_decile, Ido_Ret, Mkt.Share, size)])

# statistics of vol decile portfolios
Ido_Summary<- IdoVol_Port[ , .(Mean=round(mean(Ido_Ret,na.rm=T)*100,2),
                               Std.Dev.=round(sd(Ido_Ret,na.rm=T)*100,2),
                               Mkt.Share=paste(round(mean(Mkt.Share)*100,1),"%",sep=""),
                               size=round(mean(size),2)), by=c("IdoVol_decile")]

IdoVol_FF <- merge(IdoVol_Port, FF_Factors, by=c("Year", "Month"), all.x = TRUE)
# excess returns
IdoVol_FF[,Ret.RF:=Ido_Ret-RF]

Ido_Alpha_CAPM <- IdoVol_FF[, .(CAPM.alpha=round(coef(lm(Ret.RF~ Mkt.RF))[1]*100,digits=2)),
                            by=c("IdoVol_decile")]
Ido_Alpha_T_CAPM<- IdoVol_FF[, .(CAPM.t.stat=round(coeftest(
  lm(Ret.RF~ Mkt.RF),vcov. = NeweyWest)[1,3],2)),
  by=c("IdoVol_decile")]

Ido_Alpha_FF3 <- IdoVol_FF[, .(FF3.alpha=round(coef(
  lm(Ret.RF~ Mkt.RF+ SMB + HML))[1]*100,digits=2)),
  by=c("IdoVol_decile")]

Ido_Alpha_T_FF3<-IdoVol_FF[,.(FF3.t.stat=round(coeftest(lm(Ret.RF ~ Mkt.RF + SMB + HML), 
                                                        vcov. = NeweyWest)[1,3],2)), by=c("IdoVol_decile")]

Ido_Summary<-merge(Ido_Summary,Ido_Alpha_CAPM)
Ido_Summary<-merge(Ido_Summary,Ido_Alpha_T_CAPM)
Ido_Summary<-merge(Ido_Summary,Ido_Alpha_FF3)
Ido_Summary<-merge(Ido_Summary,Ido_Alpha_T_FF3)
Ido_Summary[ ,CAPM.Alpha:=apply(Ido_Summary,1,function(x) f(x[6],x[7]))]
Ido_Summary[ ,FF3.Alpha:=apply(Ido_Summary,1,function(x) f(x[8],x[9]))]
setnames(Ido_Summary,"IdoVol_decile","Rank")

#report repplicate results
pander(Ido_Summary[,c(1:5,10,11)],
       caption="Panel B: Portfolios Sorted by Idiosyncratic Volatility Relative to FF-3")