#load the data
industry<-read.csv("./48_Industry_Portfolios-value_weighted-monthly.csv",head=TRUE)
FFfactors<-read.csv("./F-F_Research_Data_Factors -monthly.csv",head=TRUE)

# remove the industry having missing value
industry <- industry[,apply(industry,2,function(x) !any(x==-99.99))]

# set the time span from 1960 to 2015
library(zoo)
industry[,1]<-as.Date(as.yearmon(as.character(industry[,1]),"%Y%m"))
FFfactors[,1]<-as.Date(as.yearmon(as.character(FFfactors[,1]),"%Y%m"))
startdt <- as.Date("1960-01-01") 
enddt <- as.Date("2015-12-31")
industry <- industry[industry$X>=startdt & industry$X<=enddt,]
FFfactors <-FFfactors[FFfactors$X>=startdt & FFfactors$X<=enddt,]

# calc excess return for each industry
industry_exc<-industry[, -1] - FFfactors[, 5]
industry_FF<-cbind(Date=industry[, 1],industry_exc,Mkt.RF=FFfactors[,"Mkt.RF"])


# melt the data and regress each industry excess return on the market excess return
library(reshape2)
industry_FF_reg <- melt(industry_FF, id.vars=c("Date","Mkt.RF"), value.name="Ind.RF")
library(plyr)
library(sandwich)
library(lmtest)
reg_out<-dlply(industry_FF_reg, .(variable), function(df) {out <- lm(Ind.RF ~ Mkt.RF, data=df)
                  c(coeftest(out, vcov = vcovHC(out, "HC0"))[1:4],summary(out)$r.squared)}) 

reg_out<-as.data.frame(t(as.data.frame(reg_out)))
colnames(reg_out)<-c("intercept","Mkt.RF","sd_intercept","sd_Mkt.RF","r.squared")

# bar plot of industry beta
library(ggplot2)
ggplot(reg_out, aes(x=rownames(reg_out), y=Mkt.RF)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Mkt.RF-2*sd_Mkt.RF, ymax=Mkt.RF+2*sd_Mkt.RF), width=0.6,
                position=position_dodge(.9),col="blue")+
  theme(axis.text.x=element_text(size = 6, angle = 90, vjust = 0.5))+
  xlab("industry") +
  ylab("beta") +
  ggtitle("bar plot of industry beta")

# range of industry beta
library(pander)
beta_range<-data.frame(lower=range(reg_out[,"Mkt.RF"])[1],upper=range(reg_out[,"Mkt.RF"])[2])
pander(beta_range,digits = 2, caption = 'Range of estimated betas')

# mean, min and max of r squared
r.squared_sum<-data.frame(
  Min=min(reg_out[,"r.squared"]),
  Max=max(reg_out[,"r.squared"]),
  Mean=mean(reg_out[,"r.squared"]))
  
pander(r.squared_sum, digits = 2, caption = 'Summary of r squared')

# Plot estimated alphas (intercept terms) against estimated betas
ggplot(reg_out,aes(x=intercept,y=Mkt.RF)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Estimated alphas against betas")+
  xlab("alpha") +
  ylab("beta") 

#run rolling regressions of 5 years of data
dolm <- function(x) coef(lm(Ind.RF ~ Mkt.RF, data=as.data.frame(x)))[2] 
reg_roll<-dlply(industry_FF_reg, .(variable), function(df) {
  out <-rollapply(df[,c(2,4)], 60, by=60,dolm, by.column = FALSE)})
reg_roll<-as.data.frame(reg_roll) 
# calculate correlations of adjacent betas
beta_cor<-as.data.frame(apply(reg_roll,2,function(x) acf(x,lag=1,plot=FALSE)$acf[2]))
colnames(beta_cor)<-"correlation"

# plot correlations of betas across industries
ggplot(beta_cor,aes(x=rownames(beta_cor),y=correlation)) + geom_point() +
  geom_hline(yintercept=0)+
  ggtitle("correlations of adjacent betas")+
  xlab("industry") +
  ylab("correlation") +
  theme(axis.text.x=element_text(size = 6, angle = 90, vjust = 0.5))





