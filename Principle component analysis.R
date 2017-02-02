setwd("D:/MFE/Courses/Empirical Methods in Finance/HW")

#load the data
industry<-read.csv("./48_Industry_Portfolios-value_weighted-monthly.csv",head=TRUE)
FFfactors<-read.csv("./F-F_Research_Data_Factors -monthly.csv",head=TRUE)

# set the time span from 1960 to 2015
library(zoo)
industry[,1]<-as.Date(as.yearmon(as.character(industry[,1]),"%Y%m"))
FFfactors[,1]<-as.Date(as.yearmon(as.character(FFfactors[,1]),"%Y%m"))
startdt <- as.Date("1960-01-01") 
enddt <- as.Date("2015-12-31")
industry <- industry[industry$X>=startdt & industry$X<=enddt,]
FFfactors <-FFfactors[FFfactors$X>=startdt & FFfactors$X<=enddt,]

# remove the industry having missing value
industry <- industry[,apply(industry,2,function(x) !any(x==-99.99))]

# calc excess return for each industry
industry_exc<-industry[, -1] - FFfactors[, 5]

# calc the covariance matrix
exc_cov<-cov(industry_exc)

#PCA
#cov_pca<-prcomp(industry_exc,center=FALSE,scale.=FALSE)
#port_var<-as.data.frame(t(summary(cov_pca)$importance)[,2])
eid<-eigen(exc_cov,symmetric=TRUE)
eiv_industry<-eid$values
var.explained<-as.data.frame(eiv_industry/sum(eiv_industry))
colnames(var.explained)<-"Variance_exp"

# eigenvalues of variance-covariance matrix of the excess returns of 48 industries
library(pander)
pander(t(eiv_industry),digits = 2, 
       caption = 'Eigenvalues of variance-covariance matrix of the excess returns of 48 industries')

# plot the variance explained
library(ggplot2)
ggplot(var.explained, aes(x=factor(rownames(var.explained),levels = rownames(var.explained)),
                     y=Variance_exp)) +
  geom_bar(position=position_dodge(), stat="identity") +
  theme(axis.text.x=element_text(size = 6, angle = 90, vjust = 0.5))+
  xlab("Compolents") +
  ylab("% Variance explianed") +
  ggtitle("Fraction of variance explained")

#3 largest principal components
print<-as.data.frame(t(cumsum(var.explained)[1:3,]))
colnames(print)<-c("CP1","CP2","CP3")
pander(print,digits = 2, 
       caption = '3 largest principal components')

# calculate the mean, standard deviation and correlation of 3 largest components
#component<-as.matrix(industry_exc)%*%as.matrix(cov_pca$rotation)
#sqrt(diag(cov(pc_r)))
component<-as.matrix(industry_exc)%*%t(eid$vectors)
component_l3<-as.data.frame(component)[,1:3]
colnames(component_l3)<-c("CP1","CP2","CP3")
component_l3_m<-sapply(component_l3,mean)
component_l3_sd<-sqrt(diag(cov(component_l3)))
component_l3_cor<-cor(component_l3)
pander(as.data.frame(component_l3_m),digits = 2, 
       caption = "Mean of 3 largest principal components")
pander(as.data.frame(component_l3_sd),digits = 2, 
       caption = 'Standard deviation of 3 largest principal components')
pander(component_l3_cor,digits = 2, 
       caption = 'Correlation of 3 largest principal components')

# compare predicted and actual returns

# predict returns from APT model
# get betas from eigenvectors
component_beta<-eid$vectors[,1:3]
industry_apt<-as.matrix(component_beta)%*%component_l3_m
#realized average returns
industry_rel<-sapply(industry_exc,mean)
#Plot predicted return from APT model versus the realized average  returns
industry_com<-cbind(industry_apt,industry_rel)
colnames(industry_com)<-c("Predicted","Actual")

ggplot(industry_com,aes(Predicted,Actual)) +
  geom_point(colour="blue") +
  geom_abline(slope=1,intercept=0)+
  xlim(0,1)+ylim(0,1)+
  xlab("Predicted mean excess returns from APT")+
  ylab("Actual mean excess returns")+
  ggtitle("Plot of predicted returns vs realized returns")

# *********by using regression we get the same results*******#
#industry_pc<-cbind(industry_exc,component_l3)
# melt the data and regress each industry excess return on the 3 largest componets
#library(reshape2)
#industry_pc_reg <- melt(industry_pc, id.vars=c("1","2","3"), value.name="Ind.RF")
#colnames(industry_pc_reg)<-c("CP1","CP2","CP3","variable","Ind.RF")
#library(plyr)
#reg_out<-dlply(industry_pc_reg , .(variable), function(df) 
#{ summary(lm(Ind.RF ~ CP1+CP2+CP3, data=df))$coef[,1]})
#component_beta<-as.data.frame(t(as.data.frame(reg_out)))[,2:4]
# *********by using regression we get the same results*******#

#implied cross-sectional r squared
diff<-industry_rel-industry_apt
r.squared<-1-var(diff)/var(industry_rel)
pander(r.squared,digits = 2, 
       caption = 'Implied cross-sectional r squared')

#load the 25 portfolio data
portfolio25<-read.csv("./25_Portfolios_5x5-value_weighted-monthly.csv",head=TRUE)

# set the time span from 1960 to 2015
portfolio25[,1]<-as.Date(as.yearmon(as.character(portfolio25[,1]),"%Y%m"))
portfolio25 <- portfolio25[portfolio25$X>=startdt & portfolio25$X<=enddt,]

# calc excess return for each portfolio
portfolio25_exc<-portfolio25[, -1] - FFfactors[, 5]

# calc the covariance matrix
portfolio25_cov<-cov(portfolio25_exc)

#PCA
#cov_pca<-prcomp(industry_exc,center=FALSE,scale.=FALSE)
#port_var<-as.data.frame(t(summary(cov_pca)$importance)[,2])
eid_potfolio25<-eigen(portfolio25_cov,symmetric=TRUE)
eiv_potfolio25<-eid_potfolio25$values
var.explained_potfolio25<-as.data.frame(eiv_potfolio25/sum(eiv_potfolio25))
colnames(var.explained_potfolio25)<-"Variance_exp"

# eigenvalues of variance-covariance matrix of the excess returns of 25 F-F portfolios
pander(t(eiv_potfolio25),digits = 2, 
       caption = 'eigenvalues of variance-covariance matrix of the excess returns of 25 F-F portfolios')

# plot the variance explained
ggplot(var.explained_potfolio25, aes(x=factor(rownames(var.explained_potfolio25),
                    levels = rownames(var.explained)), y=Variance_exp)) +
  geom_bar(position=position_dodge(), stat="identity") +
  theme(axis.text.x=element_text(size = 6, angle = 90, vjust = 0.5))+
  xlab("Compolents") +
  ylab("% Variance explianed") +
  ggtitle("Fraction of variance explained")

#cumulative variance explained
pander(t(eiv_potfolio25),digits = 2, 
       caption = 'eigenvalues of variance-covariance matrix of the excess returns of 25 F-F portfolios')
