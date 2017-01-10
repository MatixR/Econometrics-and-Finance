# Bootstrap parameters of simple linear regression

# work on data defined as Reg_data
B=10000  #number of samples
BS_coefs=matrix(0,nrow=B,ncol=2)
for(b in 1:B){
  BS_sample=Reg_data[sample(1:N,size=N,replace=TRUE),]
  sum=summary(lm(GENX~VW,data=BS_sample))
  BS_coefs[b,]=sum$coef[,1]
}

par(mfrow=c(1,2))
hist(BS_coefs[,2],breaks=40,col="magenta",xlab="",ylab="",
     main="Bootstrap Dist of Slope",xlim=c(.3,.60),
     sub=paste("bootstrap std error = ",round(sd(BS_coefs[,2]),digits=3)))
summary(BS_coefs[,2])
hist(rnorm(B,mean=lsq.slope,sd=lsq.slope.stderr),breaks=40,col="magenta",
     xlab="",ylab="",main="Normal Dist of Slope",xlim=c(.3,.60),
     sub=paste("Least Squares Std Error = ",round(lsq.slope.stderr,digits=3)))

# construct a Bootstrap CI for the slope coef

int=quantile(BS_coefs[,2],probs=c(.975,.025))
CI.pivotal.bootstrap = c(2*lsq.slope-int[1],2*lsq.slope-int[2])
CI.normal.theory=c(lsq.slope+qt(.025,df=347)*lsq.slope.stderr,
                   lsq.slope+qt(.975,df=347)*lsq.slope.stderr)
CI.mat=rbind(CI.normal.theory,CI.pivotal.bootstrap)
colnames(CI.mat)=c("Lower","Upper")
rownames(CI.mat)=c("Normal Theory","Bootstrap Pivotal")
print(CI.mat)


# bootstrap exp(slope)

par(mfrow=c(1,1))
hist(exp(BS_coefs[,2]),breaks=40,col="magenta",xlab="",ylab="",
     main="Bootstrap Distribution of exp(Slope)",
     sub=paste("bootstrap std error = ",round(sd(exp(BS_coefs[,2])),digits=3)))
abline(v=exp(lsq.slope),lwd=3,col="yellow")

# bootstrap pivot CI for exp(slop)
int=quantile(exp(BS_coefs[,2]),probs=c(.975,.025))
CI.pivotal.bootstrap = c(2*exp(lsq.slope)-int[1],2*exp(lsq.slope)-int[2])
names(CI.pivotal.bootstrap)=c("Lower","Upper")
CI.pivotal.bootstrap