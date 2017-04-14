
#processing details :
 # 1) add DLRET return is it is not NA at the delisting month.
#2) market equity calculated as abs(PRC) × abs(SHROUT) × 1000.
#3) only consider firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ
#(with EXCHCD 1,2,3) and U.S. common stocks (with SHRCD of 10 or 11) at the beginning
#of month t.
#4) exclude rows with missing record, missing price or share data at t ??? 1.
#The head of output and the plot of return series are:

PS1_Q1 <- function(CRSP_Stocks)
{

 # Input
 # data.table CRSP Stocks, with columns: PERMNO, date, SHRCD, EXCHCD,
 # RET, RETX, DLRET, DLRETX, PRC, SHROUT
 # Output
 # data.table, with each row corresponding to a unique year and month, with columns
 .
  library('data.table')
  library('dplyr')
  library('lubridate')
  library('zoo')
  
  ## include delisting returns
  CRSP_Stocks[,TRET:=ifelse(is.na(DLRET)==1,RET,ifelse(is.na(RET)==1,
                                                       DLRET,(1+RET)*(1+DLRET)-1))]
  
  ## Construct market equity abs(PRC)*abs(SHROUT)*1000
  CRSP_Stocks[, c("Year",'Month','MV') := list(as.integer(year(date)),as.integer(month(date)), 
                                               abs(PRC)*abs(SHROUT)*1000)]
  CRSP_Stocks[,YearMonth :=as.yearmon(paste(Year, Month, sep = "-"))]
  
  setorder(CRSP_Stocks,PERMNO,YearMonth)
  ## maket value at the beginning of t
  CRSP_Stocks[,c("lag_YearMonth",'lag_MV') :=list(YearMonth==shift(YearMonth)+1/12,
                                                  shift(MV)),by='PERMNO']
  
  ## only consider firms incorporated in the US and listed on the NYSE, AMEX, or NASDAQ (with EXCHCD 1,2,3)
  ## and U.S. common stocks (with SHRCD of 10 or 11) at the beginning of month t
  CRSP_Stocks <- CRSP_Stocks[SHRCD %in% c(10, 11) &  EXCHCD %in% c(1,2,3)]
  
  ## exclude rows with missing record at the beginning of t, missing price or share data at beginning of t,
  ## missing return data at t
  CRSP_Stocks <- CRSP_Stocks [lag_YearMonth==TRUE & !is.na(lag_MV) & !is.na(TRET),
                              .(PERMNO,Year,Month,TRET,lag_MV)]
  CRSP_Stocks <- unique(CRSP_Stocks)
  
  ## calc total market equity  at the beginning of t
  CRSP_Stocks[, 'Stock_lag_MV' :=sum(lag_MV,na.rm =TRUE)/1000000,
              by = .(Year,Month)]
  
  ## calc equal weighted and value weighted return
  result <- unique(CRSP_Stocks[,.(Stock_lag_MV,Stock_Ew_Ret= mean(TRET,na.rm =TRUE),
                                  Stock_Vw_Ret=sum(lag_MV/1000000*TRET/Stock_lag_MV,na.rm =TRUE)),by=.(Year,Month)])
  return (result)
} 
  

  PS1_Q2 <- function (Monthly_CRSP_Stocks,FF_mkt)
  {
    #Output
    # 5 * 2 numeric matrix. Rows: Annualized Mean, Annualized Standard Deviation,
    #Sharpe Ratio, Skewness, and Excess Kurtosis. Columns: Estimated FF Market,
    #Actual FF Market.
    
    library('dplyr')
    library(moments)
    
    # merge two input data table
    Com_R<-merge(Monthly_CRSP_Stocks[,.(Year,Month,Stock_Vw_Ret)],
                 FF_mkt[,.(Year,Month,Market_minus_Rf,Rf)],by=c('Year','Month'))
    
    # calculate the estimated excess returns (estimated FF Market)
    Com_R[, Est.Market_minus_Rf :=Stock_Vw_Ret-Rf]
    
    # calculate the annual return geometrically
    Com_R<-Com_R[,.(Year,Actual_FF_Market=Market_minus_Rf,Estimated_FF_Market=Est.Market_minus_Rf)]
    
    Com_R<-Com_R[, c("Annual_Actual","Annual_Estimate") :=
                   list( prod(1+Actual_FF_Market)^(12/.N)-1,
                         prod(1+Estimated_FF_Market)^(12/.N)-1 ),by=.(Year)]
    Annual_CRSP_Stocks<-unique(Com_R[,.(Annual_Actual,Annual_Estimate)])
    
    # output is 5*2 matrix
    result <- matrix(NA, nrow = 5, ncol = 2)
    colnames(result) <- c("Actual_FF_Market","Estimated_FF_Market")
    rownames(result) <- c("Annualized_Mean","Annualized_Standard_Deviation",
                          "Annualized_Sharpe_Ratio","Skewness","Excess_Kurtosis")
    
    #calculate annualized return geometrically
    result[1,] <- apply(Annual_CRSP_Stocks,2,mean)
    #calculate annualized stadard deviation 
    result[2,] <- apply(Annual_CRSP_Stocks,2,sd)
    #calculate annualized sharpe ratio
    result[3,] <- c(result[1,1]/result[2,1],result[1,2]/result[2,2])
    #calculate annualized skewness
    result[4,] <-  apply(Annual_CRSP_Stocks,2,function(x) skewness(x))
    #calculate annualized kurtosis
    result[5,] <-  apply(Annual_CRSP_Stocks,2,function(x) kurtosis(x)-3)
    
    # round the result to 4 decimal digits
    result<-round(result,digits=4)
    
    return (result)
  }
  
  

  PS1_Q3 <- function(Monthly_CRSP_Stocks,FF_mkt)
  {
   # Output
    #Vector of length two. Correlation between time series and maximum absolute
   # diFFerence between two time series.
    
    # merge two input data table
    Com_R<-merge(Monthly_CRSP_Stocks[,.(Year,Month,Stock_Vw_Ret)],
                 FF_mkt[,.(Year,Month,Market_minus_Rf,Rf)],by=c('Year','Month'))
    
    # calculate the estimated excess returns (estimated FF Market)
    Com_R[, Est.Market_minus_Rf :=Stock_Vw_Ret-Rf]
    
    # calculate absolute return difference
    Com_R<-Com_R[,.(Actual_FF_Market=Market_minus_Rf,Estimated_FF_Market=Est.Market_minus_Rf)]
    Com_R[,diff_FF_Market := abs(Actual_FF_Market-Estimated_FF_Market)]
    
    # output is 5*2 matrix
    result <- vector(mode = "numeric", length = 2)
    names(result) <- c("Correlation","Maximum_Absolute_difference")
    
    # calculate Correlation between time series
    result[1] <- cor(Com_R$Actual_FF_Market,Com_R$Estimated_FF_Market)
    # calculate maximum absolute difference between two time series
    result[2] <- max(Com_R$diff_FF_Market)
    
    # round the result to 8 decimal digits
    result<-round(result,digits=8)
    
    return(result)
  }