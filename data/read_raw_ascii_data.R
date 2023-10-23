rm(list=ls(all=TRUE))
library(xts)
library(highfrequency)
library(tidyquant)
library(moments)

sumst = function(x){
  vec = c(mean(x,na.rm = TRUE),sd(x,na.rm = TRUE),
  skewness(x,na.rm = TRUE),kurtosis(x,na.rm = TRUE))
  names(vec) = c('mean','sd','skew','kurt')
  return(round(vec,2))
}

min.seq = seq.POSIXt(as.POSIXct("2007-01-01 00:00:00",tz="EST"), 
                     as.POSIXct("2023-12-31 23:59:59",tz="EST"), 
                     by = "5 min")
all.dfs = list()

assets  = c('EURUSD','EURCHF','EURGBP','EURJPY','EURAUD',
            'USDCAD','USDCHF','USDJPY','GBPCHF','GBPJPY',
            'GBPUSD','AUDJPY','AUDUSD','CHFJPY','NZDJPY',
            'NZDUSD','EURCAD','AUDCAD','CADJPY','GBPAUD',
            'AUDNZD','GBPCAD')
# EURCHF is strange, but the problem is not in the data
# something happened on the 2014; use it anyways

i=5

# for(i in 1:length(assets)){

  x       = read.table(paste('C:/Users/USER/Desktop/raw_histdata_data/',assets[i],
                             ".csv",sep=''),header = FALSE, sep = ";",dec=".")
  colnames(x) <- c("DateTime Stamp", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  # format the first column as a date.time
  temp    <- as.POSIXct(strptime(x[,1], "%Y%m%d %H%M%S"),tz = 'EST')
  x$temp  <- temp+60*60*7 #change to GMT+2; now data is weekdays only
  index   = match(min.seq,temp)
  index   = na.omit(index)
  all.dfs[[i]] = xts(x[index,5], order.by=x[index,7])
# }


hfrets   = makeReturns(all.dfs[[i]])
dailyRV  = rRVar(hfrets, alignBy = "minutes",
                 alignPeriod = 5,makeReturns = FALSE)

RVts     = xts(coredata(dailyRV),order.by = date(dailyRV))

dailyret = aggregateTS(hfrets,FUN = "sum",alignBy = "days")

newts1 = xts(coredata(dailyret)*100,order.by = date(dailyret))

stand = scale(dailyret/sqrt(RVts))

hist(stand)
mean(stand)
sd(stand)

plot(stand)

# compare with data from YahooFinance
data = getSymbols(paste(assets[i],'=X',sep=''), from = date(dailyret)[1],
                  to = tail(date(dailyret),1),warnings = FALSE,
                  auto.assign = FALSE,periodicity = "daily",
                  return.class = 'xts')

yf = diff(log(data[,6]))*100
newts2 = xts(coredata(yf),order.by = date(yf))

plot(newts1)
lines(newts2,col=2,lwd=2)

cbind(sumst((dailyret)*100),sumst(yf))

# check more closely some small subsample: 
plot(newts1[100:150])
lines(newts2[100:150],col=2,lwd=2)

# very similar.
