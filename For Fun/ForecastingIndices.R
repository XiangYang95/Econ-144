rm(list = ls(all=T))
library(quantmod)
library(qrmdata)
library(aTSA)
library(xts)
setwd("/Users/Xiang/OneDrive/Desktop/Econ-144")



data("SP500")
data("DJ")
data("NASDAQ")
data("FTSE")
data("SMI")
data("EURSTOXX")
data("CAC")
data("DAX")
data("CSI")
data("HSI")
data("SSEC")
data("NIKKEI")


windows()
par(mfrow=c(2,2))
plot(CAC)
plot(DJ)
plot(NASDAQ)
plot(CSI)

windows()
par(mfrow=c(2,2))
plot(HSI)
plot(FTSE)
plot(SMI)
plot(EURSTOXX)

windows()
par(mfrow=c(2,2))
plot(DAX)
plot(SSEC)
plot(NIKKEI)

indices = data.frame(CAC,DJ,NASDAQ,HSI,FTSE,SMI,EURSTOXX,DAX,SSEC,NIKKEI)
#Test cointegration of Dow Jones and NASDAQ
FTSEmonth = to.monthly(FTSE)
DJmonth = to.monthly(DJ)
FTSEmonth_ts = as.ts(FTSEmonth$FTSE.Close[13:length(FTSEmonth$FTSE.Close)],
      start = head(index(FTSEmonth$FTSE.Close[13:length(FTSEmonth$FTSE.Close)]), 1),
      end = tail(index(FTSEmonth$FTSE.Close[13:length(FTSEmonth$FTSE.Close)]), 1))
DJmonth_ts = as.ts(DJmonth$DJ.Close, start = head(index(DJmonth$DJ.Close), 1),
                   end = tail(index(DJmonth$DJ.Close), 1))
reg=lm(FTSEmonth_ts~DJmonth_ts)
summary(reg)

plot(reg$residuals)
adf.test(res)

#cointegration between Dow Jones and NASDAQ is not possible, the relationship is spurious

#Test cointegration of NASDAQ and NIKKEI
FTSEmonth = to.monthly(FTSE)
NIKKEImonth = to.monthly(NIKKEI)
reg=lm(FTSEmonth$FTSE.Close~NIKKEImonth$NIKKEI.Close)
summary(reg)
plot(as.numeric(reg$residuals))

#Johansen test
x=cbind(FTSEmonth$FTSE.Close,NIKKEImonth$NIKKEI.Close)
m1=ar(x)
m1$order
m2=ca.jo(x,K=2)
summary(m2)
m3=ca.jo(x,K=2,type=c("trace"))
summary(m3)

y = FTSEmonth$FTSE.Close -0.2038147*NIKKEImonth$NIKKEI.Close
plot(residuals(y))