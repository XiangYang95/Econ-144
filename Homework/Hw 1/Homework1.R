
#changed the working directory
setwd("/Users/Lenovo/Desktop/Econ 144/Homework/Hw 1")
#source("/Users/Lenovo/Desktop/Econ 144/Homework/Hw 1/Homework1.R")

#clear all variables and prior sessions
rm(list=ls(all=TRUE))

library(DAAG)
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(e1071)
library(corrplot)

#imported the dataset from DAAG'
data = data.frame(nsw74psid1)
attach(data)
summary(nsw74psid1)

##(a) Plot a histogram of each variable and discuss its properties.
windows()
par(mfrow=c(2,2))
truehist(trt,col='steelblue3',main="Whether the subject was enrolled in \n PSID(==0) or NSW(==1)",xlab="NSW or PSID", ylab="Fraction")
lines(density(trt),lwd=2)

truehist(age,col='steelblue3',main="Age",xlab="age", ylab="Fraction")
lines(density(age),lwd=2)

truehist(educ,col='steelblue3',main="Years of Education",xlab="years", ylab="Fraction")
lines(density(educ),lwd=2)

truehist(black,col='steelblue3',main="Whether the subject was Black",xlab="black or not", ylab="Fraction")
lines(density(black),lwd=2)

windows()
par(mfrow=c(2,2))
truehist(hisp,col='steelblue3',main="Whether the subject was Hispanic",xlab="hispanic or not", ylab="Fraction")
lines(density(hisp),lwd=2)

truehist(marr,col='steelblue3',main="Whether the subject was married",xlab="married or not", ylab="Fraction")
lines(density(marr),lwd=2)

truehist(nodeg,col='steelblue3',main="Whether the subject was a \n high school dropout",xlab="dropout or not", ylab="Fraction")
lines(density(nodeg),lwd=2)

truehist(re74,col='steelblue3',main="Real Earnings in 1974",xlab="real earnings", ylab="Fraction")
lines(density(re74),lwd=2)

windows()
par(mfrow=c(1,2))
truehist(re75,col='steelblue3',main="Real Earnings in 1975",xlab="real earnings", ylab="Fraction")
lines(density(re75),lwd=2)

truehist(re78,col='steelblue3',main="Real Earnings in 1978",xlab="real earnings", ylab="Fraction")
lines(density(re78),lwd=2)

# Most of the data variables are binary, such as whether the subject was black or not. Even when 
# the data is not binary, the pdf does not exhibit the shape of a normal distribution

##(b) Estimate your full regression model and show your results (e.g., the output from the `summary' command in R).
##    Discuss these results. 
y = lm(re78~trt+age+educ+black)
summary(y)

#Since the p-values are so small, the estimated coefficients that we obtained by running the regression
#is statistically significant. However since the R^2 is so low, the model does not fit the data

# (c) Compute the Mallows CP statistic for all the plausible models and choose only one
# model. Discuss why you chose this model. For the next questions, only use your selected
# model from this part.
windows()
ss=leaps::regsubsets(re78~trt+age+educ+black,method=c("exhaustive"),nbest=4,data=data)
subsets(ss,statistic="cp",legend=F,main="Mallows CP",col="steelblue4")
legend(2.3,650,bty="n",legend=c('t=trt','a=Age','e=Years of Education', 'b=Black'),col="steelblue4",cex=1.5)

# From the Mallows Cp, we can see that the choosing all 4 variables would give the lowest Cp. 
# However, the Cp is sufficiently low enough when we just take trt, age and educ variables. 
# So I'll use these variables.

y1 = lm(re78~trt+age+educ)

#(d) Plot the residuals vs. the fitted values.
windows()
par(mfcol=c(2,2))
plot(y1$fit,y1$res,col="skyblue3",pch=20,xlab="Predicted Response",ylab="Residuals",main="Residuals vs. Predicted Response",cex.axis=0.8,cex.main=0.9)
abline(h=0,lwd=2,col="red")
lines(lowess(y1$fit,y1$res),lwd=1.5) 
abline(v=6e5,col="red",lty=2,lwd=1.5)
legend(-5500,100000,c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=0.8)

#(e)Plot and discuss the VIF plot.
plot(vif(y1),col="skyblue3",xlab=rbind("trt", "age", "educ"),ylab="Residuals",main="Residuals vs. Predicted Response",cex.axis=0.8,cex.main=0.9)

#(f) Plot and discuss the correlation graph (use the corrplot library).
M <- cor(data)
corrplot(M, method = "circle")

#The plot shows that there is very little correlation between any 2 variables 
#from the data, except that between years of education and being a high school dropout,
#and between the different years of the earning. This might imply that the other
#variables such as age, years of education, etc does not affect earnings in the population.

# (g) Plot the Cook's Distance values. Are there any outliers? If so, discuss what you would
# do with them.
y1_cook=cooks.distance(y1)
plot(y1_cook,ylab="Cook's distance",type='o',main="Cook's Distance Plot",col="skyblue4", pch=20,lwd=.25)
text(7,0.28,"wt=5345lbs \n 8-Cyl \n  hp=230 \n mpg=14.7")
text(14,0.33, expression(""%->%""))

# From the plot, we could see that there are outliers. One way to deal with it is to consider
# modifying the model, such as to a log-linear one. Since we could see that there is only one 
# outlier that stands out too much, another way is to just remove that point.

#(h) Plot a histogram of the residuals and discuss the results.
truehist(y1$res,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals")
xr=rnorm(800000,mean(y1$res),sd(y1$res))
lines(density(xr),col="black",lwd=2)
lines(density(y1$res),col="red",lwd=2)
legend(0.3,0.2,c("Density","Normal Distr."),fill=c("red","black"),bty='n')

# Since pdf of residuals closely resembles that of a normal distribution, we know that the OLS is unbiased.
# This means that we can estimate the coefficients of the variables without bias.

#(i) Plot the QQ Normal Plot and discuss the results.
qqnorm(y1$res,col="skyblue4", pch=20,lwd=1,main="QQ Normal Plot")

#change this
# Since the shape of the line is fairly straight, we can assume that linear regression works fine.
# However we could that there are some points that jump at the right tail, which implies that we have outliers.

#(j) Plot the observed vs. predicted values, overlay a Lowess smoother, and discuss the
#results.
windows()
plot(y1$fit,re78,pch=20,col="skyblue4",cex=1,xlab="Predicted Response",ylab="Observed Response",main="Observed vs. Predicted Response \n Full Model",cex.axis=0.8,cex.main=1.0)
lines(lowess(y1$fit,re78),lwd=2)
abline(0,1,col="red",lwd=2,lty=2) 
text(25,18,expression(R^2==0.826))
legend(15,31, c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=1,bty="y")

# The way the plots scatter all over indicates that there is very little correspondence 
# between the observed values and the predicted values. This means that the model for prediction  
# is poor
#-------------------------------------------------------------------
#2
#clear all variables and prior sessions
rm(list=ls(all=TRUE))

#get the data and attach it
data = read.csv("2.2.csv", header = T)
attach(data)

#obtain the descriptive statistics: mean, median, variance, standard deviation,
#skewness, and kurtosis.
print(paste("The mean of the GDP quarterly growth rates is ", mean(GRGDP)))
print(paste("The median of the GDP quarterly growth rates is ", median(GRGDP)))
print(paste("The variance of the GDP quarterly growth rates is ", var(GRGDP)))
print(paste("The standard deviation of the GDP quarterly growth rates is ", (var(GRGDP)^(0.5))))
print(paste("The skewness of the GDP quarterly growth rates is ", skewness(GRGDP)))
print(paste("The kurtosis of the GDP quarterly growth rates is ", kurtosis(GRGDP)))

print(paste("The mean of the S&P500 quarterly returns is ", mean(RETURN)))
print(paste("The median of the S&P500 quarterly returns is ", median(RETURN)))
print(paste("The variance of the S&P500 quarterly returns is ", var(RETURN)))
print(paste("The standard deviation of the S&P500 quarterly returns is ", (var(RETURN)^(0.5))))
print(paste("The skewness of the S&P500 quarterly returns is ", skewness(RETURN)))
print(paste("The kurtosis of the S&P500 quarterly returns is ", kurtosis(RETURN)))


#histograms
windows()
par(mfrow=c(1,2))
truehist(GRGDP,col='steelblue3',main="U.S. GDP quarterly growth rates",xlab="Rates", ylab="Fraction",
         xlim=c(-5,5),ylim=c(0,0.7))
lines(density(GRGDP),lwd=2)

truehist(RETURN,col='steelblue3',main="S&P 500 quarterly returns",xlab="Returns", ylab="Fraction",
         xlim=c(-40,50),ylim=c(0,0.09))
lines(density(RETURN),lwd=2)

#correlation 
print ("The correlation matrix is: ")
cor(cbind(GRGDP, RETURN))

#This statistics shows that the median is higher slightly compared to the mean,
#the data is skewed slightly to the right, which can be verified by looking at the 
#negative skewness and the shape of the histogram. 
#The correlation between US GDP growth rate and SP500 return is also very low.

#3.
#clear all variables and prior sessions and obtain the data
rm(list=ls(all=TRUE))
data = read.csv("3.1.csv", header = T)
attach(data)

#a)
rpce_pctchg = diff(log(rpce))
rdpi_pctchg = diff(log(rdpi))

windows()
par(mfrow = c(2,1))
plot(rpce_pctchg, main="Scatterplot of real personal consumption expenditure growth rate", 
     ylab="real personal consumption expenditure growth rate", pch=19)
plot(rdpi_pctchg, main="Scatterplot of real disposal personal income",  
     ylab="real disposal personal income", pch=19)
#change this
#from the plots, we can see that growth rate of the real personal consumption expenditure
#growth rate is more volatile than that of real disposal personal income
#Permanent income model that the current expenditure is not determined just by their 
#current income but also by their future expected income. As such, it would mean 
#that even if a person's income stays relatively constant, his consumption might vary alot 
#depending on whether he wants to spend less in the current time and spend more later or
#vice versa. This would mean that that the model explains the volatility difference.

#b)
y = lm(rpce_pctchg~rdpi_pctchg)
summary(y)

#both values for the intercept and the coefficient of income have very low p value,
#which means that they are statistically significant.
#the intercept is close to 0, which means that if the person's income is 0, he would not consume. 
#The income coefficient is positive, which means that the higher the income,
#the more the person will consume

#c)
rdpi_pctchg_lag = c(NA,head(rdpi_pctchg,-1))
y = lm(rpce_pctchg~rdpi_pctchg+rdpi_pctchg_lag)
summary(y)


#4.
#clear all variables and prior sessions and obtain the data
rm(list=ls(all=TRUE))
rgdp1 = read.csv("3.3a.csv", header = T)
attach(rgdp1)
rgdp_ts<-ts(rgdp,start=1948,freq=4)

jpusforex = read.csv("3.3b.csv", header = T)
attach(jpusforex)
jpusforex_ts<-ts(jpy_usd,start=1971,freq=252)

tcmr = read.csv("3.3c.csv", header = T)
attach(tcmr)
tcmr_ts<-ts(CMRate10Yr,start=1971,freq=252)

cer = read.csv("3.3d.csv", header = T)
attach(cer)
cer_ts<-ts(unemrate,start=1948, freq=12)

windows()
par(mfrow = c(1,2))
plot(rgdp_ts, main="Scatterplot of real GDP",  
     ylab="real GDP", pch=19)
plot(jpusforex_ts, main="Scatterplot of Japan-US FOREX",  
     ylab="Jp-US Forex", pch=19)
windows()
par(mfrow = c(1,2))
plot(tcmr_ts, main="Scatterplot of Treasure \n Constant Maturity Rate",  
     ylab="tcmr", pch=19)
plot(cer_ts, main="Scatterplot of \n Civilian Unemployment Rate",  
     ylab="cur", pch=19)

#All 4 scatterplots of time series, i.e those of real GDP, Japan-US FOREX and
#Treasury Constant Maturity Rate, Civilian Unemployment Rate show no equal 
#mean for its stochastic processes over time. This means that there these time
#series are neither first nor second order weakly stationary.

#5
#clear all variables and prior sessions and obtain the data
rm(list=ls(all=TRUE))
hpir = read.csv("4.31.csv", header = T)
attach(hpir)

hp_growth = diff(log(P))
ir_chg = diff(R..in...)

windows()
par(mfrow=c(2,2))
acf(P,main="ACF of the house prices")
pacf(P,main="PACF of the house prices")

acf(R..in...,main="ACF of the interest rates")
pacf(R..in...,main="PACF of the interest rates")

windows()
par(mfrow=c(2,2))
acf(hp_growth,main="ACF of the house prices growth")
pacf(hp_growth,main="PACF of the house prices growth")

acf(ir_chg,main="ACF of the interest rates changes")
pacf(ir_chg,main="PACF of the interest rates changes")

#Although there is a very high ACF for the house prices and interest rates, 
#the corresponding PACF is very low. But since the ACF slowly decreases as 
#the number of lags increases, the ACF of the first lag propagates to those of 
#all subsequent lags, which means that there is dependence when lag is 1 but not if 
#for all other lags greater than 1. This is further proved since the ACF and PACF
#of the house prices growth and interest rate changes show similar patterns to those 
#of house prices and interest rates.