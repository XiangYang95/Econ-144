#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/22/2015
# Comment(s): R code example for characterizing cycles in ts data.
# Data File(s): The observations are generated via random walks.
#***********************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
#setwd("/Users/DrDrR4/Documents/Courses/2015/spring/Econ144/R_Codes")

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
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
library(DAAG)
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
#install.packages("astsa")
#require(astsa)
library(nlstools)

# Random Walk Simulation, Zt ~ N(0,1)

#1. Xt = Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = x[t-1] + e[t]
windows()
plot(x, type="l")
acf(x)
acf(diff(x))

#2. Xt =  Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = e[t]
windows()
plot(x, type="l")
acf(x)
acf(diff(x))

#3. Xt = -0.3Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = -0.3*x[t-1] + e[t]
windows()
par(mfrow =c(3,1))
plot(x, type="l")
acf(x)
acf(diff(x))
#if you get a spike for the lag 1 for the pacf, it proves that there is a correlation 
#with the lag 

#4. Xt = sin(pi t/3) + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = sin(pi*t/3)+ e[t] -x[t-1]
windows()
par(mfrow=c(3,1))
plot(x,type='l',main="Original Time Series of the Data")
acf(x,main="ACF")
acf(diff(x),main="ACF (First Difference)",ylab=expression(ACF(Delta)))

# Plot the autocorrelation, autocovariance, and partial autocorrelation functions
windows()
par(mfrow=c(2,2))
plot(x,type='l')
acf(x, type = "correlation")
acf(x, type = "covariance")
acf(x, type = "partial")

# Plot the autocorrlation function of the First and Second difference
windows()
par(mfrow=c(2,1))
acf(diff(x))
acf(diff(diff(x)))



