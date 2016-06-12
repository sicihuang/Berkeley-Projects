library(ggmap)
library(mapproj)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(reshape2)

library(stringr)
library(xts)
library(tseries)
library(forecast)
library(multitaper)
library(dplR)

setwd("~/Documents/Cal/Stat_248/Project/Climate Change")

######## Data ########
#### City Seasonal Average Temperatures (1900 vs 2000) ####
city = read.csv("USByCity.csv", stringsAsFactors=FALSE)

us = get_map(location="United States", maptype="roadmap", color="bw",zoom=4)

plotTemp = function(day, ti, data=city, map=us){
  # Exclude Alaska station
  temp = data[(data$dt == day) & (data$City != "Anchorage"),]
  temp$lat = as.numeric(str_replace(temp$Latitude, "N", ""))
  temp$long = as.numeric(str_replace(temp$Longitude, "W", "")) * (-1)
  temp$MajorCity = (temp$City == "Los Angeles" | temp$City == "New York" | temp$City == "Chicago")
  ggmap(map) + 
    geom_point(aes(x=long, y=lat, color=AverageTemperature, size=MajorCity), data=temp) + 
    scale_color_gradient2(low="deepskyblue3", mid="yellow", high="red3", space="Lab", 
                          guide="colorbar") +
    labs(title=ti, x="Longitude", y="Latitude")
}

p1 = plotTemp("1900-02-01", "February, 1900")
p2 = plotTemp("1900-05-01", "May, 1900")
p3 = plotTemp("1900-08-01", "August, 1900")
p4 = plotTemp("1900-11-01", "November, 1900")
grid.arrange(p1, p2, p3, p4, ncol = 2, top="Average Temperatures of US Cities in 1900 (ºC)")

p1 = plotTemp("2000-02-01", "February, 2000")
p2 = plotTemp("2000-05-01", "May, 2000")
p3 = plotTemp("2000-08-01", "August, 2000")
p4 = plotTemp("2000-11-01", "November, 2000")
grid.arrange(p1, p2, p3, p4, ncol = 2, top="Average Temperatures of US Cities in 2000 (ºC)")

#### State vs Major City Monthly Average Temperatures (1849-2013) ####
la = subset(read.csv("LA.csv", stringsAsFactors=FALSE), dt < "2013-01-01")
nyc = subset(read.csv("NYC.csv", stringsAsFactors=FALSE), dt < "2013-01-01")
chi = subset(read.csv("Chicago.csv", stringsAsFactors=FALSE), dt < "2013-01-01")

state = read.csv("ByState.csv", stringsAsFactors=FALSE)
ca = subset(state[state$State == "California",], dt < "2013-01-01")
ny = subset(state[state$State == "New York",], dt < "2013-01-01")
il = subset(state[state$State == "Illinois",], dt < "2013-01-01")

plotCityState = function(df1, df2, state, city){
  statePlot = data.frame(cbind(as.Date(df1$dt), df1$AverageTemperature, df2$AverageTemperature))
  colnames(statePlot) = c("dt", state, city)
  statePlotLong = melt(statePlot, id="dt")
  
  ti = paste(state, "and", city)
  ggplot(data=statePlotLong, aes(x=as.Date(dt), y=value, colour=variable)) + 
    geom_line(alpha=0.7) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(title=ti, x="Year", y="Temperature (ºC)")
}

p1 = plotCityState(ca, la, "California", "Los Angeles")
p2 = plotCityState(ny, nyc, "New York", "New York City")
p3 = plotCityState(il, chi, "Illinois", "Chicago")
grid.arrange(p1, p2, p3, ncol = 2, top="State vs Major City Average Temperatures from 1849 to 2012")

######## EDA ########
#### Histograms ####
# cityPlot = data.frame(cbind(as.Date(la$dt), la$AverageTemperature, nyc$AverageTemperature, 
#                             chi$AverageTemperature))
# colnames(cityPlot) = c("dt", "LA", "NYC", "CHI")
# cityPlotLong = melt(cityPlot, id="dt")

# ggplot(cityPlotLong, aes(x=value, fill=variable)) + 
#   geom_histogram(alpha=0.7, position="identity") +
#   labs(title="Histogram", x="Temperature (ºC)")

p1 = ggplot(la, aes(x=AverageTemperature)) + 
  geom_histogram(fill="lightcoral", color="seashell") +
  scale_x_continuous(limits=c(-10,30)) +
  scale_y_continuous(limits=c(0,210)) +
  theme(plot.title=element_text(size=25), axis.title=element_text(size=25),
        axis.title.x=element_blank(), axis.text=element_text(size=20)) +
  labs(title="Los Angeles")

p2 = ggplot(nyc, aes(x=AverageTemperature)) + 
  geom_histogram(fill="lightgreen", color="seashell") +
  scale_x_continuous(limits=c(-10,30)) +
  scale_y_continuous(limits=c(0,210)) +
  theme(plot.title=element_text(size=25), axis.title=element_text(size=25),
        axis.title.x=element_blank(), axis.text=element_text(size=20)) +
  labs(title="New York")

p3 = ggplot(chi, aes(x=AverageTemperature)) + 
  geom_histogram(fill="steelblue2", color="seashell") +
  scale_x_continuous(limits=c(-10,30)) +
  scale_y_continuous(limits=c(0,210)) +
  theme(plot.title=element_text(size=25), axis.title=element_text(size=25),
        axis.text=element_text(size=20)) +
  labs(title="Chicago", x="Temperature (ºC)")

grid.arrange(p1, p2, p3, nrow=3, 
             top=textGrob("Major City Monthly Average Temperature Distibutions (1849-2012)",
                          gp=gpar(fontsize=25)))

#### Yearly Line Plots ####
plotCity = function(year){
  cityPlot = data.frame(cbind(as.Date(la$dt, format="%Y-%m-%d", origin="1849-01-01"), 
                              la$AverageTemperature, nyc$AverageTemperature, chi$AverageTemperature))
  colnames(cityPlot) = c("dt", "LA", "NYC", "CHI")
  cityPlotLong = melt(cityPlot[((year-1849)*12+1):((year-1849)*12+12),], id="dt")

  ti = paste("Los Angeles, New York City, and Chicago Monthly Average Temperatures for", year)
  ggplot(data=cityPlotLong, aes(x=as.Date(dt), y=value, colour=variable)) + 
    geom_line() +
    scale_x_date(date_labels="%b", date_breaks="1 month") +
    scale_y_continuous(limits=c(-10, 30)) +
    labs(title=ti, x="Month", y="Temperature (ºC)")
}

p1 = plotCity(1850)
p2 = plotCity(1900)
p3 = plotCity(1950)
p4 = plotCity(2000)
grid.arrange(p1, p2, p3, p4, nrow=2, 
             top="Los Angeles, New York City, and Chicago Monthly Average Temperatures")

plotCityLong = function(sYr, eYr){
  cityPlot = data.frame(cbind(as.Date(la$dt, format="%Y-%m-%d", origin="1849-01-01"), 
                              la$AverageTemperature, nyc$AverageTemperature, chi$AverageTemperature))
  colnames(cityPlot) = c("dt", "LA", "NYC", "CHI")
  if(eYr >= 2012){
    cityPlotLong = melt(cityPlot[((sYr-1849)*12+1):nrow(cityPlot),], id="dt")
  }else{
    cityPlotLong = melt(cityPlot[((sYr-1849)*12+1):((eYr-1849)*12+12),], id="dt")
  }
  
  ti = paste("Los Angeles, New York City, and Chicago \n Monthly Average Temperatures from", 
             sYr, "to", eYr)
  ggplot(data=cityPlotLong, aes(x=as.Date(dt), y=value, colour=variable)) + 
    geom_line(size=1) +
    scale_x_date(date_labels="%Y", date_breaks="1 year") +
    scale_y_continuous(limits=c(-10, 30)) +
    theme(plot.title=element_text(size=25), 
          legend.title=element_text(size=20), legend.text=element_text(size=20),
          axis.text=element_text(size=20), axis.title=element_text(size=25)) +
    labs(title=ti, x="Year", y="Temperature (ºC)") 
}

plotCityLong(2006, 2010)

#### Seasonal Decomposition ####
# laTS = xts(x=la$AverageTemperature, order.by=la$dt)
# plot(laTS['1951-01/1960-12'], main="LA Monthly Temperature", sub="Years 1951-1960")
# plot(laTS['2001-01/2010-12'], main="LA Monthly Temperature", sub="Years 2001-2010")
# plot(laTS, main="LA Monthly Temperature")
# lines(apply.yearly(laTS, FUN=mean), col="red")

laTS = ts(la$AverageTemperature, frequency=12, start=1849)
# seasonal trend decomposition based on moving average
# plot(decompose(laTS))
# seasonal trend decomposition based on Loess
plot(stl(laTS, "periodic"))

nycTS = ts(nyc$AverageTemperature, frequency=12, start=1849)
# plot(decompose(nyTS))
plot(stl(nycTS, "periodic"))

chiTS = ts(chi$AverageTemperature, frequency=12, start=1849)
# plot(decompose(chiTS))
plot(stl(chiTS, "periodic"))

######## Modeling ########
#### Linear Regression ####
plotMo = function(data, mo){
  plot(data$AverageTemperature[seq(mo, 1968, by=12)], type='l')
}

# LA, NYC, and CHI monthly temperatures exhibit linear trends, but have large variances
plotMo(la, 1)
plotMo(nyc, 2)
plotMo(chi, 5)

tsReg = function(dat){
  timeString = strptime(dat$dt, "%Y-%m-%d", tz="UTC")
  dat$Yr = as.numeric(format(timeString, "%Y"))
  dat$Mo = as.factor(as.numeric(format(timeString, "%m")))
  reg = lm(AverageTemperature~0+Mo+Yr, data=dat)
  return(reg)
}

resHist = function(res, ti){
  h = hist(res, breaks=100, ylim=c(0,100), main=ti)
  xfit = seq(min(res), max(res), length=40) 
  yfit = dnorm(xfit, mean=mean(res), sd=sd(res)) 
  yfit = yfit*diff(h$mids[1:2])*length(res) 
  lines(xfit, yfit, col="red", lwd=2) 
}

# Yr significant at less than 0.001 level
laReg = tsReg(la)
summary(laReg)

nycReg = tsReg(nyc)
summary(nycReg)

chiReg = tsReg(chi)
summary(chiReg)

par(mfrow = c(3,3))
# slightly light tailed comparing to normal
qqnorm(laReg$residuals, main="Normal Q-Q Plot for LA")
qqline(laReg$residuals, col="red")
qqnorm(nycReg$residuals, main="Normal Q-Q Plot for NYC")
qqline(nycReg$residuals, col="red")
qqnorm(chiReg$residuals, main="Normal Q-Q Plot for CHI")
qqline(chiReg$residuals, col="red")

# close to normal
resHist(laReg$residuals, "Histogram of Linear Regression Residuals (LA)")
resHist(nycReg$residuals, "Histogram of Linear Regression Residuals (NYC)")
resHist(chiReg$residuals, "Histogram of Linear Regression Residuals (CHI)")

# residuals display a non-random pattern, indicating a poor fit for a linear model
plot(laReg$fitted.values, laReg$residuals, type='p', pch=20, main="Residual Plot for LA")
plot(nycReg$fitted.values, nycReg$residuals, type='p', pch=20, main="Residual Plot for NYC")
plot(chiReg$fitted.values, chiReg$residuals, type='p', pch=20, main="Residual Plot for CHI")

#### ARIMA ####
acfPlot = function(dat){
  par(mfrow = c(2, 2))
  acf(dat$AverageTemperature)
  pacf(dat$AverageTemperature)
  acf(diff(dat$AverageTemperature))
  pacf(diff(dat$AverageTemperature))
}

# seems stationary
acfPlot(la)
acfPlot(nyc)
acfPlot(chi)

# par(mfrow = c(2, 2))
# acf(nyc$AverageTemperature)
# pacf(nyc$AverageTemperature)
# acf(diff(nyc$AverageTemperature))
# pacf(diff(nyc$AverageTemperature))

# stationary by Dickey-FUller test; don't need differencing
adf.test(la$AverageTemperature) 
adf.test(nyc$AverageTemperature) 
adf.test(chi$AverageTemperature) 

# non stationary by kpss test
kpss.test(la$AverageTemperature) 
kpss.test(nyc$AverageTemperature)
kpss.test(chi$AverageTemperature)

# auto.arima didn't give seasonal component
# la: ARIMA(3,1,2); nyc: ARIMA(2,1,1); chi: ARIMA(2,1,1)
auto.arima(la$AverageTemperature, max.p=5, max.q=5, start.p=0, start.q=0, ic="bic")
auto.arima(nyc$AverageTemperature, max.p=5, max.q=5, start.p=0, start.q=0, ic="bic")
auto.arima(chi$AverageTemperature, max.p=5, max.q=5, start.p=0, start.q=0, ic="bic")

# look at acf and pacf of residuals: if needs more differencing, add AR term, if needs 
# less differencing, add MA terms

# arima function errors out for certain combinations:
# non-finite finite-difference value
# non-stationary seasonal AR part from CSS
# possible convergence problem: optim gave code = 1
# initial value in 'vmmin' is not finite

# refer to code in arima_test.R
arimaSel = function(dat){
  result = cbind(expand.grid(0:2,0:2,0:2,0:2)[-1,], matrix(0, nrow=80, ncol=62))
  colnames(result) = c("p", "q", "P", "Q", "AIC", "BIC", 7:66)
  rownames(result) = 1:nrow(result)
  
  for(i in 1:nrow(result)){
    # fit model on 1849-2007 data; predict for 2008-2012 (5 years)
    fit = arima(dat$AverageTemperature[-(1909:1968)], order=c(result[i,1],0,result[i,2]),
                seasonal=list(order=c(result[i,3],0,result[i,4]), period=12))
    pred = predict(fit, n.ahead=60)
    result[i,5] = fit$aic
    result[i,6] = AIC(fit, k=log(length(dat$AverageTemperature[-(1909:1968)])))
    result[i,7:66] = pred$pred
  }
  return(result)
}

resultLA = arimaSel(la)
resultLA = resultLA[order(resultLA$BIC),]
laFit = arima(la$AverageTemperature[-(1909:1968)], order=c(1,0,1),
              seasonal=list(order=c(1,0,1), period=12))
acf2(laFit$residuals)
# close to normal 
qqnorm(laFit$residuals)
qqline(laFit$residuals, col="red")
pred = predict(laFit, n.ahead=120)
yr = seq(1849, 2013, length.out=1969)[-1969]
plot(yr, la$AverageTemperature, type='l', xlim=c(2003,2017), xlab="Year", 
     main="ARIMA Prediction of LA Temperature")
lines(ts(pred$pred, frequency=12, start=2008), col="red")
# MSE
mean((la$AverageTemperature[1909:1968]-pred$pred[1:60])^2)

resultNYC = arimaSel(ny)
resultNYC = resultNYC[order(resultNYC$BIC),]
nycFit = arima(nyc$AverageTemperature[-(1909:1968)], order=c(1,0,1),
               seasonal=list(order=c(1,0,1), period=12))
acf2(nycFit$residuals)
# close to normal 
qqnorm(nycFit$residuals)
qqline(nycFit$residuals, col="red")
pred = predict(nycFit, n.ahead=120)
yr = seq(1849, 2013, length.out=1969)[-1969]
plot(yr, nyc$AverageTemperature, type='l', xlim=c(2003,2017), xlab="Year", 
     main="ARIMA Prediction of NYC Temperature")
lines(ts(pred$pred, frequency=12, start=2008), col="red")
# MSE
mean((nyc$AverageTemperature[1909:1968]-pred$pred[1:60])^2)

resultCHI = arimaSel(chi)
resultCHI = resultCHI[order(resultCHI$BIC),]
chiFit = arima(chi$AverageTemperature[-(1909:1968)], order=c(1,0,0),
               seasonal=list(order=c(1,0,1), period=12))
acf2(chiFit$residuals)
# close to normal 
qqnorm(chiFit$residuals)
qqline(chiFit$residuals, col="red")
pred = predict(chiFit, n.ahead=120)
yr = seq(1849, 2013, length.out=1969)[-1969]
plot(yr, chi$AverageTemperature, type='l', xlim=c(2003,2017), xlab="Year", 
     main="ARIMA Prediction of CHI Temperature")
lines(ts(pred$pred, frequency=12, start=2008), col="red")
# MSE
mean((chi$AverageTemperature[1909:1968]-pred$pred[1:60])^2)


#### Harmonic Regression ####
# The purpose of spectral analysis is to decompose a time series into periodic components. 
# regress the time series on a set of sine and cosine waves. 
# For a dataset with annual variation, we might expect that the sine and cosine waves with 
# one year might be important, but what other waves might be present in this time series?
# Harmonic regression: regress the time series on harmonics (waves). 
# I've included here annual harmonics, as well as other harmonics.

# Atlantic multidecadal oscillation: around 50 to 70 years, but unpredictable
# affect the sea surface temperature of the North Atlantic Ocean
# correlated to air temperatures and rainfall over much of the Northern Hemisphere, 
# in particular in the summer climate in North America and Europe.

# Sunspot cycle: about 11 years
# Both long-term and short-term variations in solar activity are hypothesized to affect global climate
# but it has proven extremely challenging to quantify the link between solar variation and climate

# El nino: 2-7 years (3, 3.5, 6 years significant)

climateCycle = function(data, ti){
  yr = seq(1849, 2018, length.out=2029)[-2029]
  # Create dataframe with different harmonic waves
  X = data.frame(Year=yr, Temp=c(data$AverageTemperature, rep(NA, 60)), 
                 sin(2*pi*2*yr), cos(2*pi*2*yr), # 6 months; freq = 2 
                 sin(2*pi*1*yr), cos(2*pi*1*yr), # 1 year; freq = 1 
                 sin(2*pi*1/3.5*yr), cos(2*pi*1/3.5*yr), # 3.5 years; freq = 1/3.5
                 sin(2*pi*1/6*yr), cos(2*pi*1/6*yr), # 6 years; freq = 1/6
                 sin(2*pi*1/11*yr), cos(2*pi*1/11*yr)) # 11 years; freq = 1/11
  
  # Regress 1849-2007 Temp on everything (but Year)
  mod = lm(Temp ~ . - Year, data=X[1:1908,])  
  print(summary(mod))
  
  X$pred = predict(mod, X)
  # Plot predictions from 2008-2017
  p = ggplot(data=subset(X, Year >= 2008)) + 
    geom_line(aes(x=Year, y=Temp)) + 
    geom_line(data=subset(X, Year >= 2008 & Year < 2013), aes(x=Year, y=pred), color="dodgerblue") +
    geom_line(data=subset(X, Year >= 2013), aes(x=Year, y=pred), color="coral") +
    geom_vline(aes(xintercept=2013), lty=2) +
    labs(title=ti, y="Temperature(ºC)")
  print(p)
  # Confidence Interval very small
  # pred = predict(mod, X, interval="confidence")
  # X$pred = pred[,"fit"]
  # X$lwr = pred[,"lwr"]
  # X$upr = pred[,"upr"]
  # geom_ribbon(data = subset(X, Year >= 2013), aes(x=Year, ymin=lwr, ymax=upr), alpha=0.3)
  
  mse = mean((X$Temp[1909:1968] - X$pred[1909:1968])^2)
  return(mse)
}

# half-year and 1-year cycles significant
climateCycle(la, "Harmonic Regression Prediction of LA Temperature")
# half-year, 1-year, 3.5-year, 6-year, and 11-year cycles significant
climateCycle(nyc, "Harmonic Regression Prediction of NYC Temperature")
# half-year, 1-year, 3.5-year, 6-year, and 11-year cycles significant
climateCycle(chi, "Harmonic Regression Prediction of CHI Temperature")

# decent annual prediction, but didn't pick up year-to-year trend

######## Spectral Analysis ########
#### Periodogram ####
# Main Idea: If I have N data, and I include N sines and cosines, 
# then my regression will perfectly predict the data. 
# The regression will be overfitted. 
# But I might learn something by seeing which coefficients are significantly different from zero. 
# This is what the “periodogram” tells us.

period = function(dat, logScale=TRUE, ti){
  rawSpec = spec.pgram(dat$AverageTemperature, taper=0, plot=FALSE)
  # The default plotting shows a confidence interval. 
  # in general, it is difficult to construct meaningful confidence intervals of spectral density estimates.
  # plot(rawSpec) # log scale
  # plot(rawSpec, log="no")
  
  dfSpec = data.frame(freq=rawSpec$freq, spec=rawSpec$spec)
  dfSpec$period = 1/dfSpec$freq
  
  # Create period labels, units are in years
  yrsPeriod = rev(c(1/6, 1/5, 1/4, 1/3, 0.5, 1, 3, 5, 10, 100))
  yrsLabels = rev(c("1/6", "1/5", "1/4", "1/3", "1/2", "1", "3", "5", "10", "100"))
  # Convert annual period to annual freq, and then to monthly freq
  yrsFreqs = 1/yrsPeriod * 1/12  
  
  if(logScale){
    p1 = ggplot(data=subset(dfSpec)) + 
      geom_line(aes(x=freq, y=spec)) + 
      scale_x_log10("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
      scale_y_log10() 
    p2 = ggplot(data=subset(dfSpec)) + 
      geom_line(aes(x=freq, y=spec)) + 
      scale_x_continuous("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
      scale_y_log10() +
      theme(axis.title.y=element_blank())
      grid.arrange(p1, p2, nrow=1, top=ti)
  }else{
    ggplot(data=subset(dfSpec)) + 
      geom_line(aes(x=freq, y=spec)) + 
      scale_x_continuous("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
      scale_y_continuous() +
      labs(title=ti)
  }
}

# log scale more revealing and has theoretical advantages 
# The periodogram values should be approximately normally distributed on the log scale

# 1 year and half year significant
p1 = period(la, logScale=FALSE, ti="LA")
# 1 year significant
p2 = period(nyc, logScale=FALSE, ti="NYC")
# 1 year and half year significant
p3 =period(chi, logScale=FALSE, ti="CHI")
grid.arrange(p1, p2, p3, nrow=2, top="Periodogram of LA, NYC, and CHI Temperatures")

# 1 year, half year, and 1/3 year significant
p1 = period(la, ti="LA")
# 1 year significant
p2 = period(nyc, ti="NYC")
# 1 year, half year, and 1/3 year significant
p3 = period(chi, ti="CHI")
grid.arrange(p1, p2, p3, nrow=3, top="Periodogram of LA, NYC, and CHI Temperatures (Log Scale)")

#### Smoothing #### 
# Fundamental problem with periodogram: Unlike most estimates you've encountered, 
# such as the mean or a regression coefficient, which get more reliable as you collect more data, 
# the periodogram does not get more reliable. 
# As you collect more data, you add more periodogram points, but they are all just as noisy as before.

# plot(kernel("daniell", m = 10))  # A short moving average
# plot(kernel("daniell", m = 50))  # A long moving average
# plot(kernel("daniell", c(5, 5)))  # m=5 moving average of a m=5 moving average

smoothPeriod = function(dat, ti){
  # confidence interval got much narrower
  smoothSpec = spec.pgram(dat$AverageTemperature, kernel=kernel("daniell", c(9, 9, 9)), 
                          taper=0, plot=FALSE)
  dfSpec = data.frame(freq=smoothSpec$freq, `c(9,9,9)`=smoothSpec$spec)
  names(dfSpec) = c("freq", "c(9,9,9)")
  dfSpec[, "c(9,9)"] = spec.pgram(dat$AverageTemperature, kernel=kernel("daniell", c(9, 9)), 
                                  taper=0, plot=FALSE)$spec
  dfSpec[, "c(9)"] = spec.pgram(dat$AverageTemperature, kernel=kernel("daniell", c(9)), 
                                taper=0, plot=FALSE)$spec
  
  dfSpec = melt(dfSpec, id.vars="freq", value.name="spec", variable.name="kernel")
  yrsPeriod = rev(c(1/6, 1/5, 1/4, 1/3, 0.5, 1, 3, 5, 10, 100))
  yrsLabels = rev(c("1/6", "1/5", "1/4", "1/3", "1/2", "1", "3", "5", "10", "100"))
  # Convert annual period to annual freq, and then to monthly freq
  yrsFreqs = 1/yrsPeriod * 1/12 
  
  p1 = ggplot(data=subset(dfSpec)) + 
    geom_path(aes(x=freq, y=spec, color=kernel)) + 
    scale_x_log10("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
    scale_y_log10() + 
    theme(axis.title.x=element_blank())
  
  p2 = ggplot(data=subset(dfSpec)) + 
    geom_path(aes(x=freq, y=spec, color=kernel)) + 
    scale_x_continuous("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
    scale_y_log10() 
  
  grid.arrange(p1, p2, top=ti)
}

# When you smooth the periodogram, then the log spacing on frequency is less necessary.
# smoothing is a good thing when the true spectral density is smooth, and smoothing is a bad thing 
# when the true spectral density is not smooth. Here, smoothing seems to be a good thing everywhere 
# except for the annual frequency, and it's harmonics. The spikes probably shouldn't be smoothed, 
# but it is what it is.

# c(9,9) did the best
smoothPeriod(la, "Smoothed Periodogram of LA Temperature (Log Scale)")
smoothPeriod(nyc, "Smoothed Periodogram of NYC Temperature (Log Scale)")
smoothPeriod(chi, "Smoothed Periodogram of CHI Temperature (Log Scale)")

#### Tapering ####
taper = function(dat){
  smoothSpec = spec.pgram(dat$AverageTemperature, kernel=kernel("daniell", c(9, 9)), 
                          taper=0, plot=FALSE)
  dfSpec = data.frame(freq=smoothSpec$freq, `0%`=smoothSpec$spec)
  names(dfSpec) = c("freq", "0%")
  dfSpec[, "10%"] = spec.pgram(dat$AverageTemperature, kernel=kernel("daniell", c(9, 9)), 
                               taper=0.1, plot=FALSE)$spec
  dfSpec[, "30%"] = spec.pgram(la$AverageTemperature, kernel=kernel("daniell", c(9, 9)), 
                               taper=0.3, plot=FALSE)$spec
  
  dfSpec = melt(dfSpec, id.vars="freq", value.name="spec", variable.name="taper")
  yrsPeriod = rev(c(1/6, 1/5, 1/4, 1/3, 0.5, 1, 3, 5, 10, 100))
  yrsLabels = rev(c("1/6", "1/5", "1/4", "1/3", "1/2", "1", "3", "5", "10", "100"))
  # Convert annual period to annual freq, and then to monthly freq
  yrsFreqs = 1/yrsPeriod * 1/12 
  
  p1 = ggplot(data=subset(dfSpec)) + 
    geom_path(aes(x=freq, y=spec, color=taper)) + 
    scale_x_log10("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
    scale_y_log10()
  
  p2 = ggplot(data=subset(dfSpec)) + 
    geom_path(aes(x=freq, y=spec, color=taper)) + 
    scale_x_continuous("Period (years)", breaks=yrsFreqs, labels=yrsLabels) + 
    scale_y_log10()
  
  grid.arrange(p1, p2)
}

# In practice, 5% from each side often works pretty well. 
# Tapering is less important the longer your time series is, but it can be very important in short series

# didn't seem necessary; kernel smoothing did well enough
taper(la)
taper(nyc)
taper(chi)

#### Multitaper ####
# not necessary here
k = kernel("daniell", c(2))
dfSpec[, "10%"] = spec.pgram(la$AverageTemperature, taper=0.05)$spec

mt.spec = spec.mtm(laTS, nw=16, k=2 * 16 - 1, jackknife=TRUE, dtUnits="month")
# multitaper can resolve frequencies to about +/- NW/N Hz. i.e 16/1518 Hz
# k is typically equal to 2NW - 1.  Higher k is smoother
mt.spec = spec.mtm(la$AverageTemperature, nw=16, k=2 * 16 - 1, jackknife=TRUE, deltat=1/12, dtUnits="year")

#### Wavelet ####
wave = function(dat){
  timeString = strptime(dat$dt, "%Y-%m-%d", tz="UTC")
  dat$Year = as.numeric(format(timeString, "%Y")) + (as.numeric(format(timeString, "%m"))-1)/12
  # frequency estimation
  wave.out = morlet(y1=dat$AverageTemperature, x1=dat$Year, p2=8, dj=0.1, siglvl=0.95)
  # p2=6 <=> estimate out to 2^8 = 256 months dj <=> controls the frequency
  # resolution hack the period estimate to be in years, not months
  wave.out$period = wave.out$period/12
  levs = quantile(wave.out$Power, c(0, 0.25, 0.5, 0.75, 0.95, 1))
  wavelet.plot(wave.out, wavelet.levels=levs, crn.ylim=c(22.5, 30))
  
  wave.avg = data.frame(power=apply(wave.out$Power, 2, mean), period=(wave.out$period))
  #plot(wave.avg$period, wave.avg$power, type = "l")
}

wave(la)
wave(nyc)
wave(chi)


