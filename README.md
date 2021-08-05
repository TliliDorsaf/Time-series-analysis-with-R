# Time-series-analysis-with-R
Time series analysis is the study of a collection of data over a specific time interval. It is a statistical technique that identifies the common pattern of data over time. </br>
The data must be ordered chronologically over a specific time interval. </br>
This data A2 and B2 were provided to us by our university professor and this work was done for college credit. </br>
In the files "Description Data" you'll find the discription of the data A2 and B2 as well as the time interval for each data. </br>
Now let's start analysing.
the teacher asked us to do these steps:
![Test Image ](/description.JPG)

This project was written in jupyter notebook with R.</br>
If you want to know how to add an R envirement in jupyter go here: </br>
https://docs.anaconda.com/anaconda/navigator/tutorials/r-lang/ </br>
First I import our data and create the time series
```R
A<-read.table(file= file.choose(),header=F)
A.ts<-ts(A, start =c(2001, 1),end=c(2020, 1), freq=12)
```
Next step is to plot the Graph and its acf, acf is the plot of the autocorrelation of the time series by lag. </br>
```R
plot(A.ts)
acf(A.ts)
```
We can notice from the plot that the time series A2 had a trend and a saisonality. </br>
After that I decided to take a look at the lag. lag.plot will help us determin if the data is random or not but since we already know that it's not random there was no use to visualize the plot. </br>
Now we have to determin which model to use additive or multiplicative and from the result I determined that the adequat model is the multiplicative one (view the notebook).</br>
After that, the next step is to adjust the series to remove the seasonal component. </br>
```R
#adjustment
t<-c(1:length(A.ts))
s.t<-(t-mean(t))/sd(t) 
CS<-matrix(0,length(A.ts),6)
SN<-matrix(0,length(A.ts),6)
for (i in 1:6) CS[,i]<-cos(2*pi*t/(12/i))
for (i in 1:6) SN[,i]<-sin(2*pi*t/(12/i))
test1<-lm(A.ts~s.t+I(s.t^2)+CS+SN)
summary(test1)
# values of p-value of CS6 and SN6> 0.05
# We must eliminate the insignificant coefficients !!
```
I ran the summary command to visualize the values of p-value of every coefficient.</br>
Any coefficient that has the p-value over 0.05 must be eliminated so I did:
```R
test2<-lm(A.ts~s.t+I(s.t^2)+CS[,-6]+SN[,-6])
```
Now we have to study the time series' stationarity. To do so i used the "adf.test" command, which does the Augmented Dickey-Fuller Test. </br>
If this test gives a p-value over 0.05 then the time series is not stationary and we must differentiate the series. </br>
```R
#Stationaryization of the series of residuals by a differentiation of order 1
res.diff<-diff(sp.res) 
plot.ts(res.diff) 
adf.test(res.diff) 
Box.test(res.diff)
#interpretation:
# p-value <0.05 ==> the differentiated series is stationary
# whiteness test / p-value <0.05 ==> the differentiated series is not
# not assimilable to white noise
# ==> We must model the differentiated series
```
Now the last step is to model the series using an ARIMA model.</br>
To know which coefficient to use, I used the "auto.arima()" command. </br>
For the second dataset B2, the series was already stationary so all I had to do is verify, pick an ARIMA model and use it.</br>
Thank you for your time!</br>
