##cerating the data for intermittent demand analysis(using croston's method)
prd <- c(1910,874,1920,350,160,685,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,176,0,16,826,0,66,3798,800,1274,638,192,160,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28,0,0,276,0,0,1072,80,1776,240,80,528,3081,566,1483,112,272,120,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,160,0,808,0,0,608,0,1480,184)
View(prd)

##convert to time series data
t <- ts(prd, f=52)

##load package for intermittent demand analysis
library("tsintermittent", lib.loc="~/R/win-library/3.5")
x<-crost(t,h=52)

x

##plotting the intermittent demand data analysis
bar <- crost(t)
plot(t,xlim=c(1,4.1))
lines(ts(bar$frc.in,frequency=52),col="red")
lines(ts(bar$frc.out,frequency=52,start=c(3,49)),col="green")

foo <- stlf(t)
foo$mean <- pmax(foo$mean,0)    # truncate at zero
plot(foo)

##---------------------------------------------------------------------------------------------------------------------
##pre loaded intermittent demand time series data
dtta<-ts.data1
View(dtta)

str(dtta)
plot(dtta)

z<-crost.decomp(dtta)
z

z1<-crost(dtta,h=30)

plot(z1,xlim=c(1,4.1))
lines(ts(z1$frc.in,frequency=30),col="red")
lines(ts(z1$frc.out,frequency=30,start=c(3,49)),col="green")
