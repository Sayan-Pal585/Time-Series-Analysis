##polk county daily data input
polk0<-read.csv(file.choose(),header=TRUE)
View(polk0)
str(polk0)
attach(polk0)

##missing value check
apply(polk0, 2, function(x){sum(is.na(x))})

Date<-as.Date(polk0$ï..PUBLIC.IOWA_LIQUOR_SALES.DATE,format="%Y-%m-%d")

polk1<-data.frame(Date,polk0$SUM.BOTTLES_SOLD.)
View(polk1)
str(polk1)

##framing the data into month
month<-as.Date(cut(polk1$Date,breaks = "month"))

polk2<-data.frame(month,polk1$polk0.SUM.BOTTLES_SOLD.)
View(polk2)

##aggregating the polk data on monthly basis
bymonth <- aggregate(polk2$polk1.polk0.SUM.BOTTLES_SOLD.~polk2$month, data = polk2, sum)
View(bymonth)
str(bymonth)

plot(bymonth,type="l")

##Build time series
polk.ts<-ts(bymonth$`polk2$polk1.polk0.SUM.BOTTLES_SOLD.`,start = c(2012,01),end = c(2017,10),frequency = 12)

##outlier analysis(Rosner's Outlier Test)

## Input data.
y = polk.ts

## Generate normal probability plot.
qqnorm(y)

## Create function to compute the test statistic.
rval = function(y){
  ares = abs(y - mean(y))/sd(y)
  df = data.frame(y, ares)
  r = max(df$ares)
  list(r, df)}

## Define values and vectors.
n = length(y)
alpha = 0.05
lam = c(1:70)
R = c(1:70)

## Compute test statistic until r=70 values have been
## removed from the sample.
for (i in 1:70){
  
  if(i==1){
    rt = rval(y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  
  else if(i!=1){
    rt = rval(newdf$y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  
  ## Compute critical value.
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
  
}
## Print results.
newdf = data.frame(c(1:70),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf

##Code End##







