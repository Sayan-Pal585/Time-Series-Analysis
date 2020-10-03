##Time Series Date Manipulation
##Install Packages##
if(!require(lubridate)){install.packages("lubridate")};library("lubridate", lib.loc="~/R/win-library/3.4")

#--------Setup Work Directory---------#
setwd("File Directory") ##Set file directory
getwd()

##Importing Data##
iowa<-read.csv(file.choose(),header=TRUE)## read data/import data
View(iowa)
attach(iowa)
str(iowa)

##missing value check##
apply(iowa, 2, function(x){sum(is.na(x))})
names(iowa)
Iowa<-iowa[,c(2,21)]## taking two columns out
View(Iowa)
apply(Iowa, 2, function(x){sum(is.na(x))})

## changing to date format##
Date<-as.Date(Iowa$Date,format="%m/%d/%Y")

Iowa0<-data.frame(Date,Iowa$Bottles.Sold)
View(Iowa0)
str(Iowa0)

##Monthly cut##
Month<-as.Date(cut(Iowa0$Date,breaks = "month"))
##Weekly Cut##
iowa4$week<-as.Date(cut(iowa4$iowa3.iowa1.date,breaks = "week",start.on.monday = FALSE))

# changes weekly break point to Sunday
Iowa1<-data.frame(Month,Iowa0$Iowa.Bottles.Sold)
View(Iowa1)
Iowa2<-Iowa1[order(as.Date(Iowa1$Month, format="%Y-%m-%d")),]
View(Iowa2)

Iowa3 <- aggregate(Iowa2$Iowa0.Iowa.Bottles.Sold~month(Iowa1$Month)+year(Iowa1$Month),
                   data=Iowa2,FUN=sum)
View(Iowa3)
write.csv(Iowa3,'iowa_monthly.csv')

Iowa_4<-data.frame(Iowa3$`year(Iowa1$Month)`,Iowa3$`Iowa2$Iowa0.Iowa.Bottles.Sold`)
View(Iowa_4)
## yearly
Iowa_5 <- aggregate(Iowa_4$Iowa3..Iowa2.Iowa0.Iowa.Bottles.Sold.~Iowa_4$Iowa3..year.Iowa1.Month..,
                    data=Iowa_4,FUN=sum)
View(Iowa_5)

iowa6<-data.frame(iowa4$week,iowa4$iowa3.iowa.Bottles.Sold)
View(iowa6)
iowals<-iowa6[order(as.Date(iowa6$iowa4.week, format="%Y-%m-%d")),]
View(iowals)

byweek <- aggregate(iowals$iowa4.iowa3.iowa.Bottles.Sold~iowals$iowa4.week, data = iowals, sum)
View(byweek)
plot(byweek,type='l')

write.csv(byweek,file = 'county(Polk)-zip-50321-week.csv')

date<-seq(as.Date("2012/1/1"), as.Date("2017/10/1"), "months")
Iowa4<-data.frame(date,Iowa3$`Iowa2$Iowa0.Iowa.Bottles.Sold`)
View(Iowa4)
Iowa.ts<-ts(Iowa3$`Iowa2$Iowa0.Iowa.Bottles.Sold`,frequency = 12,start = c(2012,01))##monthly analysis
View(Iowa.ts)

xmin<-min(iowa6$iowa4.week,na.rm=T)
xmax<-max(iowa6$iowa4.week,na.rm=T) 

##Code End##