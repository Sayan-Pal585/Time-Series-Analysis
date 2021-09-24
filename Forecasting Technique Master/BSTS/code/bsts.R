install.packages("bsts",dependencies = T)
library('latex2exp')
options( warn = -1 )
options(repr.plot.width=7, repr.plot.height=3)
library("bsts")

##########Reading the dataset########
lounge_data = read.csv('C:/Users/admin/Documents/SIA/Lounge_Analytics/ADS/ADS_exogenous_V1.csv', header=TRUE, stringsAsFactors=FALSE)

names(lounge_data)[1] <- "entry_date"
lounge_data$entry_date = as.Date(lounge_data$entry_date)

################Splitting the data set
train <- ts(na.omit(lounge_data[1:386,2:14]), frequency = 7)
test <- ts(na.omit(lounge_data[387:415,2:14]), frequency = 7)

#####Adding Linear Trend component###
ss <- AddLocalLinearTrend(list(), train)

#####Adding Seasonal Component####
ss <- AddSeasonal(ss, train, nseasons=7)
####Defining the formulae####
formula1 <- 'visitors_in_lounge ~ Medium_Haul_flights + visitors_nonSQMI_lounge + perc_visitors_having_ffpno + perc_new_members + EUR_region_flight + guests_BusinessCLasslounge + seasonal365 + Flag_sun_mon + Public_holiday_new + Day_after_holiday + Post_holiday_block + Pre_holiday_block_new_v1'

#####Adding regressor Component


#####Building the Model#####
bay <- bsts(formula = formula1, niter =100, data= train, state.specification = ss)

names(bay)

#####Check performed to see whether the model converged or not through plotting####
options(repr.plot.width=7, repr.plot.height=5)
plot(bay,
     main=TeX('Conditional Expectation of $y_i$'),
     xlab='days',
     ylab='distribution')

par(xpd=TRUE)
legend("topleft", legend=c(TeX('$E(y_i|data)$'), "data"),
       col=c("black", "blue"), lty=c(1,NA), pch=c(NA, 1), cex=1)

######Plotting the various components of the series#####

plot(bay, "components")

#######Making predictions######
pred1 <- predict(bay, h=30, newdata = test, na.action = "ignore")

########In-Sample Accuracy############
accuracy(train[,2],as.numeric(bay$final.state))

########Out-Sample Accuracy#########
accuracy(test[,2],as.numeric(pred1$mean))


plot(c(pred1$original.series,pred1$mean))

#####Creating dataframe
forecasted <- c(as.numeric(pred1$original.series), as.numeric(pred1$mean))
forecasted<-as.data.frame(forecasted)

forecasted_f_<-cbind.data.frame(actual=lounge_data[1:415,2],forecasted=forecasted)

write.csv(forecasted_f_,"C:/Users/admin/Documents/SIA/Lounge_Analytics/bsts/bsts_model_iter_1.csv",row.names = F)
