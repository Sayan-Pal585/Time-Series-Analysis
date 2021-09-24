############################################################################
############################################################################
# CODE: WEATHER DATA PULL
# AUTHOR: VEDANT PRASAD - GAC BANGALORE
# DATE: 3RD AUG 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#Install required packages

if(!require(weatherData)){install.packages("weatherData")}
if(!require(car)){install.packages("car")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(fmsb)){install.packages("fmsb")}
if(!require(MASS)){install.packages("MASS")}
if(!require(QuantPsyc)){install.packages("QuantPsyc")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(combinat)){install.packages("combinat")}
if(!require(sqldf)){install.packages("sqldf")}

############################################################################
#Set working directory

setwd("C:\\Users\\C938474\\Documents\\GFF\\UCM")

############################################################################
#Read input file

WeightsByStation <-read.csv(file.choose(),header=T)

############################################################################
#Enter years for which data is required

years <- c(2015:2017)

# Enter lower and upper limits for optimum rainfall levels in mm

lower_rFall<-10
upper_rFall<-20

############################################################################
######################      CODE STARTS HERE           #####################
############################################################################

# Before applying this function (getWeatherForYear) read more about the package
#?getWeatherForYear

stations<-as.character(WeightsByStation[,1])

# Creating empty data frame
getData <- data.frame(Date=character(),
                      Max_TemperatureF=double(),
                      Mean_TemperatureF=double(),
                      Min_TemperatureF=double(),
                      PrecipitationIn=double(),
                      Station=character(),
                      stringsAsFactors = F)



#Pulling Weather data
# If we make opt_detailed = T it will download all the columns and will take lot of time, hence manual
# method is faster. opt_detailed = F will give only few columns which we need 
# Here data_flag = 0 means data is not available for given internal and 1 means data is available

for(station_id in stations){
  for (year in years){
    start_date <- paste0(year,"-01-01")
    end_date <- as.Date(min(as.Date(paste0(year,"-12-31")), Sys.Date()))
    data_flag <- checkDataAvailabilityForDateRange(station_id, start_date, end_date)
    if(data_flag == 1){
      tempData <- NULL
      tempData <- getSummarizedWeather(station_id = station_id, start_date = start_date,
                                       end_date = end_date,
                                       opt_custom_columns=TRUE,
                                       custom_columns=c(2,3,4,20))
      tempData$Station <- station_id
      getData <- rbind(getData, tempData) 
    }
  }
}

View(getData)

############################################################################
#Extracting month and year

getData$Date<-as.Date(as.character(getData[,1]),format="%Y-%m-%d")
getData$Month<-month(getData$Date)
getData$Year<-year(getData$Date)

############################################################################
#Creating flags for High, Medium and Low rainfall

getData$HighPreciFlag<-ifelse(getData$Precipitationmm>=upper_rFall,1,0)
getData$MedPreciFlag<-ifelse(getData$Precipitationmm>=lower_rFall & getData$Precipitationmm<upper_rFall,1,0)
getData$LowPreciFlag<-ifelse(getData$Precipitationmm<lower_rFall,1,0)

############################################################################
#Aggregating by month

by_vars <- group_by(getData,Station, Month, Year)
agg<-getData %>%
  group_by(Station, Month, Year) %>%
  summarize(
    Max_Temp=mean(Max_TemperatureC),
    Mean_Temp=mean(Mean_TemperatureC),
    Min_Temp=mean(Min_TemperatureC),
    Precipitation=mean(Precipitationmm),
    HighPrecDays=sum(HighPreciFlag),
    MedPrecDays=sum(HighPreciFlag),
    LowPrecDays=sum(HighPreciFlag)
  )

############################################################################
#Weighting by population ratios

weight<-merge(x=agg,y=WeightsByStation,by="Station",all.x=TRUE)
View(weight)

weight2<-data.frame(Station = weight$Station,
                    Month = weight$Month,
                    Year = weight$Year,
                    Max_temp_w = weight$Weight*weight$Max_Temp,
                    Mean_temp_w = weight$Weight*weight$Mean_Temp,
                    Min_temp_w = weight$Weight*weight$Min_Temp,
                    Preci_w = weight$Weight*weight$Precipitation,
                    HighPrecDays_w=weight$Weight*weight$HighPrecDays,
                    MedPrecDays_w=weight$Weight*weight$MedPrecDays,
                    LowPrecDays_w=weight$Weight*weight$MedPrecDays
)

############################################################################
#Aggregating across Stations i.e. to country level

by_vars2 <- group_by(weight2,Month, Year)
agg2<-weight2 %>%
  group_by(Month, Year) %>%
  summarize(
    Max_Temp_wt=sum(Max_temp_w),
    Mean_Temp_wt=sum(Mean_temp_w),
    Min_Temp_wt=sum(Min_temp_w),
    Preci_wt=sum(Preci_w),
    HighPrecDays_wt=sum(HighPrecDays_w),
    MedPrecDays_wt=sum(MedPrecDays_w),
    LowPrecDays_wt=sum(LowPrecDays_w)
  )

View(agg2)


############################################################################
#Writing final dataset

write.csv(agg2,("Aggregated_WeatherData.csv"),row.names = F)


############################################################################
######################      CODE ENDS HERE             #####################
############################################################################
# Reference
# https://cran.r-project.org/web/packages/weatherData/weatherData.pdf