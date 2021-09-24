#VP Start Here
## fitting log-log model

library(lme4)
library(lubridate)
setwd("C:\\Users\\C938474\\Documents\\ChinaProv")
data <- read.csv("ChinaADSnatylog.csv", header = TRUE)

colnames(data)[1:3]<-c("Province","Month","Beer_Volume")
data = cbind(data, format_date = as.Date(as.character(data$Month),"%d/%m/%Y"))
### subsetting data based on beer volume availability 
#data <- data[1:sum(!is.na(data$Beer_Volume)),]
data<-subset(data,!is.na(data$Beer_Volume))
data_copy<-subset(data, year(format_date) >= 2005 & year(format_date) <= 2015)
data_backup = subset(data_copy, year(format_date) >= 2005 & year(format_date) <= 2014)

fit1 <- lmer(Beer_Volume  ~  May + Jun + Jul + Aug + Sep + smf + 
               #min_avg_temp_f +
               #CPI_f +
               #GDP_per_capita_naty +
               #Population_15_45_naty	+ 
               #CPI_RealAvgBeerPrice_naty +	
               #temp_naty +
               #pmi_naty +
               #same vars +  
               (1 | Province) + 
               (0 + Unemp_rate_urban | Province) +
               #(0 + GRP | Province) +
               #(0 + HCE | Province) +
               #(0 + FAI | Province) +
               #(0 + REI | Province) +
               #(0 + Population_res_ye | Province) +
               #(0 + Pop_all | Province) +
               #(0 + Pop_rural | Province) +
               #(0 + Pop_urban | Province) +
               #(0 + Pop0to14 | Province) +
               (0 + Pop15to64 | Province) +
               #(0 + Pop65Plus | Province) +
               (0 + CPI_f | Province) +
               #(0 + RPI | Province) +
               (0 + F_visitors | Province) +
               #(0 + Ov_visit_arriv | Province) +
               #(0 + grp_a | Province) +
               #(0 + grp_index | Province) +
               #(0 + RegSurf_sqkm | Province) +
               (0 + pop_density | Province) +
               #(0 + pop_density_gi | Province) +
               #(0 + festivedays | Province) +
               #(0 + prcp_f | Province) +
               #(0 + max_temp_f | Province) +
               #(0 + min_temp_f | Province) +
               #(0 + max_avg_temp_f | Province) +
               (0 + min_avg_temp_f | Province) +
               #(0 + snwd_f | Province) +
               
               (0 + ny | Province) +
               #(0 + christ | Province) +
               #(0 + ld | Province) +
               #(0 + cny | Province) +
               #(0 + gweek | Province) +
               #(0 + qmj | Province) +
               (0 + nd | Province) +
               #(0 + bolym | Province) +
               (0 + cgp | Province) +
               #(0 + qbf | Province) +
               (0 + sbf | Province) +
               #(0 + srm | Province) +
               #(0 + smf | Province) +
               
               
               #(0 + Jan | Province) +
               (0 + Feb | Province) +
               (0 + Mar | Province) +
               #(0 + Apr | Province) +
               #(0 + May | Province) +
               #(0 + Jun | Province) +
               #(0 + Jul | Province) +
               #(0 + Aug | Province) +
               #(0 + Sep | Province) +
               #(0 + Oct | Province) +
               (0 + Nov | Province),
             
             
             #(0+min_avg_temp_f | Province), 
             
             data = data_backup, REML = FALSE)

#fixedeff()

coeff1 <- coef(fit1)$Province
coefs <- data.frame(coef(summary(fit1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


p <- predict(fit1, data_copy)
p1 <- data.frame(Province = data_copy$Province, Month = data_copy$Month, Beer_Volume = data_copy$Beer_Volume, predicted = p)
write.csv(p1,"mixedoutput.csv")
write.csv(coeff1,"coeff.csv")
write.csv(coefs,"fixedcoeff.csv")

#p <- exp(predict(fit1, Cluster1)) * Cluster1$seasonal_index * Cluster1$Production.Days
#p1 <- data.frame(State = Cluster1$State, Year = Cluster1$Year, Month = Cluster1$Month, predicted = p)


#######Clustering#######

forcm<-read.csv(choose.files())
row.names(forcm) <- forcm$X
forcm <- forcm[, -1]

cor_matrix = cor(forcm, use="pairwise.complete.obs")
CM<-cor_matrix
#CM <- read.csv(choose.files())
#row.names(CM) <- CM$X
CM <- CM[, -1]
diss <- 1-abs(CM)
diss <- as.dist(diss)
clust <- hclust(diss)
n = 6
c <- cutree(clust, k = n)
d <- as.data.frame(c)
d <- data.frame(Province = row.names(d), Cluster = d$c)
clusters <- split(d, f = d$Cluster)
clusters
clusters[[1]]
plot(clust)


####################################################
#############