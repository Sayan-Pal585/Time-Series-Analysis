############################################################################
############################################################################
# CODE: UNIVARIATE REPORT GENERATOR
# AUTHOR: DIPANKAR DOLEY - GAC BANGALORE
# DATE: 16TH JUN 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################

country<-"Australia"

#############################SET DIRECTORY##################################

setwd("C:\\Users\\C938474\\Documents\\GFF\\Merging")

###################### IMPORT RAW DATA #####################################

monthly<-read.csv(file.choose(),header=T, na.strings = c("","NA"))
quarterly<-read.csv(file.choose(),header = T, na.strings = c("","NA"))
yearly<-read.csv(file.choose(),header = T, na.strings = c("","NA"))

####################### BACKUP OF DATA #####################################

m=monthly
q=quarterly
y=yearly

############################################################################
######################      CODE STARTS HERE           #####################
############################################################################

m$Month<-as.Date(m$Month,"%m/%d/%Y")

### COLLECTING THE GRANULARITY AGAINST VARIABLE NAMES
g_y<-cbind.data.frame(Granularity="Yearly",Col_names=colnames(y)[-1])
g_q<-cbind.data.frame(Granularity="Quarterly",Col_names=colnames(q)[-c(1:2)])
g_m<-cbind.data.frame(Granularity="Monthly",Col_names=colnames(m)[-c(1:3)])
g_all<-rbind.data.frame(g_m,g_q,g_y)

############################################################################
####################### CONSOLIDATION OF DATA ##############################

## MERGE QTR WITH MONTHLY
mrg_dt<-merge.data.frame(x=m,y=q,by = c("Year","Quarter"),all.x = T)

## MERGE YEAR WITH YEARLY
mrg_dt<-merge.data.frame(x = mrg_dt,y = y,by = "Year",all.x = T)

## REMOVING 1ST TWO COLUMNS
mrg_dt<-mrg_dt[,-which(colnames(mrg_dt)==c('Year','Quarter'))]

############################################################################
############################ UNIVARIATE ANALYSIS ###########################
a1=mrg_dt

cnt=0;r=2
x_d1<-NULL;x_d<-NULL
for(r in 2:ncol(a1)){
  tryCatch({
    
    if(class(a1[,r])=="factor" |class(a1[,r])=="character" ){
      
      Data_Type<-class(a1[,r])
      num_obs<-length(a1[,r])
      miss<-sum(is.na(a1[,r])) 
      miss_pc<-100*miss/nrow(a1)
      mean<-NA
      median<-NA 
      sd<-NA
      qtl<-NA
      num_unique_obs<-length(unique(na.omit(a1[,r])))
      cnt2=0
      for(i in 1:num_unique_obs){
        if(cnt2==0){
          res<-data.frame(table(a1[,r]))[,2][i]
          nam<-data.frame(table(a1[,r]))[,1][i]
          r_f<-paste(nam,res,sep = ":-")
          cnt2=cnt2+1
        }else{
          res<-data.frame(table(a1[,r]))[,2][i]
          nam<-data.frame(table(a1[,r]))[,1][i]
          r_f_1<-paste(nam,res,sep = ":-")
          r_f<-rbind(r_f,r_f_1)            
        }
      }
      
      factor_unique_level_obs<-paste(unlist(r_f),collapse=";  ")
      
    }else{
      
      Data_Type<-class(a1[,r])
      num_obs<-length(a1[,r])
      miss<-sum(is.na(a1[,r])) 
      miss_pc<-100*miss/nrow(a1)
      mean<-mean(a1[,r],na.rm = T)
      median<-median(a1[,r],na.rm = T)
      sd<-sd(a1[,r],na.rm = T)
      qtl<-as.numeric(quantile(a1[,r],prob=seq(0,1,0.05),na.rm = T))
      num_unique_obs<-NA
      factor_unique_level_obs<-NA
    } #If loop factor
    
    x_d<-cbind.data.frame(Variable_Names=names(a1[r]),Data_Type,num_obs,miss,miss_pc,mean,median,sd,
                          min=qtl[1],qtl_5th=qtl[2],qtl_10th=qtl[3],qtl_15th=qtl[4],
                          qtl_20th=qtl[5],qtl_25th=qtl[6],qtl_30th=qtl[7],qtl_35th=qtl[8],
                          qtl_40th=qtl[9],qtl_45th=qtl[10],qtl_50th=qtl[11],
                          qtl_55th=qtl[12],qtl_60th=qtl[13],qtl_65th=qtl[14],qtl_70th=qtl[15],
                          qtl_75th=qtl[16],qtl_80th=qtl[17],qtl_85th=qtl[18],
                          qtl_90th=qtl[19],qtl_95th=qtl[20],max=qtl[21],
                          num_unique_obs,factor_unique_level_obs)
    
    #rownames(x_d)<-names(a1[r])
    
    if(cnt==0){
      x_d1<-x_d
      cnt=cnt+1
    }else{
      x_d1<-rbind.data.frame(x_d1,x_d)
    }},error=function(e){cat("Error:","Occured in Column-",r,"-in your Data","\n")})
}

############################################################################
### ADDING GRANULARITY IN THE DATA

x_d_f=merge.data.frame(x=x_d1,y=g_all,by.x = "Variable_Names",by.y = "Col_names",all.x = T)

### ORDERING COLUMNS

x_d_f<-x_d_f[,c(ncol(x_d_f),1:31)]

############################################################################
today<-Sys.Date()
############################## OUTPUT/SUMMARY ##############################

write.csv(mrg_dt,paste("Merged_data_",country,today,".csv"))
write.csv(x_d_f,paste("Univariate_",country,today,".csv"))

############################################################################
######################      CODE ENDS HERE             #####################
############################################################################