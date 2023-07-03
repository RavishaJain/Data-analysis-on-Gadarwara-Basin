rm(list = ls())
####### Setting the working Directory ###########################
setwd("C:/Users/HP/Downloads")
##### reading the data as dataframe#############
df_1<-read.csv("Gadarwara.csv")
##### converting date and time #####################
df_1$Date<-as.Date(df_1$Date,format="%d-%m-%Y")
df_1$day<-as.numeric(format(df_1$Date,'%d'))
df_1$month<-as.numeric(format(df_1$Date,"%m"))
df_1$year<-as.numeric(format(df_1$Date,"%Y"))
class(df_1)   # For checking the class of df_1
####### Creating vectors###########################
s_1=c()  
s_2=c()
s_3=c()
s_4=c()
s_5=c()
############## Agrregating the monthly precipitation data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d1<-sum(df_1[which(df_1$year==i & df_1$month==j),]$P)
    s_1<-append(s_1,d1)
  }
}
############## Agrregating the monthly PET data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d2<-sum(df_1[which(df_1$year==i & df_1$month==j),]$E)
    s_2<-append(s_2,d2)
  }
}
############## Agrregating the monthly Streamflow data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d3<-mean(df_1[which(df_1$year==i & df_1$month==j),]$Q)
    s_3<-append(s_3,d3)
  }
}
############## Converting the above data into data frames #####################
df_3<-matrix(unlist(s_2),nrow=456,ncol=1)
df_3<-data.frame(df_3)

df_2<-matrix(unlist(s_1),nrow=456,ncol=1)
df_2<-data.frame(df_2)

df_4<-matrix(unlist(s_3),nrow=456,ncol=1)
df_4<-data.frame(df_4)

################## Adding months and year to the data #################
for(i in min(df_1$year):max(df_1$year)){
  for( j in 1:12){
    s_4<-append(s_4,j)
    s_5<-append(s_5,i)
}
}

df_5<-matrix(unlist(s_4),nrow=456,ncol=1) ######### Converting it to data frame ############
df_6<-matrix(unlist(s_5),nrow=456,ncol=1)
df_5<-data.frame(df_5)  
df_6<-data.frame(df_6)

############# Creating the monthly Time-Series ####################### 

TimeSeries<-cbind(df_6,df_5,df_2,df_3,df_4)
colnames(TimeSeries)<-c("Year","month","P","E","Q ")
############## Plotting the Graphs ###############################################
par(mfrow=c(4,3))
############## PLotting Precipitation V/s Streamflow ###########################
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$P,df_7$Q,col="Red",xlab="Precipitation",ylab="Streamflow",pch=19)
}

par(mfrow=c(4,3))
################## Plotting PET V/s Streamflow ##############################
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$E,df_7$Q,col="Blue",ylab="Streamflow",xlab="PET",pch=19)
}
