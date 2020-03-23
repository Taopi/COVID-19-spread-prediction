## packages
list.of.packages <- c("readr", "forecast", "pracma","Metrics","readr","dplyr","tibble","reshape","zoo","googledrive","tidyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### read data
time_range  <- seq(as.Date("2020-02-27"), as.Date(Sys.time()),"days")
time_range <- as.data.frame(time_range)
rownames(time_range) <-time_range[,1]
colnames <- c("Date","AG","AI","AR","BE","BL","BS","FR","GE","GL","GR","JU","LU","NE","NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS","ZG","ZH", "CH")


#read availuabe data sources and map to same nrow()

# offical data
urlfile <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Cases_Cantons_CH_total.csv"
offical<- read.csv(url(urlfile))
offical <- offical[,c("date","canton","tested_pos")]
colnames(offical)<-c("Date","canton","tested_pos")
offical <- na.omit(offical)
library(reshape)
offical <- cast(offical, Date ~ canton)
rownames(offical) <- offical[,1]
offical <- merge(time_range, offical,by="row.names",all.x=TRUE)
offical <- offical[-(2:3)]
colnames(offical)[1] <- "Date"
offical <- offical[,colnames]


# data scraped from twitter by daenuprobst  
urlfile <- "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland.csv"
scraped <- read.csv(url(urlfile))
scraped <- scraped[,colnames]
rownames(scraped) <- scraped[,1]
scraped <- merge(time_range, scraped,by="row.names",all.x=TRUE)
scraped <- scraped[,-(2:3)]
colnames(scraped)[1] <- "Date"
scraped <- scraped[,colnames]
#scraped <- scraped[-nrow(scraped),] #test

# data predicted yesterday until today
urlfile <- "https://raw.githubusercontent.com/Taopi/COVID-19-spread-prediction/master/covid19_cases_switzerland_forecast.csv"
yesterdays_prediction <- read.csv(url(urlfile))
yesterdays_prediction<-yesterdays_prediction[1:nrow(time_range),]
colnames(yesterdays_prediction)[1] <- "Date"
yesterdays_prediction <- yesterdays_prediction[,colnames]

#create an empty data frame
empty <- data.frame(matrix(ncol=length(colnames), nrow=nrow(time_range)))
colnames(empty) <- colnames
empty[,"Date"] <-time_range
empty[1,2:ncol(empty)]<-0

#fill empty of today with: A) offical data or B) scraped data or c) yesterdays prediction (only for today!)
for(k in 2 : ncol(empty) ) {
  empty[is.na(empty[,k]),k] <- offical[is.na(empty[,k]),k]
  empty[is.na(empty[,k]),k] <- scraped[is.na(empty[,k]),k]
  empty[nrow(time_range),k] <- yesterdays_prediction[nrow(time_range),k]
}

#fill empty with offical, scraped, predicted data, impute missing using na.approx()
full <- empty
for(k in 1 : ncol(empty) ) {
  full[,k] <- round(na.approx(empty[,k]))
}

library(pracma)
library(Metrics)
library(readr)
library(tibble)

covid19_cases_switzerland <- full
#all<-all[1:nrow(all)-1,]
for(i in 1:3) {
  all <- covid19_cases_switzerland
  colnames(all)[1] <- paste("date")
  all <- as_tibble(all)
  #all$X1<-NULL
  date<-all[,1]
  date[nrow(date) + 1,1] <-all[nrow(all),1]+1
  pred_all<-NULL
  for (n in 2:ncol(all)-1) {
    Y<-ts(data = all[n+1], start = 1, end =nrow(all)+1)  
    sig_w<-0.01
    w<-sig_w*rnorm(100) # acceleration which denotes the fluctuation (Q/R) rnorm(100, mean = 0, sd = 1)
    sig_v<-0.01
    v<-sig_v*rnorm(100)
    t<-0.45
    phi<-matrix(c(1,0,t,1),2,2)
    gama<-matrix(c(0.5*t^2,t),2,1)
    H<-matrix(c(1,0),1,2)
    #Kalman
    x0_0<-p0_0<-matrix(c(0,0),2,1)
    p0_0<-matrix(c(1,0,0,1),2,2)
    Q<-0.01
    R<-0.01
    X<-NULL
    X2<-NULL
    pred<-NULL
    for (i in 0:nrow(all)) {
      namp <-paste("p", i+1,"_",i, sep = "")
      assign(namp, phi%*%(get(paste("p", i,"_",i, sep = "")))%*%t(phi)+gama%*%Q%*%t(gama))
      namk <- paste("k", i+1, sep = "")
      assign(namk,get(paste("p", i+1,"_",i, sep = ""))%*%t(H)%*%(1/(H%*%get(paste("p", i+1,"_",i, sep = ""))%*%t(H)+R)))
      namx <- paste("x", i+1,"_",i, sep = "")
      assign(namx,phi%*%get(paste("x", i,"_",i, sep = "")))
      namE <- paste("E", i+1, sep = "")
      assign(namE,Y[i+1]-H%*%get(paste("x", i+1,"_",i, sep = "")))
      namx2 <- paste("x", i+1,"_",i+1, sep = "")
      assign(namx2,get(paste("x", i+1,"_",i, sep = ""))+get(paste("k", i+1, sep = ""))%*%get(paste("E", i+1, sep = "")))
      namp2 <- paste("p", i+1,"_",i+1, sep = "")
      assign(namp2,(p0_0-get(paste("k", i+1, sep = ""))%*%H)%*%get(paste("p", i+1,"_",i, sep = "")))
      X<-rbind(X,get(paste("x", i+1,"_",i,sep = ""))[1])
      X2<-rbind(X2,get(paste("x", i+1,"_",i,sep = ""))[2])
      if(i>2){
        remove(list=(paste("p", i-1,"_",i-2, sep = "")))
        remove(list=(paste("k", i-1, sep = "")))
        remove(list=(paste("E", i-1, sep = "")))
        remove(list=(paste("p", i-2,"_",i-2, sep = "")))
        remove(list=(paste("x", i-1,"_",i-2, sep = "")))
        remove(list=(paste("x", i-2,"_",i-2, sep = "")))}
    }
    pred<-NULL
    pred<-cbind(Y,X,round(X2,4))
    pred<-as.data.frame(pred)
    pred$region<-colnames(all[,n+1])
    pred$date<-date$date
    pred$actual<-rbind(0,(cbind(pred[2:nrow(pred),1])/pred[1:nrow(pred)-1,1]-1)*100)
    pred$predict<-rbind(0,(cbind(pred[2:nrow(pred),2])/pred[1:nrow(pred)-1,2]-1)*100)
    pred$pred_rate<-(pred$X/pred$Y-1)*100
    pred$X2_change<-rbind(0,(cbind(pred[2:nrow(pred),3]-pred[1:nrow(pred)-1,3])))
    pred_all<-rbind(pred_all,pred)
  }
  pred_all<-cbind(pred_all[,4:5],pred_all[,1:3])
  names(pred_all)[5]<-"X2"
  pred_all=pred_all[with( pred_all, order(region, date)), ]
  ### clean data; export data
  covid19_cases_switzerland_forecast <-  pred_all[,c(1,2,4)]
  covid19_cases_switzerland_forecast[,3] <- round(covid19_cases_switzerland_forecast[,3])
  library(reshape)
  covid19_cases_switzerland_forecast <- cast(covid19_cases_switzerland_forecast, date ~ region)
  colnames(covid19_cases_switzerland_forecast)[1] <- paste("Date")
  covid19_cases_switzerland_forecast <- covid19_cases_switzerland_forecast[,colnames]
  covid19_cases_switzerland <- rbind(covid19_cases_switzerland,covid19_cases_switzerland_forecast[nrow(covid19_cases_switzerland_forecast),])
}
covid19_cases_switzerland <- covid19_cases_switzerland[,colnames]
covid19_cases_switzerland[,1] <- as.Date(covid19_cases_switzerland[,1])
write.csv(covid19_cases_switzerland, "covid19_cases_switzerland_forecast.csv")

# now upload to git by hand