## packages
list.of.packages <- c("readr", "forecast", "pracma","Metrics","readr","dplyr","tibble","reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### read data

library (readr)

#international data
path <- "/Users/davidschwelien/Desktop/Modell_Infos/ts_r.csv" ### issue: read from git!
covid_world_data <- read_csv(path)
covid_world_data  <- as.data.frame(covid_world_data )

#swiss data:
urlfile <- "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/predicted.csv"
covid_swiss_data <- read.csv(url(urlfile))
covid_swiss_data[,1] <- as.Date(covid_swiss_data[,1])
c("Switzerland_",names(covid_swiss_data))

# merge and clean
names(covid_swiss_data) <- paste("Switzerland_", names(covid_swiss_data), sep='')
names(covid_swiss_data)[1] <- "date"
library(dplyr)
covid_all_data  <- left_join(covid_world_data, covid_swiss_data)
covid_all_data[is.na(covid_all_data)] <- 0
#swiss_compare<-covid_all_data[,grepl( "wit" , names( covid_all_data ) )]

### predict data


library(pracma)
library(Metrics)
library(readr)
library(tibble)

all <- covid_all_data
### wip:
all <- as_tibble(all) 
###
all$X1<-NULL
date<-all[,1]
date[nrow(date) + 1,1] <-all[nrow(all),1]+1
pred_all<-NULL
for (n in 2:ncol(all)-1) {
  Y<-ts(data = all[n+1], start = 1, end =nrow(all)+1)  
  sig_w<-0.01
  w<-sig_w*rnorm(100) # acceleration which denotes the fluctuation (Q/R) rnorm(100, mean = 0, sd = 1)
  sig_v<-0.01
  v<-sig_v*rnorm(100) ## changed by ds  
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
covid_all_data_predicted <-  pred_all[,c(1,2,4)]
covid_all_data_predicted[,3] <- round(covid_all_data_predicted[,3])
library(reshape)
covid19_cases_predicted <- cast(covid_all_data_predicted, date ~ region)
colnames(covid19_cases_predicted) <- gsub("nan", "all", colnames(covid19_cases_predicted))
### 
colnum <-which(colnames(covid19_cases_predicted) =="Switzerland_all")
colnames(covid19_cases_predicted)[colnum] <- paste("Switzerland_all")
colnum <-which(colnames(covid19_cases_predicted) =="Switzerland_CH")
colnames(covid19_cases_predicted)[colnum] <- paste("Switzerland_all_scraped")
write.csv(covid19_cases_predicted,"covid19_cases_predicted.csv")
# subset Switzerland
x <- c("date", "wit")
covid19_cases_predicted_switzerland <- covid19_cases_predicted[,grepl( paste(x, collapse = "|") , names(covid19_cases_predicted))]

col_names <- names(covid19_cases_predicted_switzerland)
first_cols  <- c("date", "Switzerland_all", "Switzerland_all_scraped")
'%ni%' <- Negate('%in%')
last_cols <- col_names[col_names%ni%first_cols]
col_order <- c(first_cols, last_cols)
covid19_cases_predicted_switzerland <-covid19_cases_predicted_switzerland[,col_order]
write.csv(covid19_cases_predicted_switzerland, "covid19_cases_predicted_switzerland.csv")
