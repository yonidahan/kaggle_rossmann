
lapply(c("dplyr","ggplot2"),require,character.only=T)

Sys.setlocale("LC_TIME", "C")

setwd("C:/Users/Sarah/Desktop/Data Science/Projects/rossmann")

data<-merge(read.csv("train.csv"),read.csv("store.csv"),
             by="Store")
test<-merge(read.csv("test.csv"),read.csv("store.csv"),by="Store")



feat_eng=function(data){
        data$Date<-as.POSIXlt(data$Date,format="%Y-%m-%d")
        data$Month<-as.factor(months(data$Date))#Creates a "month" var
        
        data$CompElapsed<-paste("01",data$CompetitionOpenSinceMonth,
                                     data$CompetitionOpenSinceYear,sep="-")
        data$CompElapsed<-as.POSIXlt(data$CompElapsed,format="%d-%m-%Y")
        
        #Elapsed time since opening
        data$CompElapsed<-difftime(data$Date,data$CompElapsed,units="days")
        data$CompElapsed[is.na(data$CompElapsed)]<-0#NA-->0 ??
        
        data$PromoInterval<-as.character(data$PromoInterval)
        data$PromoInterval[data$PromoInterval==c("Jan,Apr,Jul,Oct")]<-c("January,April,July,October")
        data$PromoInterval[data$PromoInterval==c("Feb,May,Aug,Nov")]<-c("February,May,August,November")
        data$PromoInterval[data$PromoInterval==c("Mar,Jun,Sept,Dec")]<-c("March,June,September,December")
        
        promo2act<-function(x){
                month<-as.character(x[1])
                promonth<-x[2]
                
                promonth<-unlist(strsplit(promonth,","))
                if(!is.na(match(month,promonth))){
                        y<-1
                }else{
                        y<-0
                }
                return(y)
        }
        data$Promo2Act<-apply(data[,c("Month","PromoInterval")],1,promo2act)
        
        data$Promo2Act[data$Open==0]<-0
        
        data<-select(data,-CompetitionOpenSinceYear,-CompetitionOpenSinceMonth,
                     -PromoInterval)
        return(data)
        }
