

smpl_rows<-sample.int(n=dim(data)[1],n=20000)
smpl<-data[smpl_rows,]

smpl<-select(smpl,-Store,-Date)


#NA
na_var<-sapply(data,FUN=function(x)sum(is.na(x)))
na_var<-na_var[na_var>0]


#Na Var by store
summ_na<-function(x){
        summ<-data[,-3]%>%
                group_by(Store)%>%
                summarize_(y=interp(~sum(is.na(var)),var=as.name(x)))
        names(summ)[names(summ)=="y"]<-x
        summ<-summ[summ[,2]!=0,]
        summ
}

na_by_store<-lapply(names(na_var),FUN=summ_na)
