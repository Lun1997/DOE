#set====
library(readxl)
library(mice)
library(ggplot2)
library(tidyr)
library(patchwork)
library(writexl)
library(useful)
library(factoextra)
library(cluster)
setwd("C:/Users/c1024/OneDrive/桌面/NCCU/碩二上/統計諮詢/final")
#data====
Data=list()
for(i in 1:3){
  Data[[i]] = as.data.frame(read_xlsx("data2.xlsx",sheet = i))
}
 
names(Data) = c("海馬迴","前額葉","杏仁核")

#NA====

#1. 刪除
Data[["海馬迴"]] = Data[["海馬迴"]][-c(which(Data[["海馬迴"]]$ID=="H24"),
                                       which(Data[["海馬迴"]]$ID=="L24")),] 
Data[["前額葉"]] = Data[["前額葉"]][-c(which(Data[["前額葉"]]$ID=="C44")),] 
Data[["杏仁核"]] = Data[["杏仁核"]][-c(which(Data[["杏仁核"]]$ID=="L34")),] 
for(i in 1:3){
  Data[[i]]$ID = NULL
  Data[[i]]$gender = NULL
}
v1=c()
for(i in 3:15){
  v1[i-2] = sum(is.na(Data[[1]][,i]))
}
write.csv(data.frame(na=v1,name=colnames(Data[[1]])[3:15]),"海馬迴.csv")

v2=c()
for(i in 3:8){
  v2[i-2] = sum(is.na(Data[[2]][,i]))
}
write.csv(data.frame(na=v2,name=colnames(Data[[2]])[3:8]),"前額葉.csv")

v3=c()
for(i in 3:8){
  v3[i-2] = sum(is.na(Data[[3]][,i]))
}
write.csv(data.frame(na=v3,name=colnames(Data[[3]])[3:8]),"杏仁核.csv")

Data[[1]]$ERK = NULL
Data[[1]]$pJNK = NULL
Data[[1]]$JNK = NULL
Data[[1]]$pp38 = NULL
Data[[1]]$P38 = NULL
Data[[1]]$pCaMKII = NULL
Data[[1]]$pDAPK = NULL

Data[[2]]$pJNK = NULL

Data[[3]]$pERK = NULL
Data[[3]]$pDAPK = NULL

for(i in 1:3){
  print(sum(is.na(Data[[i]])))
}

#2. 補值
for(i in 1:3){
  Mice = mice(Data[[i]],m = 1,maxit = 50,method = "norm.predict")
  Data[[i]] = complete(Mice,1)
  
}

#3. 重新排序
Data[[1]] = data.frame(Data[[1]]$state,Data[[1]]$month,Data[[1]]$pERK,
                       Data[[1]]$CaMKII,Data[[1]]$DAPK,Data[[1]]$Arc,
                       Data[[1]]$Egr1,Data[[1]]$GFAP)
colnames(Data[[1]])=c("state","month","pERK","CaMKII","DAPK","Arc","Egr1","GFAP")
Data[[2]] = data.frame(Data[[2]]$state,Data[[2]]$month,Data[[2]]$pERK,
                       Data[[2]]$pCaMKII,Data[[2]]$Arc,Data[[2]]$GFAP,
                       Data[[2]]$Iba1)
colnames(Data[[2]])=c("state","month","pERK","pCaMKII","Arc","GFAP","Iba1")
Data[[3]] = data.frame(Data[[3]]$state,Data[[3]]$month,Data[[3]]$pJNK,
                       Data[[3]]$pCaMKII,Data[[3]]$DAPK,Data[[3]]$Arc)
colnames(Data[[3]])=c("state","month","pJNK","pCaMKII","DAPK","Arc")

#EDA====

# 1.相關性
#海馬迴
plot1 = Data[[1]]

plot1 = gather(plot1,key="keys",value="values",
         pERK,CaMKII,Arc,DAPK,Egr1,GFAP)
plot1$month = as.factor(plot1$month)
  
p1 = ggplot(aes(x=month,y=values,fill=keys),data=plot1)+geom_boxplot()+
     facet_grid(. ~ state)+
     labs(title="海馬迴")+
     theme(plot.title=element_text(hjust = 0.5,face = "bold"))

#前額葉
plot2 = Data[[2]]

plot2 = gather(plot2,key="keys",value="values",
               pERK,pCaMKII,Arc,GFAP,Iba1)
plot2$month = as.factor(plot2$month)

p2 = ggplot(aes(x=month,y=values,fill=keys),data=plot2)+geom_boxplot()+
     facet_grid(. ~ state)+
     labs(title="前額葉")+
     theme(plot.title=element_text(hjust = 0.5,face = "bold"))

#杏仁核
plot3 = Data[[3]]

plot3 = gather(plot3,key="keys",value="values",
               pCaMKII,pJNK,Arc,DAPK)
plot3$month = as.factor(plot3$month)

p3 = ggplot(aes(x=month,y=values,fill=keys),data=plot3)+geom_boxplot()+
     facet_grid(. ~ state)+
     labs(title="杏仁核")+
     theme(plot.title=element_text(hjust = 0.5,face = "bold"))


p1+p2+p3+plot_layout(nrow = 3)



# 2.常態qqplot
#海馬迴
plot1 = Data[[1]][,c(1,2,3)] ;colnames(plot1)[3] = "values"
P = ggplot(plot1, aes(sample = values)) +
    stat_qq()+stat_qq_line()+
    labs(title=colnames(Data[[1]][3]))+
    theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
    facet_grid(state ~ month) 

for(i in 1:5){
    plot1 = Data[[1]][,c(1,2,i+3)] ;colnames(plot1)[3] = "values"
    p1 = ggplot(plot1, aes(sample = values)) +
         stat_qq()+stat_qq_line()+
         labs(title=colnames(Data[[1]][i+3]))+
         theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
         facet_grid(state ~ month)
    
    P = P + p1 
}
P + plot_layout(nrow = 3)
#前額葉
plot2 = Data[[2]][,c(1,2,3)] ;colnames(plot2)[3] = "values"
P = ggplot(plot2, aes(sample = values)) +
    stat_qq()+stat_qq_line()+
    labs(title=colnames(Data[[2]][3]))+
    theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
    facet_grid(state ~ month) 

for(i in 1:4){
  plot2 = Data[[2]][,c(1,2,i+3)] ;colnames(plot2)[3] = "values"
  p2 = ggplot(plot2, aes(sample = values)) +
    stat_qq()+stat_qq_line()+
    labs(title=colnames(Data[[2]][i+3]))+
    theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
    facet_grid(state ~ month)
  
  P = P + p2 
}
P + plot_layout(nrow = 3)

#杏仁核
plot3 = Data[[3]][,c(1,2,3)] ;colnames(plot3)[3] = "values"
P = ggplot(plot3, aes(sample = values)) +
  stat_qq()+stat_qq_line()+
  labs(title=colnames(Data[[3]][3]))+
  theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
  facet_grid(state ~ month) 

for(i in 1:3){
  plot3 = Data[[3]][,c(1,2,i+3)] ;colnames(plot3)[3] = "values"
  p3 = ggplot(plot3, aes(sample = values)) +
    stat_qq()+stat_qq_line()+
    labs(title=colnames(Data[[3]][i+3]))+
    theme(plot.title=element_text(hjust = 0.5,face = "bold"))+
    facet_grid(state ~ month)
  
  P = P + p3 
}
P + plot_layout(nrow = 2)

 
#常態檢定====
set = expand.grid(c("C","H","L"),c("1","2"))
set$Var2 = as.character(set$Var2)
set = as.matrix(set)

for(i in 1:3){
  Data[[i]]$set = NA
}

check = function(x){
  if(sum(x[c(1,2)]==set[1,])==2){return(1)}
  else if (sum(x[c(1,2)]==set[2,])==2){return(2)}
  else if (sum(x[c(1,2)]==set[3,])==2){return(3)}
  else if (sum(x[c(1,2)]==set[4,])==2){return(4)}
  else if (sum(x[c(1,2)]==set[5,])==2){return(5)}
  else if (sum(x[c(1,2)]==set[6,])==2){return(6)}
}

for(i in 1:3){
  Data[[i]]$set = apply(Data[[i]],1,check)
}
#海馬迴
pv1=matrix(NA,6,6)
for(i in 1:6){
  for(s in 1:6){
    x = Data[[1]][,i+2][Data[[1]]$set == s]
    pv1[s,i] = round(shapiro.test(x)$p.value,3) 
  }
}
#前額葉
pv2=matrix(NA,6,5)
for(i in 1:5){
  for(s in 1:6){
    x = Data[[2]][,i+2][Data[[2]]$set == s]
    pv2[s,i] = round(shapiro.test(x)$p.value,3) 
  }
}
#杏仁核
pv3=matrix(NA,6,4)
for(i in 1:4){
  for(s in 1:6){
    x = Data[[3]][,i+2][Data[[3]]$set == s]
    pv3[s,i] = round(shapiro.test(x)$p.value,3) 
  }
}

PV = list(pv1,pv2,pv3);PV = lapply(PV,as.data.frame)
write_xlsx(PV,path = "pvlue.xlsx")


#anova====

for(i in 1:3){
  Data[[i]]$state = factor(Data[[i]]$state)
  Data[[i]]$month = factor(Data[[i]]$month,labels=c("one","two"))
}


ANOVA=list("海馬迴"=list(),"前額葉"=list(),"杏仁核"=list())

#海馬迴
for(i in 1:6){
  my_data = as.data.frame(Data[[1]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = summary(Aov)
  Aov = as.data.frame(Aov[[1]])
  index = c("state","month","interaction","Residuals")
  Aov = cbind(index,Aov);Aov=as.data.frame(Aov)
  ANOVA[["海馬迴"]][[i]] = Aov 
}

#前額葉
for(i in 1:5){
  my_data = as.data.frame(Data[[2]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = summary(Aov)
  Aov = as.data.frame(Aov[[1]])
  index = c("state","month","interaction","Residuals")
  Aov = cbind(index,Aov);Aov=as.data.frame(Aov)
  ANOVA[["前額葉"]][[i]] = Aov 
}

#杏仁核
for(i in 1:4){
  my_data = as.data.frame(Data[[3]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = summary(Aov)
  Aov = as.data.frame(Aov[[1]])
  index = c("state","month","interaction","Residuals")
  Aov = cbind(index,Aov);Aov=as.data.frame(Aov)
  ANOVA[["杏仁核"]][[i]] = Aov 
}

for(i in 1:3){
  file=paste(i,".xlsx",sep="")
  write_xlsx(ANOVA[[i]],file,)
}

#事後檢定====
for(i in c(1,3,4)){
  my_data = as.data.frame(Data[[1]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = rbind(TukeyHSD(Aov)$state,TukeyHSD(Aov)$month);Aov=as.data.frame(Aov)
  files = paste(colnames(Data[[1]][i+2]),"海馬迴.csv",sep="")
  write.csv(Aov,file=files)
  
}

for(i in c(3,4,5)){
  my_data = as.data.frame(Data[[2]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = rbind(TukeyHSD(Aov)$state,TukeyHSD(Aov)$month);Aov=as.data.frame(Aov)
  files = paste(colnames(Data[[2]][i+2]),"前額葉.csv",sep="")
  write.csv(Aov,file=files)
  
}

for(i in c(2,3)){
  my_data = as.data.frame(Data[[3]][,c(1,2,i+2)])
  colnames(my_data)[3] = "values" 
  Aov = aov(values ~ state + month + state*month , data = my_data)
  Aov = rbind(TukeyHSD(Aov)$state,TukeyHSD(Aov)$month);Aov=as.data.frame(Aov)
  files = paste(colnames(Data[[3]][i+2]),"杏仁核.csv",sep="")
  write.csv(Aov,file=files)
}



#KNN
