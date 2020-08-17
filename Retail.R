setwd("D:/edvancer/r files/Data/retail/")
ci_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
ci_test=read.csv("store_test.csv",stringsAsFactors = FALSE)
ci_test$store=NA
ci_test$data="test"
ci_train$data="train"
ci_all=rbind(ci_train,ci_test)
library(dplyr)
glimpse(ci_all)
table(ci_all$Id)
sum(unique(table(ci_all$store_Type)))
table(ci_all$countytownname)
ci_all=ci_all %>% 
  select(-state_alpha)
ci_all=ci_all %>% 
  select(-storecode)
ci_all$population[is.na(ci_all$population)]=round(mean(ci_all$population,na.rm=T),0)
ci_all$country[is.na(ci_all$country)]=round(mean(ci_all$country,na.rm=T),0)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_var=names(ci_all)[sapply(ci_all,is.character)]
cat_var=cat_var[cat_var != "data"& cat_var!="store" ]   


for(var in cat_var){
  ci_all=CreateDummies(ci_all,var,100)
}
glimpse(ci_all)

ci_all$store=as.factor(ci_all$store)
View(ci_all)
ci_train=ci_all %>% filter(data=='train') %>% select(-data)
ci_test=ci_all %>% filter(data=='test') %>% select (-data,-store)

param=list(mtry=c(3),
           ntree=c(800),
           maxnodes=c(500),
           nodesize=c(5)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}
num_trials=1
#my_params=subset_paras(param,num_trials)
my_params=param

myauc=0


library(tree)
library(cvTools)
library(randomForest)
## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params
  
  k=cvTuning(randomForest,store~.-Id, 
             data =ci_train,
             tuning =params,
             folds = cvFolds(nrow(ci_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    #print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  print('DONE')
  #uncomment the line above to keep track of progress
}

## Values obtained from an earlier run 

myauc=0.8163751
best_params=data.frame(mtry=3,
                       ntree=800,
                       maxnodes=500,
                       nodesize=5)

## Model on the entire training data

ci.rf.final=randomForest(store~.-Id,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=ci_train
)

## Follow the same process as earlier for prediction on
## test production data

test.score=predict(ci.rf.final,newdata = ci_test,type='prob')[,2]

write.csv(test.score,'Kavya_G_P2_part2.csv',row.names = F)     

ci.rf.final

train.score=predict(ci.rf.final,newdata = ci_train,type='prob')[,2]

real=ci_train$store
mycost_auc(real,train.score)
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
View(cutoff_data)

test.predicted=as.numeric(test.score>my_cutoff)

final.test.prediction = as.character(test.predicted == 1)
final.test.prediction = gsub("FALSE","No",final.test.prediction)
final.test.prediction = gsub("TRUE","Yes",final.test.prediction)
write.csv(final.test.prediction,"Kavya_G_P5_part2.csv",row.names = F)
table(final.test.prediction)
