#We set our working directory and assign thess data sets
actlabels<-read.table("activity_labels.txt",header=FALSE)
features<-read.table("features.txt",header=FALSE)
xtest<-read.table("test/X_test.txt",header=FALSE,sep="")
subtest<-read.table("test/subject_test.txt",header=FALSE)
ytest<-read.table("test/y_test.txt",header=FALSE)
xtrain<-read.table("train/X_train.txt",header=FALSE,sep="")
subtrain<-read.table("train/subject_train.txt",header=FALSE,sep="")
ytrain<-read.table("train/y_train.txt",header=FALSE,sep="")
#Step1
mergedat<-rbind(xtest,xtrain)
#Step2
colmean<-grep("mean()",as.vector(features[,2]),fixed=TRUE)
colsd<-grep("std()",as.vector(features[,2]),fixed=TRUE)
matdata<-mergedat[,c(colmean,colsd)]
#Step3
ynumtot<-rbind(ytest,ytrain)
yacttot<-vector()
for(i in 1:length(ynumtot[,1])){
     for(j in 1:length(actlabels[,1])){
          if(ynumtot[i,1]==actlabels[j,1]){
               yacttot[i]=as.vector(actlabels[j,2])
          } 
     }
     
}
library(data.table)
matdata<-data.table(matdata)
matdata<-matdata[,Activity:=yacttot]
subtot<-rbind(subtest,subtrain)
matdata<-matdata[,Subject:=subtot]
#Step4
colmean2<-grep("mean()",as.vector(features[,2]),fixed=TRUE,value=TRUE)
colsd2<-grep("std()",as.vector(features[,2]),fixed=TRUE,value=TRUE)
matdata<-data.frame(matdata)
colnames(matdata)<-c(colmean2,colsd2,"Activity","Subject")
#Step5. 
newestdata<-matrix(nrow=180,ncol=68)
meann<-vector()
for(i in 1:6){
     act<-c("WALKING","STANDING","SITTING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","LAYING")
     for(j in 1:30){
          Subset<-matdata[matdata$Activity==act[i],]
          Subset2<-Subset[Subset$Subject==j,]
          Subset22<-data.frame(Subset2)
          for(k in 1:66){ 
               meann[k+2]<-mean(as.numeric(Subset22[,k]))
               meann[2]<-act[i]
               meann[1]<-j
          } 
          val<-30*(i-1)+j
          newestdata[val,]<-meann
     }
}
newestdata<-data.frame(newestdata)
avvec<-c("","",rep("Average ",times=66))
namess<-c("Subject","Activity",colmean2,colsd2)
newnames<-paste(avvec,namess,sep="")
colnames(newestdata)<-newnames
write.table(newestdata,file="run_analysis.txt",row.names=TRUE,quote=TRUE)