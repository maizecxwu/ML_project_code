# feature selection with Backwards Selection
#数据预处理----
library(caret)
library(openxlsx)
library(dplyr)
 ourdata<-read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx",rowNames = F,colNames = T)
 # ourdata<-read.xlsx("D:/win10data/machine learning project/8801a79384a6d889.xlsx",rowNames = F,colNames = T)
Y<-ourdata[,ncol(ourdata)] #只包含产量
rownames(ourdata)<-ourdata$line.name
ourdata<-ourdata[,-c(1:2)]
X<-ourdata[,-ncol(ourdata)]  #X只包含自变量,169个样本，736个变量
#剔除因变量中的缺失值个体
X<-X[-which(is.na(Y)),]
Y<-Y[-which(is.na(Y))]
# X_rm_zerovarK<- X[,-zerovar] 没有接近0变异的变量----
# X_Corr = cor(X,use = "pairwise.complete.obs")
#先找出高相关的变量并删除,暂时没有必要，因为要对所有变量进行重要性评估
# high_Corr=findCorrelation(X_Corr,0.9)
# X_rm_highcorvar<-X[,-high_Corr]----
#先进性插补 ，使用knn
#注意插补后，数据自动会标准化
Impute_Method <- preProcess(X,method = c("knnImpute"))
impuated_X<- 
  predict(Impute_Method,X) #这里impuated_X已经自动标准化了
#数据预处理----
# cannot customize the metric fuction
# caretFuncs; rfFuncs   rfFuncs nbFuncs treebagFuncs
filterCtrl <- rfeControl( functions = rfFuncs,
                         method = "repeatedcv", 
                         repeats = 33,returnResamp = "all",number=3,
                         saveDetails = F
)
# speed up the process by parallel
library(parallel)
library(doParallel)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g") #用来提升初始的java内存，防止内存限制报错

# cannot choose the nums of vars customizely
rfWithFilter <- rfe(impuated_X, Y,sizes = c(1:736),rfeControl = filterCtrl)

write.table(rfWithFilter$variables,"~/machine_learning_project/feature_selection_recursive/rfFuncs_variables.txt",
            row.names = F,col.names = T,sep = "\t",quote = F)

write.table(rfWithFilter$resample,"~/machine_learning_project/feature_selection_recursive/rfFuncs_resample.txt",
       row.names = F,col.names = T,sep = "\t",quote = F)



