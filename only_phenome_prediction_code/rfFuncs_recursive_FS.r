# feature selection with Backwards Selection
# data preprocessing
library(caret)
library(openxlsx)
library(dplyr)
 ourdata<-read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx",rowNames = F,colNames = T)
Y<-ourdata[,ncol(ourdata)] # Include yield only
rownames(ourdata)<-ourdata$line.name
ourdata<-ourdata[,-c(1:2)]
X<-ourdata[,-ncol(ourdata)]  # X contains only independent variables, 169 samples, 736 variables
# Eliminate individuals with missing values in the dependent variable
X<-X[-which(is.na(Y)),]
Y<-Y[-which(is.na(Y))]
# First imputation, using knn
# Note that after imputation, the data are automatically normalized
Impute_Method <- preProcess(X,method = c("knnImpute"))
impuated_X<- 
  predict(Impute_Method,X) # Here impuated_X has been automatically normalized

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
options(java.parameters = "-Xmx5g") # Used to increase the initial java memory to prevent memory limit errors

# cannot choose the nums of vars customizely
rfWithFilter <- rfe(impuated_X, Y,sizes = c(1:736),rfeControl = filterCtrl)

write.table(rfWithFilter$variables,"~/machine_learning_project/feature_selection_recursive/rfFuncs_variables.txt",
            row.names = F,col.names = T,sep = "\t",quote = F)

write.table(rfWithFilter$resample,"~/machine_learning_project/feature_selection_recursive/rfFuncs_resample.txt",
       row.names = F,col.names = T,sep = "\t",quote = F)


# -------------- linear selection methods
filterCtrl <- rfeControl( functions = lmFuncs,
                         method = "repeatedcv", 
                         repeats = 33,returnResamp = "all",number=3,
                         saveDetails = F
)

# cannot choose the nums of vars customizely
rfWithFilter <- rfe(impuated_X, Y,sizes = c(1:736),rfeControl = filterCtrl)

write.table(rfWithFilter$variables,"~/machine_learning_project/feature_selection_recursive/lmFuncs_variables.txt",
            row.names = F,col.names = T,sep = "\t",quote = F)

write.table(rfWithFilter$resample,"~/machine_learning_project/feature_selection_recursive/lmFuncs_resample.txt",
       row.names = F,col.names = T,sep = "\t",quote = F)