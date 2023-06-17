
library(dplyr)
library(caret)
library(parallel)
library(doParallel)
library(openxlsx)
library(data.table)
library(rrBLUP)
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)
# ourdata<-read.xlsx("D:/win10data/machine learning project/8801a79384a6d889.xlsx",rowNames = F,colNames = T)
Y <- ourdata[, ncol(ourdata)] 
rownames(ourdata) <- ourdata$line.name
ourdata <- ourdata[, -c(1:2)]
X <- ourdata[, -ncol(ourdata)] 
X <- X[-which(is.na(Y)), ]
Y <- Y[-which(is.na(Y))]
Impute_Method <- preProcess(X, method = c("knnImpute"))
impuated_X <-
  predict(Impute_Method, X) 

ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)

 Bin <- openxlsx::read.xlsx("~/machine_learning_project/bin_inf_predict_yield/Bin_inf.xlsx",
                             rowNames = F, colNames = T, rows = 3:2499)

Bin <- Bin %>% select(-c(Chromosome, `genetic.position.(cM)`)) 

rownames(Bin) <- Bin$bin

Bin <- Bin %>% mutate(bin = NULL) %>% t() %>% as.data.frame()

Bin$Line <- rownames(Bin)

Yield_Bin <- left_join(ourdata %>% select(Yield_Blup, 2), 
                        Bin , by = c("line.name" = "Line"))


 Yield_Bin <- Yield_Bin[-c(1:2),] 

 Bin_matrix <- Yield_Bin[, -c(1:2)]

  Bin_matrix [ Bin_matrix == 0] <- NA
  Bin_matrix [ Bin_matrix == 3] <- 0
   Bin_matrix [ Bin_matrix == 1] <- -1
   Bin_matrix [ Bin_matrix == 2] <- 1

impute_Bin_matrix <- A.mat(Bin_matrix, max.missing = 0.5, 
                    impute.method = "mean", return.imputed = T)

impute_Bin_matrix <- impute_Bin_matrix$imputed


# cannot customize the metric fuction
# caretFuncs; rfFuncs   rfFuncs nbFuncs treebagFuncs
filterCtrl <- rfeControl( functions = lmFuncs,
                         method = "repeatedcv", 
                         repeats = 33,returnResamp = "all",number=3,
                         saveDetails = F
)
# speed up the process by parallel
library(parallel)
library(doParallel)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g")

# cannot choose the nums of vars customizely
rfWithFilter <- rfe(impute_Bin_matrix, Y,sizes = c(1:300),rfeControl = filterCtrl)

write.table(rfWithFilter$variables,"~/machine_learning_project/bin_inf_predict_yield/Bin_lmFuncs_variables.txt",
            row.names = F,col.names = T,sep = "\t",quote = F)

write.table(rfWithFilter$resample,"~/machine_learning_project/bin_inf_predict_yield/Bin_lmFuncs_resample.txt",
       row.names = F,col.names = T,sep = "\t",quote = F)



