source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")
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
impuated_X <- predict(Impute_Method, X) 

Yield_Meta <- read.table("~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
             header = T, check.names = F, sep = "\t")

Yield_Meta <- Yield_Meta[, -c(136:ncol(Yield_Meta))]

impuated_Meta <- Yield_Meta[,-c(1:2)]

Impute_Method <- preProcess(impuated_Meta, method = c("medianImpute", "center", "scale"))

impuated_Meta <- predict(Impute_Method, impuated_Meta)




lm_var <- read.table("~/machine_learning_project/Meta_analysis/Meta_rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)

Random <- read.table("~/machine_learning_project/feature_selection/Tune_result/bartMachine_train.txt",
  header = T, check.names = F, sep = "\t"
)

Resample <- data.frame()

cl <- makeCluster(6)

for (varNum in seq(2, 130, 2))
{

  NameofVar <-
    lm_var %>%
    filter(Variables == varNum) %>%
    select(var) %>%
    table() %>%
    sort() %>%
    rev() %>%
    .[1:varNum] %>%
    names()

  if (varNum == 1) {
    df <- cbind(Yield_Meta[, 1], impuated_Meta[, NameofVar])
    colnames(df) <- c("Yield", NameofVar)
  } else {
    df <- cbind(Yield_Meta[, 1], impuated_Meta[, NameofVar]) %>%
      dplyr::rename(Yield = "Yield_Meta[, 1]")
  }

  df <- df[, -1]

  clusterExport(cl, list("df", "Yield_Meta", "R_MAE_RMSE"))

  clusterEvalQ(cl, {
    library(dplyr)
    library(rrBLUP)
  })

  jj <- parApply(cl = cl, X = Random, MARGIN = 2, function(x) {


    # partition the training data
    Bin_train <- df[x, ] %>% as.matrix()
    Bin_test <- df[setdiff(1:167, x), ] %>% as.matrix()
    Yeild_train <- Yield_Meta[x,1]
    Yeild_test <- Yield_Meta[setdiff(1:167, x),1]

  pheno_answer<-mixed.solve(Yeild_train,Z = Bin_train,K = NULL,SE = F,return.Hinv = F)
  e <- as.matrix(pheno_answer$u)

  pred_Pheno_valid<-Bin_test %*% e

  pred_Pheno<-pred_Pheno_valid[,1]+pheno_answer$beta

  pred <- data.frame(pred = pred_Pheno, obs = Yeild_test)

  c(
      cor(Yeild_train, 
        ( (Bin_train %*% e)[,1] + pheno_answer$beta)),
      R_MAE_RMSE(pred)

    ) %>% as.vector()

  })

  
  rownames(jj) <- c("R_Fit", "R", "R2", "Rsquared", "RMSE", "MAE")

  jj <- jj %>% t() %>% as.data.frame()

  jj$`Fold.Rep` <- rownames(jj)

  jj$varNum <- varNum

Resample <- rbind(Resample, jj)

}

 write.table(Resample, "~/machine_learning_project/Meta_analysis/FS_result/rrblup_result.txt", 
                row.names = F, col.names = T, quote = F, sep = "\t")