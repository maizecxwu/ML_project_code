

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

impute_Bin_matrix <- impute_Bin_matrix$imputed %>% as.data.frame()

Impute_Method <- preProcess(impute_Bin_matrix, method = c("center", "scale"))

impute_Bin_matrix <-predict(Impute_Method, impute_Bin_matrix) 




lm_var <- read.table("~/machine_learning_project/bin_inf_predict_yield/Bin_rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)
Random <- read.table("~/machine_learning_project/feature_selection/Tune_result/bartMachine_train.txt",
  header = T, check.names = F, sep = "\t"
)

Resample <- data.frame()

cl <- makeCluster(6)

for (varNum in seq(2, 280, 2))
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
    df <- cbind(Y, impute_Bin_matrix[, NameofVar])
    colnames(df) <- c("Yield", NameofVar)
  } else {
    df <- cbind(Y, impute_Bin_matrix[, NameofVar]) %>%
      dplyr::rename(Yield = Y)
  }

  df <- df[, -1]

  clusterExport(cl, list("df", "Yield_Bin", "R_MAE_RMSE"))

  clusterEvalQ(cl, {
    library(dplyr)
    library(rrBLUP)
  })

  jj <- parApply(cl = cl, X = Random, MARGIN = 2, function(x) {


    # partition the training data
    Bin_train <- df[x, ] %>% as.matrix()
    Bin_test <- df[setdiff(1:167, x), ] %>% as.matrix()
    Yeild_train <- Yield_Bin[x,1]
    Yeild_test <- Yield_Bin[setdiff(1:167, x),1]

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

 write.table(Resample, "~/machine_learning_project/bin_inf_predict_yield/FS_result/rrblup_result.txt", 
                row.names = F, col.names = T, quote = F, sep = "\t")
