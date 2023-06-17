# Select different nums of vars to predict to valid the influence of nums of vars to accurancy
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", rowNames = F, colNames = T)
Y <- ourdata[, ncol(ourdata)]
rownames(ourdata) <- ourdata$line.name
ourdata <- ourdata[, -c(1:2)]
X <- ourdata[, -ncol(ourdata)] 
X <- X[-which(is.na(Y)), ]
Y <- Y[-which(is.na(Y))]
Impute_Method <- preProcess(X, method = c("knnImpute"))
impuated_X <- predict(Impute_Method, X) 


source("~/machine_learning_project/ML_project_code/metric_evluation_funtion.R")

# Set the same training and validation set with other model
Random <- read.table("~/machine_learning_project/feature_selection/Tune_result/bartMachine_train.txt",
  header = T, check.names = F, sep = "\t"
)

cl <- makeCluster(6)

lm_var <- read.table("~/machine_learning_project/feature_selection_recursive/rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)

Resample <- data.frame()

# -------------- Start training
for (varNum in seq(2, 200, 2))
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
    df <- cbind(Y, impuated_X[, NameofVar])
    colnames(df) <- c("Yield", NameofVar)
  } else {
    df <- cbind(Y, impuated_X[, NameofVar]) %>%
      dplyr::rename(Yield = Y)
  }

  df <- df[, -1]


  clusterExport(cl, list("df", "Y", "R_MAE_RMSE"))

  clusterEvalQ(cl, {
    library(dplyr)
    library(rrBLUP)
  })

  jj <- parApply(cl = cl, X = Random, MARGIN = 2, function(x) {


    # partition the training data
    Bin_train <- df[x, ] %>% as.matrix()
    Bin_test <- df[setdiff(1:167, x), ] %>% as.matrix()
    Yeild_train <- Y[x]
    Yeild_test <- Y[setdiff(1:167, x)]

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


 write.table(Resample, "~/machine_learning_project/feature_tune_parallel/5_model_results/rrblup_result_2_200_var.txt", 
                row.names = F, col.names = T, quote = F, sep = "\t")