# Select different nums of vars to predict to valid the influence of nums of vars to accurancy
source("~/machine_learning_project/ML_project_code/metric_evluation_funtion.R")
# 数据预处理----
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", rowNames = F, colNames = T)
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

lm_var <- read.table("~/machine_learning_project/feature_selection_recursive/lmFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)



Repeats <- 33
Numbers <- 3


MM <- "bartMachine"

cl <- makePSOCKcluster(6)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g") 

Result_All <- data.frame()
Resample_All <- data.frame()
IMP_Overall_All <- data.frame()

for(TUNE in seq(2,120,4))
{

Result <- data.frame()
Resample <- data.frame()
IMP_Overall <- data.frame()

for (varNum in 2:110)
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



  set.seed(123)
  seeds <- vector(mode = "list", length = (Repeats * Numbers + 1))
  for (i in 1:(Repeats * Numbers)) seeds[[i]] <- sample.int(1000, 36)
  seeds[[Repeats * Numbers + 1]] <- sample.int(1000, 1)
  #
  # set.seed(123)
  fitControl <- trainControl( ## 10-fold CV
    method = "repeatedcv",
    number = Numbers,
    ## repeated ten times
    repeats = Repeats,
    # p = 0.9,
    # search = "random" 
    returnResamp = "all",
    summaryFunction = R_MAE_RMSE,
    seeds = seeds,
    # index = Index_list,
  )
  tryCatch({
  set.seed(1) 
  Fit <- train(Yield ~ .,
    data = df,
    method = MM,
    # tuneLength = , 
    maximize = TRUE,
    trControl = fitControl,
    tuneGrid = expand.grid(
      num_trees = TUNE, 
      alpha = 0.95, beta = 2, k = 2, nu = 3
    ),
    metric = "R2",
  )



  R_Fit <- sapply(1:(Repeats * Numbers), FUN = function(x) {
    cor(predict(Fit, newdata = impuated_X[Fit$control$index[[x]], ]), Y[Fit$control$index[[x]]]) # nolint
  })

  Resample <- rbind(Resample, cbind(R_Fit, Fit$resample, varNum))

  Result <- rbind(Result, cbind(Fit$results, varNum))


  IMP <- varImp(Fit, scale = FALSE)

  IMP <- cbind(rownames(IMP$importance), IMP$importance)

  colnames(IMP) <- c("Var", "Scale_Value")

  if (all(dim(IMP_Overall) == c(0, 0))) {
    IMP_Overall <- IMP
  } else {
    IMP_Overall <- full_join(IMP_Overall, IMP, by = c("Var" = "Var"))
  }
  }, error = function(e) {
  print(cbind( c(TUNE, varNum), "error"))
  }

  )
}


Result_All <- rbind(Result_All, Result)

Resample_All <- rbind(Resample_All, Resample)

   if (all(dim(IMP_Overall_All) == c(0, 0))) {
    IMP_Overall_All <- IMP_Overall
  } else {
    IMP_Overall_All <- full_join(IMP_Overall_All, 
    IMP_Overall, by = c("Var" = "Var"))
  }

}


write.table(Result_All, paste0("~/machine_learning_project/feature_tune_parallel/lm_model_results/", MM, "_2_120_results_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table(Resample_All, paste0("~/machine_learning_project/feature_tune_parallel/lm_model_results/", MM, "_2_120_resample_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table(as.data.frame(do.call(cbind, Fit$control$index)),
  paste0("~/machine_learning_project/feature_tune_parallel/lm_model_results/", MM, "_2_120_train_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table(IMP_Overall, paste0("~/machine_learning_project/feature_tune_parallel/lm_model_results/", MM, "_2_120_IMP_Overall_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 


stopImplicitCluster()
stopCluster(cl)


# First test the way of choosing nums of vars
# After testing, the way of choosing nums of vars is that
# Count the n variables that appear most frequently
# in all resamples