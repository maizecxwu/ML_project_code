# Select different nums of vars to predict to valid the influence of nums of vars to accurancy
source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")

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

MM <- "bartMachine"

lm_var <- read.table("~/machine_learning_project/Meta_analysis/Early_Meta_rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)

Early_Name <- colnames(impuated_X)[as.vector(sapply(1:8, function(x) {
  seq(x, 736, 16)
}))]

Late_Name <- colnames(impuated_X)[as.vector(sapply(9:16, function(x) {
  seq(x, 736, 16)
}))]

#  get the early var to predict the yield
IMP_2 <- read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
  header = T, check.names = F, sep = "\t"
) %>% filter(Var %in% Early_Name) %>%
  filter(model == MM)


# get the Meta inf
Yield_Meta <- read.table("~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
             header = T, check.names = F, sep = "\t")

Yield_Meta <- Yield_Meta[, -c(136:ncol(Yield_Meta))]

impuated_Meta <- Yield_Meta[,-c(1:2)]

Impute_Method <- preProcess(impuated_Meta, method = c("medianImpute", "center", "scale"))

impuated_Meta <-
  predict(Impute_Method, impuated_Meta)


Repeats <- 33
Numbers <- 3


cl <- makePSOCKcluster(4)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g") 

Result_All <- data.frame()
Resample_All <- data.frame()
IMP_Overall_All <- data.frame()

for(TUNE in seq(120,160,4))
{

Result <- data.frame()
Resample <- data.frame()
IMP_Overall <- data.frame()

for (varNum in seq(2,50,4))
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
  df <- cbind(
    Y, impuated_X[,  IMP_2$Var],
    impuated_Meta[, NameofVar]
  ) %>% dplyr::rename(Yield = Y)
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
    maximize = TRUE,
    trControl = fitControl,
    tuneGrid = expand.grid(
      num_trees = TUNE, 
      alpha = 0.95, beta = 2, k = 2, nu = 3
    ),
    metric = "R2",
  )


  R_Fit <- sapply(1:(Repeats * Numbers), FUN = function(x) {
    cor(predict(Fit, newdata = df[,-1][Fit$control$index[[x]], ]), Y[Fit$control$index[[x]]]) # nolint
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


write.table(Result_All, paste0("~/machine_learning_project/Early_Phenomics_Meta_truly/result/", MM, "_1_10_results_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table(Resample_All, paste0("~/machine_learning_project/Early_Phenomics_Meta_truly/result/", MM, "_1_10_resample_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table(as.data.frame(do.call(cbind, Fit$control$index)),
  paste0("~/machine_learning_project/Early_Phenomics_Meta_truly/result/", MM, "_1_10_train_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 

write.table( IMP_Overall_All , paste0("~/machine_learning_project/Early_Phenomics_Meta_truly/result/", MM, "_1_10_IMP_Overall_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 


stopImplicitCluster()
stopCluster(cl)


# First test the way of choosing nums of vars
# After testing, the way of choosing nums of vars is that
# Count the n variables that appear most frequently
# in all resamples



# "Galactonic.acid-1,4-lactone_seeding"
# IMP_Overall_All$Var <- gsub("`", "", IMP_Overall_All$Var)

# IMP_Overall_All$Var[14] <- "Galactonic.acid-1,4-lactone_seeding"

# write.table(IMP_Overall_All , paste0("~/machine_learning_project/Plot_result/Multi_omics_IMP/bartMachine_best_IMP_Meta.txt"),
#   row.names = F, col.names = T, sep = "\t", quote = F
# )