source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx",rowNames = F, colNames = T)

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
impute_Bin_matrix <-
                    predict(Impute_Method, impute_Bin_matrix) 

lm_var <- read.table("~/machine_learning_project/bin_inf_predict_yield/Bin_rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)


Random <- read.table("~/machine_learning_project/feature_selection/Tune_result/bartMachine_train.txt",
  header = T, check.names = F, sep = "\t"
)

Repeats <- 33
Numbers <- 3






cl <- makeCluster(6)

MM <- "lightGBM"

options(java.parameters = "-Xmx5g") 

Result_All <- data.frame()
Resample_All <- data.frame()
IMP_Overall_All <- data.frame()

for (TUNE in seq(2, 60, 2))
{
  train_params <- list(
    num_leaves = TUNE,
    learning_rate = 0.1,
    objective = "regression",
    nthread = 2L
  )

  Result <- data.frame()
  Resample <- data.frame()
  IMP_Overall <- data.frame()

  for (varNum in seq(2,120,2))
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





    clusterExport(cl, list("df", "train_params", "R_MAE_RMSE"))

    clusterEvalQ(cl, {
      library(dplyr)
      library(lightgbm)
    })

    tryCatch({
        jj <- parApply(cl = cl, X = Random, MARGIN = 2, function(x) {
          # partition the training data
          train <- df[x, ] %>% as.matrix()
          test <- df[setdiff(1:167, x), ] %>% as.matrix()

          # Alternatively, you can put in dense matrix, i.e. basic R-matrix

          bst <- lightgbm::lightgbm(
            data = train[, -1],
            params = train_params,
            label = as.numeric(train[, 1]),
            nrounds = 2L
          )


          obs_pred <- cbind(
            as.numeric(test[, 1]),
            predict(bst, test[, -1] %>% as.matrix())
          ) %>%
            as.data.frame() %>%
            dplyr::rename(obs = V1, pred = V2)

          # get each Fold.Rep's result
          c(
            cor(train[, 1], predict(bst, train[, -1] %>% as.matrix())),
            R_MAE_RMSE(obs_pred)
          ) %>% as.vector()
        })

        rownames(jj) <- c("R_Fit", "R", "R2", "Rsquared", "RMSE", "MAE")

        jj <- t(jj) %>%
          as.data.frame() %>%
          dplyr::mutate(
            num_leaves = TUNE
          )

        jj$Rep.Fold <- rownames(jj)

        jj$varNum <- varNum

        Resample <- rbind(Resample, jj)

       
      },
      error = function(e) {
        print(cbind(c(TUNE, varNum), "error"))
      }
    )
  }

  Resample_All <- rbind(Resample_All, Resample)
}

write.table(Resample_All, paste0("~/machine_learning_project/bin_inf_predict_yield/FS_result/", MM, "_1_29_resample_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
)

  stopCluster(cl)

# First test the way of choosing nums of vars
# After testing, the way of choosing nums of vars is that
# Count the n variables that appear most frequently
# in all resamples