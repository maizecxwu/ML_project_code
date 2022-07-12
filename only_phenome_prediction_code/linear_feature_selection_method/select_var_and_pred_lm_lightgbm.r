# Select different nums of vars to predict to valid the influence of nums of vars to accurancy
source("~/machine_learning_project/ML_project_code/metric_evluation_funtion.R")
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

Random <- read.table("~/machine_learning_project/feature_selection/Tune_result/bartMachine_train.txt",
  header = T, check.names = F, sep = "\t"
)

Repeats <- 33
Numbers <- 3


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

    cl <- makeCluster(6)

    clusterExport(cl, list("df", "train_params", "R_MAE_RMSE"))

    clusterEvalQ(cl, {
      library(dplyr)
      library(lightgbm)
    })

    tryCatch(
      {
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

        stopCluster(cl)
      },
      error = function(e) {
        print(cbind(c(TUNE, varNum), "error"))
      }
    )
  }

  Resample_All <- rbind(Resample_All, Resample)
}

write.table(Resample_All, paste0("~/machine_learning_project/feature_tune_parallel/lm_model_results/", MM, "_2_60_resample_rf.txt"),
  row.names = F,
  col.names = T, sep = "\t", quote = F
) 


# First test the way of choosing nums of vars
# After testing, the way of choosing nums of vars is that
# Count the n variables that appear most frequently
# in all resamples