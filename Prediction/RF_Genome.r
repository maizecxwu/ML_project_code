# Select different nums of vars to predict to valid the influence of nums of vars to accurancy

args <- commandArgs(TRUE)

if(length(args) == 0)
{
print('The script need 4 parameters: FS_methods, omics_type, model, agro_trait')
print('FS_methods: RF, Linear')
print('omics_type: Phenome, metabolome, Genome, Early_metabolome')
print('model: rf, bartMachine....')
print('agro_trait: Yield, PH, EH, DTT, DTA, DTS')
stop('Try again!')
}

source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")
load("~/machine_learning_project/train_again/REP_AND_FOLD_SET.rda")
source("~/R_public_function/useful_function.R")
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", rowNames = F, colNames = T)

mutate_NA_toMean <- function(Vec) {
Vec [which(is.na(Vec))] <- mean(Vec, na.rm = T)
Vec
}

if(args [4] == 'Yield')
{
Y <- ourdata[, ncol(ourdata)]
Y <- Y[-which(is.na(Y))]
Y <- Y[-excluded_observed]
} else {
   Y <- openxlsx::read.xlsx(
   '~/machine_learning_project/2012_by804_b73_agro_all.xlsx',
   rowNames = F, colNames = T
   )
  Y <-
   ourdata %>% .[, 1:2] %>% set_names(c('Line1', 'Line2')) %>%
   left_join(
   Y, by = c('Line2' = 'Line')
   ) %>% .[-c(1:2), ] %>% mutate(across(PH:DTS, mutate_NA_toMean)) %>%
   select(args [4]) %>% . [-excluded_observed, ]
}


# 数据预处理----

omics_file <-
c('imputed_phenomics_data_sacled.txt',
'imputed_Meta_data_sacled.txt',
'imputed_Bin_data_sacled.txt',
'imputed_Meta_data_sacled.txt') %>%
set_names(c('Phenome', 'metabolome', 'Genome', 'Early_metabolome'))


read_X_data <- function(omics, omics_file2 = omics_file) {
  FILE = str_c("~/machine_learning_project/train_again/",  omics_file2[omics])
  FILE <- read.table(FILE, header = T, sep = "\t", check.names = F)
  if(omics != 'Early_metabolome') {
    FILE
  } else {
    FILE[, 1:69]
  }
}

impuated_X <- read_X_data(args[2])

Var_feature <- read.table(
  str_c("~/machine_learning_project/train_again/", args[1], "_", args[2], "_mean_IMP_result.txt"),
header = T, sep = "\t", check.names = F)

# -------------- training and validation sets
cl <- makePSOCKcluster(3)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g") # 用来提升初始的java内存，防止内存限制报错

get_model_params_combn <- function(MODEL2) {
  VARNUM <- 50

  if(MODEL2 == 'lasso') {
    VARNUM <- as.numeric(args[5])
  }

  MIDDD <-
  if(MODEL2 == 'rf')
  {
    list(mtry = seq(1, 100, 1), varnum = VARNUM)
  } else if (MODEL2 == "bartMachine") {
    list(varnum = VARNUM, num_trees = seq(2, 300, 4),
         alpha = c(0.9, 0.95, 0.99), beta = c(2),
         k = c(2), nu = c(3))
  } else if (MODEL2 == "earth") {
    list(varnum = VARNUM, nprune = 2:60,
         degree = 1)
  } else if (MODEL2 == "enet") {
    list(varnum = VARNUM,
    lambda = seq(0.1, 1, 0.1),
    fraction = seq(0.1, 1, 0.01))
  } else if (MODEL2 == "gaussprRadial") {
    list(varnum = VARNUM,
    sigma = c(seq(0.01, 1, 0.01))
    )
  } else if (MODEL2 == "gbm") {
    list(varnum = VARNUM,
        interaction.depth = seq(4, 20, 2),
        n.trees = seq(13, 30, 2),
        shrinkage = c(0.05),
        n.minobsinnode = c(2:4)
        )
  } else if (MODEL2 == "lasso") {
     list(varnum = VARNUM,
          fraction = c(seq(0.01, 1, 0.01))
          )
  } else if (MODEL2 == "mlp") {
     list(varnum = VARNUM,
          size = 2:40)
  } else if (MODEL2 == "neuralnet") {
     list(varnum = VARNUM,
          layer1 = c(1:5),
          layer2 = c(1:5),
          layer3 = c(1:5))
  } else if (MODEL2 == "pls") {
     list(varnum = VARNUM,
          ncomp = 1:50)
  } else if (MODEL2 == "svmLinear") {
     list(varnum = VARNUM,
          C = 1)
  } else if (MODEL2 == "leapSeq") {
     list(varnum = VARNUM,
          nvmax = seq(2, 30, 2))
  }

get_all_combination(MIDDD)
}

ALL_params <- get_model_params_combn(args[3]) %>% distinct
# -------------- Set data for training and validation sets

train_funcs <- function(MODEL, All_params) {
 tryCatch({
  varNum <- All_params['varnum'] %>% as.numeric

  FS_var <- head(Var_feature, varNum) %$% var
  df <- cbind(impuated_X %>% select(FS_var), Y)
  
  set.seed(123)
  fitControl <- trainControl( ## 10-fold CV
    method = "repeatedcv",
    # p = 0.9,
    # search = "random" #随机寻找参数组合
    returnResamp = "all", # returnResamp 可以选择最后返回的model参数类型，all，final，none
    summaryFunction = R_MAE_RMSE,
    index = FOLD_AND_REP_var
    # index = Index_list,
  )

  Fit <- train(
    Y ~ .,
    data = df,
    method = MODEL,
    # tuneLength = , #定义一共有多少个参数组合
    maximize = TRUE,
    #  verbose 在运行一些model时，不能设置，否则会产生
    # Error in R: Error in { : task 1 failed - "'x' must be numeric"
    # 的错误
    #  verbose = FALSE, #不会打印分析log,
    trControl = fitControl,
    tuneGrid = expand.grid(
      All_params %>% select(-varnum)
    ),
    metric = "R2",
  )

  # If the model does not have a specific IMP method,
  # then use a non-specific
  if(MODEL == 'gbm')
  {
      IMP <- varImp(Fit, scale = FALSE, useModel = F, nonpara = TRUE)
  } else {
      IMP <- varImp(Fit, scale = FALSE)
  }

  IMP <- cbind(rownames(IMP$importance), IMP$importance)
  colnames(IMP) <- c("Var", "Scale_Value")

  list(Resample = Fit$resample %>% mutate(varNum = varNum),
      Result = Fit$results %>% mutate(varNum =varNum),
      Var_IMP = IMP)
    }, error = function(x) {NULL})
}

clusterEvalQ(cl = cl, {
  library(tidyverse)
  library(magrittr)
  library(caret)
  library(rrBLUP)
})

clusterExport(cl = cl, varlist =
list(
  "train_funcs", "Var_feature", "impuated_X", "Y",
  "R_MAE_RMSE", "FOLD_AND_REP_var", 'args'
))

RESULT <-
parApply(cl = cl, ALL_params, 1, function(xx) {
    xx <-
    xx %>% as.data.frame() %>% t %>% as.data.frame() %>%
    map_df(as.numeric)
    train_funcs(args[3], xx)
})

Save_path <- str_c("~/machine_learning_project/train_again_V2/First_tuning/",
args[1], "_", args[2], "_", args[4], "_predict_result")

if(!dir.exists(Save_path))
{
  dir.create(Save_path)
}

params_range <-
ALL_params %>% apply(2, function(x) str_c(range(x), collapse = "_")) %>%
str_c(names(.), ., sep = "_") %>% str_c(., collapse = "__")


save(RESULT, file = str_c(Save_path, "/All_result_",
str_c(args, collapse = "_"), "_", params_range, ".rda"
))

Result2 <-
lapply(RESULT, function(datax) {
  datax$Result
}) %>% do.call(rbind.data.frame, .)

quick_write_table(Result2, str_c(Save_path, "/Mean_result_",
str_c(args, collapse = "_"), "_", params_range, ".txt"
))


stopImplicitCluster()
stopCluster(cl)


# First test the way of choosing nums of vars
# After testing, the way of choosing nums of vars is that
# Count the n variables that appear most frequently
# in all resamples

# # -------------- get all combination

# write.table(
# get_all_combination(
# list(
#     FS_method = c("RF", "Linear"),
#     Omics_type = c('Phenome', 'metabolome', 'Genome'),
#     Model =
#       c('rf', "bartMachine", "earth", "enet", "gaussprRadial", "gbm", "lasso",
#       "mlp", "neuralnet", "pls", "svmLinear", "leapSeq"),
#     Trait = c('PH', 'EH', 'DTT', 'DTA', 'DTS')
# )
# ), "~/machine_learning_project/train_again_V2/tuning_script/FS_method_Omics_type_Model_Trait.txt",
# row.names = F, col.names = F, sep = '\t', quote = F
# )

