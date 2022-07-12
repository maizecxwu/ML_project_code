library(dplyr)
library(caret)
library(parallel)
library(doParallel)
library(openxlsx)
library(data.table)
library(ggplot2)
library(stringi)
library(stringr)
library(ggpubr)
library(ggvenn)
library(tidyverse)
library(magrittr)

ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", rowNames = F, colNames = T)
# ourdata<-read.xlsx("D:/win10data/machine learning project/8801a79384a6d889.xlsx",rowNames = F,colNames = T)
Y <- ourdata[, ncol(ourdata)] 
rownames(ourdata) <- ourdata$line.name
ourdata <- ourdata[, -c(1:2)]
X <- ourdata[, -ncol(ourdata)] 
X <- X[-which(is.na(Y)), ]
Y <- Y[-which(is.na(Y))]
Impute_Method <- preProcess(X, method = c("knnImpute"))

impuated_X <- predict(Impute_Method, X)
 
Early_Name <- colnames(impuated_X)[as.vector(sapply(1:8, function(x){seq(x,736,16)}))]

Late_Name <- colnames(impuated_X)[as.vector(sapply(9:16, function(x){seq(x,736,16)}))]

Best_model <- list()

Var_Time_type <- list()

Var_Time_IMP_type <- list()

ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)

Bin <- openxlsx::read.xlsx("~/machine_learning_project/bin_inf_predict_yield/Bin_inf.xlsx",
                             rowNames = F, colNames = T, rows = 3:2499)

Bin <- Bin %>% select(-c(Chromosome, `genetic.position.(cM)`)) 

rownames(Bin) <- Bin$bin

Bin <- Bin %>% mutate(bin = NULL) %>% t() %>% as.data.frame()

Bin$Line <- rownames(Bin)

Yield_Bin <- left_join(ourdata %>% select(Yield_Blup, 2),
                        Bin, by = c("line.name" = "Line"))

# Eliminate individuals that are missing too many
 Yield_Bin <- Yield_Bin[-c(1:2), ]



Model_order <-
c(
  "bartMachine", "earth", "enet",
  "lasso", "mlp", "neuralnet",
  "pls", "rf", "gaussprRadial", "leapseq"
)

Model_order_all <-
c("rrBLUP", "leapseq", "pls", "lasso", "enet",
  "earth", "bartMachine", "svmLinear",
  "gaussprRadial", "gbm", "lightgbm", "rf",
  "mlp", "neuralnet")

X_axis_labels_all <-
c("rrBLUP", "SR", "PLS", "LASSO", "EN", "MARS", "BART",
"SVR", "GaussprRadial", "GBM", "LightGBM", "RF", "MLP", "ANN")

# Sort by predicted mean
# Convert the model name to correspond
Model_Name <- data.frame(original = Model_order_all, now = X_axis_labels_all)




# -------------- Manually obtain and save prediction results under optimal parameters
# -------------- only Phenomics
#########################################
#########################################
#########################################
# ----- R distributions when manually summarizing the optimal parameters of all models in phenotype group testing ------------
# the best tune model 
results = "resample"

rrBlup_result_rf <- read.table("~/machine_learning_project/feature_tune_parallel/5_model_results/rrblup_result_2_200_var.txt", header = T, check.names = F, sep = "\t")

rrBlup_result_lm <- read.table("~/machine_learning_project/feature_tune_parallel/5_model_results/rrblup_result_2_108_var_lm.txt", header = T, check.names = F, sep = "\t")

rrBlup_result_rf %>% group_by(varNum) %>% summarise(Mean = mean(R)) %>% as.data.frame() %>% filter(Mean == max(Mean))

# rf :

RESult <-
  c(
    paste0("leapSeq20_2_120_",results,"_lm.txt"),
    paste0("gbm_1_50_",results,"_rf.txt"),
    paste0("bartMachine_62_120_",results,"_rf.txt"),
    paste0("earth_2_30_degree_1_5_",results,"_rf.txt"),
    paste0("enet_",results,"_rf.txt"),
    paste0("lasso_0.6_0.9_",results,"_rf.txt"),
    paste0("lightGBM_2_60_",results,"_rf.txt"),
    paste0("mlp_",results,"_rf.txt"),
    paste0("neuralnet_",results,"_rf.txt"),
    paste0("pls_1_50_",results,"_rf.txt"),
    paste0("rf_1_29_",results,"_rf.txt"),
    paste0("gaussprRadial_0.02_0.1_",results,"_rf.txt")
  ) [MM_NUM]

lm :
  leapSeq6_2_110_results_lm.txt
gaussprRadial_0.01_0.1_results_rf.txt
RESult <- c(
  paste0("gbm_1_50_",results,"_rf.txt"),
  paste0("bartMachine_2_120_",results,"_rf.txt"),
  paste0("earth_2_30_",results,"_rf.txt"),
  paste0("enet_",results,"_rf.txt"),
  paste0("lasso_0.6_0.9_",results,"_rf.txt"),
  paste0("mlp_",results,"_rf.txt"),
  paste0("neuralnet_",results,"_rf.txt"),
  paste0("pls_1_50_",results,"_rf.txt"),
  paste0("rf_",results,"_rf.txt"),
  paste0("gaussprRadial_0.01_0.1_",results,"_rf.txt")
)[MM_NUM]

jjj <- read.table(
  file = paste0(
    "~/machine_learning_project/feature_tune_parallel/5_model_results/",
    RESult
  ),
  header = T, check.names = F, sep = "\t"
)

Best_model[[MM_NUM]] <-
  rrBlup_result_rf %>% filter(
    #  layer2 == 1, layer3 ==1, 
    # interaction.depth == 5,
    # n.trees == 17,
    # shrinkage == 0.1,
    # n.minobsinnode == 5,
    # num_trees == 118,
    # alpha == 0.95,
    # beta == 2,
    # k == 2,
    # nu == 3,
    # nprune == 8,
    # degree == 1,
    # lambda == 1,
    # fraction == 0.68, 
    # size == 3,
    # layer1 == 2,  layer2 == 1,  layer3 == 4,
    # ncomp == 3,
    # mtry == 1,
    # sigma == 0.01,
    # num_leaves == 2,
    # C == 1,
    # nvmax == 20,
    varNum == 66) %>%
  mutate(Model = "rrBLUP")


light <- read.table("~/machine_learning_project/feature_tune_parallel/5_model_results/lightGBM_2_60_resample_rf.txt",
                    header = T, check.names = F, sep = "\t")

light %>% group_by(num_leaves, varNum) %>%
  summarise( R_Mean = mean(R, na.rm =T)) %>%
  as.data.frame() %>% 
  filter(R_Mean == max(R_Mean))

svmLinear <- read.table("~/machine_learning_project/feature_selection_recursive/different_numsofvars_pred/rf_selection_methods_Result/svmLinear_resample_rf.txt",
                        header = T, check.names = F, sep = "\t")

svmLinear  %>% group_by(C, varNum) %>%
  summarise( R_Mean = mean(R, na.rm =T)) %>% 
  as.data.frame() %>% 
  filter(R_Mean == max(R_Mean))

leapseq <- read.table("~/machine_learning_project/use_stepwisefromZhang_to_validation/pdf/diff_NumsofVars_influence/result/leapSeq20_2_120_resample_lm.txt",
                      header = T, check.names = F, sep = "\t")

leapseq  %>% group_by(nvmax, varNum) %>%
  summarise( R_Mean = mean(R, na.rm =T)) %>% 
  as.data.frame() %>% 
  filter(R_Mean == max(R_Mean))

# -------------- Save the results under the optimal parameters first
save(Best_model, file = "~/machine_learning_project/Intermediate_data/only_phenomics_best_14_model_results_by_non_linear_methods.rda")

# -------------- make sure that result is right with result file

Check_R_mean_funcs <- function(data, orig_name) {
  lapply(data, function(dat) {
    dat %>% rename(Model = orig_name) %>%
    group_by(Model) %>% summarise(Mean = mean(R, na.rm = T))
  })
}
get_all_evaluation_param <- function(data, model_name) {
  lapply(data, function(dat) {
    dat %>% select(R_Fit:MAE, varNum, model_name)
  }) %>% do.call(rbind.data.frame, .)
}

Check_R_mean_funcs(Best_model, "Model")

source("~/R_public_function/useful_function.R")

quick_write_table(get_all_evaluation_param(Best_model, "Model"),
"~/machine_learning_project/Intermediate_data/only_phenomics_best_14_model_results_by_non_linear_methods_exclude_tune_params.txt")


# -------------- only metabolome
# ------------------------------- get the Meta best result -------------
# ------- get the best result under optimal parameters manually -------------------

i ="neuralnet"

iii = Model_Name %>% filter(original == i) %$% now

for(i in c("rrBLUP", "leapSeq", "pls", "lasso", "enet",
            "earth", "bartMachine", "svmLinear",
            "gaussprRadial", "gbm", "lightGBM", "rf",
            "mlp", "neuralnet"))
{
  Mid <- read.table(paste0("~/machine_learning_project/Meta_analysis/FS_result/rrblup_result.txt"),
  header = T, check.names = F, sep = "\t")

  Mid <- read.table(paste0("~/machine_learning_project/Meta_analysis/FS_result/", i,"_1_29_resample_rf.txt"),
  header = T, check.names = F, sep = "\t")

#  Mid <- read.table("~/machine_learning_project/Meta_analysis/FS_result/rrblup_result.txt", header = T, 
#                     check.names = F, sep = "\t")
  
  head(Mid)

  Mid %>% 
  # filter(varNum == "Early") %>% 
  # group_by(C, varNum) %>%
  # group_by(mtry, varNum) %>%
  group_by(
    # fraction,
    # nvmax,
    # ncomp,
    # lambda, fraction,
    # num_trees, alpha, beta, k, nu,
    # C,
    # sigma,
    #  interaction.depth,n.trees ,shrinkage ,n.minobsinnode,
    # nprune, degree,
    # num_leaves,
    # mtry,
    # size,
    layer1, layer2, layer3,
    varNum) %>%
  summarise(R_Mean = mean(R, na.rm = T)) %>% 
  # as.data.frame() %>%
  # group_by( varNum, mtry) %>%
  # summarise(R_Max = max(R_Mean, na.rm = T)) %>%
  as.data.frame() %>%
  filter(R_Mean == max(R_Mean))


}

ALL <- list()

ALL[[i]] <- 
  Mid %>%
    filter(varNum == 2,
    # layer1 == 1, layer2 == 5, layer3 == 4,
    # ncomp == 1,
    # nprune ==2, degree == 2
    # num_trees == 118, alpha == 0.95, beta ==2, k ==2, nu==3,
    # lambda == 1, fraction == 1,
    # fraction == 0.9,
    # C == 1,
    # sigma == 1e-06,
    # interaction.depth == 3,n.trees ==5 ,shrinkage == 0.1 ,n.minobsinnode == 1,
    # nvmax == 6,
    # mtry == 1,
    # size == 1,
    layer1 == 1, layer2 == 5, layer3 == 4,
    # num_leaves == 4
    ) %>% 
    # filter((sigma == 0.009 & varNum == 30)) %>%
    # filter((num_trees == 152 & varNum == 20)) %>%
    mutate(model = iii)

# -------------- Save detailed parameters
save(ALL, file = "~/machine_learning_project/Intermediate_data/only_Metabolomics_best_14_model_results.rda")

lapply(ALL, function(dat) {
  dat %>% group_by(model) %>% summarise(Mean = mean(R, na.rm = T))
})

Final_Re <-
lapply(ALL, function(dat) {
  dat %>% select(R_Fit:MAE, varNum, model)
}) %>% do.call(rbind.data.frame, .)

quick_write_table(Final_Re, "~/machine_learning_project/Intermediate_data/only_Metabolomics_best_14_model_results_exclude_tune_params.txt")

# write.table(ALL, "~/machine_learning_project/Plot_result/only_Meta_Best_result.txt", 
#             row.names = F, col.names = T, sep = "\t", quote = F)


# -------------- only genomes
# ----------------------------------- get the Bin best result -------------------------
# ----------- get the best result under optimal param based on genome data -------------
i = "neuralnet"

iii = Model_Name %>% filter(original == i) %$% now

for(i in c("rrBLUP", "leapSeq", "pls", "lasso", "enet",
            "earth", "bartMachine", "svmLinear",
            "gaussprRadial", "gbm", "lightGBM", "rf",
            "mlp", "neuralnet"))
{

  Mid <- read.table(paste0("~/machine_learning_project/bin_inf_predict_yield/FS_result/", i,"_1_29_resample_rf.txt"),
  header = T, check.names = F, sep = "\t")

#  Mid <- read.table("~/machine_learning_project/bin_inf_predict_yield/FS_result/leapSeq_nvmax6_1_29_resample_rf.txt", header = T, 
#                     check.names = F, sep = "\t")

#   Mid <- read.table("~/machine_learning_project/bin_inf_predict_yield/FS_result/rrblup_result.txt", header = T, 
#                     check.names = F, sep = "\t")

  head(Mid)

  Mid %>% 
  # filter(varNum == "Early") %>% 
  # group_by(C, varNum) %>%
  # group_by(mtry, varNum) %>%
  group_by(
    # fraction,
    # nvmax,
    # ncomp,
    # lambda, fraction,
    # num_trees, alpha, beta, k, nu,
    # C,
    # sigma,
    #  interaction.depth,n.trees ,shrinkage ,n.minobsinnode,
    # nprune, degree,
    # num_leaves,
    # mtry,
    # size,
    layer1, layer2, layer3,
    varNum) %>%
  summarise(R_Mean = mean(R, na.rm = T)) %>% 
  # as.data.frame() %>%
  # group_by( varNum, mtry) %>%
  # summarise(R_Max = max(R_Mean, na.rm = T)) %>%
  as.data.frame() %>%
  filter(R_Mean == max(R_Mean))


}

ALL <- list()

ALL[[i]] <- 
  Mid %>%
    filter(varNum == 8,
    # layer1 == 1, layer2 == 5, layer3 == 4,
    # ncomp == 2,
    # nprune ==5, degree == 1
    # num_trees == 118, alpha == 0.95, beta ==2, k ==2, nu==3,
    # lambda == 1, fraction == 1,
    # fraction == 0.63,
    # C == 1,
    # sigma == 0.05,
    # interaction.depth == 2,n.trees ==5 ,shrinkage == 0.1 ,n.minobsinnode == 1,
    # nvmax == 6,
    # mtry == 1,
    # size == 16,
    layer1 == 1, layer2 == 3, layer3 == 4,
    # num_leaves == 4
    ) %>% 
    # filter((sigma == 0.009 & varNum == 30)) %>%
    # filter((num_trees == 152 & varNum == 20)) %>%
    mutate(model = iii)

# write.table(ALL, "~/machine_learning_project/Plot_result/only_Bin_Best_result.txt", 
#             row.names = F, col.names = T, sep = "\t", quote = F)

# -------------- save all detailed params about models
save(ALL, file = "~/machine_learning_project/Intermediate_data/only_Genomes_best_14_model_results.rda")

# -------------- check the mean R right ?
Check_R_mean_funcs(ALL, "model")

quick_write_table(
get_all_evaluation_param(ALL, "model"),
"~/machine_learning_project/Intermediate_data/only_Genomes_best_14_model_results_exclude_tune_params.txt")


# -------------- Combining multiple omics
# --------------------------- plot the 3 model prediction R with early Pheno Meta and all Bin-------------------------

ALL <- list()

# # Best Resulting Model for Combination Prediction
# rf_3_Meta_1_10_results_rf.txt
# gaussprRadial_1_Meta_1_10_results_rf.txt
# bartMachine_1_Meta_1_10_results_rf.txt

#  All Pheno                    Early Pheno 
#                            297                            297 
#              Early Pheno + Bin Early Pheno + Bin + Early Meta 
#                            297                            297 
#       Early Pheno + Early Meta 
#                            297 
# BART   RF   GaussprRadial

i = 12

jj <- read.table(
  file = paste0(
    "~/machine_learning_project/Early_Phenomics_Bin_Meta/result/"
  ),
  header = T, check.names = F, sep = "\t"
)

  jj %>%
  # filter(varNum == "Early") %>% 
  # filter(varNum == "Early") %>% 
  # group_by(mtry, varNum) %>%
  # group_by(sigma, varNum) %>%
  group_by(
  # num_trees,
  mtry,
  # sigma,
  varNum) %>%
  summarise(R_Mean = mean(R, na.rm = T)) %>%
  # filter(varNum == "Early") %>%
  # as.data.frame() %>%
  # group_by( varNum, mtry) %>%
  # summarise(R_Max = max(R_Mean, na.rm = T)) %>%
  as.data.frame() %>%
  filter(R_Mean == max(R_Mean))

ALL[[i]] <- 
  jj %>%
    filter(
      (
        mtry == 1
        # sigma == 0.02
        # num_trees == 160
      & varNum == 25
      )
      ) %>% 
    mutate(model = "RF", varNum2 = "Early Pheno + Bin + Early Meta")

ALL2 <- ALL

Best_rf <- load("~/machine_learning_project/Intermediate_data/only_phenomics_best_14_model_results_by_non_linear_methods.rda")

Best_model %>% lapply(function(dat) {
  dat %>% filter(Model %in% c("BART", "RF", "GaussprRadial")) %>%
  mutate(varNum2 = "All Pheno")
})

ALL <-
c(ALL2,
Best_model[c(3, 11, 12)] %>% lapply(function(dat) dat %>% mutate(varNum2 = "All Pheno")))

Check_R_mean_funcs(ALL[c(1:12)], "model")
Check_R_mean_funcs(ALL[c(13:15)], "Model")

save(ALL, file = "~/machine_learning_project/Intermediate_data/combination_prediction_3_models_result.rda")

get_all_evaluation_param(ALL[c(1:12)], "model")

get_all_evaluation_param2 <- function(data, model_name) {
  lapply(data, function(dat) {
    dat %>% select(R:MAE, varNum, varNum2, model_name)
  }) %>% do.call(rbind.data.frame, .)
}

quick_write_table(
  rbind(
get_all_evaluation_param2(ALL[c(1:12)], "model"),
get_all_evaluation_param2(ALL[c(13:15)], "Model")%>% rename(model = Model)),
"~/machine_learning_project/Intermediate_data/combination_prediction_3_models_result_exclude_tune_params.txt"
)

#  write.table(ALL, "~/machine_learning_project/Plot_result/All_Pheno_3_Type_Early_joint_prediction_best_result.txt", 
#                 row.names = F, col.names = T, sep = "\t", quote = F)

# ALL_2 <- ALL  %>% filter(varNum != "Late")






# -------------- Feature importance acquisition under optimal parameters -------------- 
# -------------- phenomics 
# The importance of variables in the optimal parameters of each model can only be obtained by manually calculating the parameters of the corresponding model.

rf_FS_best_IMP <-
list(leapSeq_IMP, BartMachine_IMP, earth_IMP, enet_IMP, lasso_IMP,
   mlp_IMP, neuralnet_IMP, pls_IMP, rf_IMP, gaussprRadial_IMP) 

for(x in 1:10)
{

rf_FS_best_IMP[[x]]$model <- c("leapseq",
        "bartMachine", "earth", "enet",
        "lasso", "mlp", "neuralnet",
        "pls", "rf", "gaussprRadial"
      )[x]
}

rf_FS_best_IMP <- do.call(rbind.data.frame, rf_FS_best_IMP)

# write.table(rf_FS_best_IMP, "~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
#                             row.names = F, col.names = T, quote = F, sep = "\t")

# ----------------- Get the IMP of the best model for different omics combinations for different models ------------------
ALL <- data.frame()

for(i in c("rf", "gaussprRadial", "bartMachine"))
{
  ALL <- rbind(ALL,
  read.table(str_c("~/machine_learning_project/Plot_result/Multi_omics_IMP/", i, "_best_IMP_Meta.txt"),
              header = T, check.names = F, sep = "\t") %>%
              mutate(model = i, Type = "Early Meta"))
}

# write.table(ALL, "~/machine_learning_project/Plot_result/Early_Phenomics_Early_Meta_Bin_and_Meta_Bin_IMP.txt",
#               row.names = F, col.names = T, sep = "\t", quote = F)