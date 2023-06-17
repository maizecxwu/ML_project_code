# -------------- color
source("~/machine_learning_project/train_again/plot/plot_color_match.R")
source("~/R_public_function/R_theme_bar_box.R")
source('~/R_public_function/useful_function.R')
# -------------- save path
Save_Path <- '~/machine_learning_project/train_again_V2/plot/Model_compare/'

# -------------- get 3 omics all model r mae rmse mean value table

Predict_result <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results.txt',
header = T
)

Mid_reslut <-
Predict_result %>% filter(RMSE <= 100, MAE <=100) %>%
group_by(Model, omcis, PHENO, Select_M) %>%
summarise(
R_Mean = mean(R, na.rm = T),
R_Median = median(R, na.rm = T),
R_min = min(R, na.rm = T),
R_max = max(R, na.rm = T),
RMSE_Mean = mean(RMSE, na.rm = T),
RMSE_Median = median(RMSE, na.rm = T),
RMSE_min = min(RMSE, na.rm = T),
RMSE_max = max(RMSE, na.rm = T),
MAE_Mean = mean(MAE, na.rm = T),
MAE_Median = median(MAE, na.rm = T),
MAE_min = min(MAE, na.rm = T),
MAE_max = max(MAE, na.rm = T),
) %>%
filter(PHENO == 'Yield', Select_M == 'RF') %>%
arrange(omcis) %>%
left_join(
Model_Name, by = c('Model' = 'original')
) %>% mutate(now = factor(now, levels = THREE_MODEL_TYPE$Model)) %>%
arrange(omcis, now)

write.table(
Mid_reslut,
'~/machine_learning_project/train_again_V2/plot/Yield_3_omics_RF_mean_prediction.csv',
row.names = F, col.names = T, sep = ',', quote = F
)



combination_result <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_Multi_omics_combns_best_train_results.txt',
header = T, sep = '\t'
)

Mid_reslut <-
combination_result %>% filter(RMSE <= 100, MAE <=100) %>%
group_by(Model, omcis, PHENO, Select_M) %>%
summarise(
R_Mean = mean(R, na.rm = T),
R_Median = median(R, na.rm = T),
R_min = min(R, na.rm = T),
R_max = max(R, na.rm = T),
RMSE_Mean = mean(RMSE, na.rm = T),
RMSE_Median = median(RMSE, na.rm = T),
RMSE_min = min(RMSE, na.rm = T),
RMSE_max = max(RMSE, na.rm = T),
MAE_Mean = mean(MAE, na.rm = T),
MAE_Median = median(MAE, na.rm = T),
MAE_min = min(MAE, na.rm = T),
MAE_max = max(MAE, na.rm = T),
) %>%
filter(PHENO == 'Yield', Select_M == 'RF') %>%
arrange(omcis) %>%
left_join(
Model_Name, by = c('Model' = 'original')
) %>% mutate(now = factor(now, levels = THREE_MODEL_TYPE$Model)) %>%
arrange(omcis, now)

write.table(
Mid_reslut,
'~/machine_learning_project/train_again_V2/plot/Yield_4_combn_omics_RF_mean_prediction.csv',
row.names = F, col.names = T, sep = ',', quote = F
)
