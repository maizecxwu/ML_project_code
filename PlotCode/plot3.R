

# -------------- color
source("~/machine_learning_project/train_again/plot/plot_color_match.R")
source("~/R_public_function/R_theme_bar_box.R")
source('~/R_public_function/useful_function.R')
# -------------- save path
Save_Path <- '~/machine_learning_project/train_again_V2/plot/Multi_prediction_plot/'

# -------------- plot the phenomic prediction best result
get_one_omics_all_model_resample_result <-
function(modelll, Select_M, PHENO, omcis, omcis2 = NULL, omcis3 = NULL) {
  load(str_c(
  '~/machine_learning_project/train_again_V2/get_best_params_result/',
  Select_M, '_', str_c(omcis, omcis2, omcis3, sep = '_'), '_', PHENO, '/',
  modelll, '.rda'))

  if (modelll %in% c("rrBLUP", "lightGBM"))
  {
  model_result %>% dplyr::select(R, RMSE, MAE, varNum) %>%
  mutate(Model = modelll, Select_M = Select_M, omcis = omcis, PHENO = PHENO)
  } else {
  model_result$Resample %>% dplyr::select(R, RMSE, MAE, varNum) %>%
  mutate(Model = modelll, Select_M = Select_M,
  omcis = str_c(omcis, omcis2, omcis3, sep = '_'),
  PHENO = PHENO)
  }
}

get_one_omics_all_model_resample_result(
modelll = 'rf', Select_M = 'RF', PHENO = 'Yield',
omcis = c('Genome', 'metabolome')
)

#  -------------- plot ALL select_M All omics All trait result
COMBNS <- read.table(
'~/machine_learning_project/train_again_V2/tuning_script/FS_method_Omics_type_Model_Trait_OMICS2.txt',
header = F
)

All_train_combns <-
COMBNS %>% set_names(c('Select_M', 'omcis', 'modelll', 'PHENO', 'omcis2')) %>%
as.list()

All_combns_best_train_results_V1 <-
pmap_dfr(
All_train_combns, get_one_omics_all_model_resample_result
)

COMBNS <- read.table(
'~/machine_learning_project/train_again_V2/tuning_script/FS_method_Omics_type_Model_Trait_OMICS2_OMICS3.txt',
header = F
)

All_train_combns <-
COMBNS %>% set_names(c('Select_M', 'omcis', 'modelll', 'PHENO', 'omcis2', 'omcis3')) %>%
as.list()

All_combns_best_train_results_V2 <-
pmap_dfr(
All_train_combns, get_one_omics_all_model_resample_result
)

All_combns_best_train_results <-
rbind(
All_combns_best_train_results_V1,
All_combns_best_train_results_V2
)

quick_write_table(All_combns_best_train_results,
'~/machine_learning_project/train_again_V2/plot/All_Multi_omics_combns_best_train_results.txt'
)

Single_omics <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results.txt',
header = T
)

Merged_Single_Multi_omics_results <-
rbind(Single_omics, All_combns_best_train_results)

NAMES_COMBN <-
data.frame(
orig = Merged_Single_Multi_omics_results$omcis %>% unique,
now = c("Genome", "metabolome", "Phenome", "Phenome + Genome",
"Phenome + metabolome", "Genome + metabolome", "Phenome + Genome + metabolome"),
combn_type = c(rep('single', 3), rep('2 omics', 3), '3 omics')
) %>% mutate(
combn_type = factor(combn_type, levels = c('single', '2 omics', '3 omics'))
)

# All_combns_best_train_results %>% head
# -- plot for single RF selection result
# 开始绘制图片

get_combns_prediction_result <- function(selection_M, PHENOOO, MODELLL) {
DAT <- Merged_Single_Multi_omics_results %>%
filter(Select_M == selection_M, PHENO == PHENOOO, Model == MODELLL)

MODEL_ORDER <-
DAT %>% group_by(omcis) %>% summarise(R = mean(R, na.rm = T)) %>%
left_join(NAMES_COMBN, by = c('omcis' = 'orig')) %>%
arrange(combn_type, R)

DAT <- DAT %>% mutate(omcis = factor(omcis, levels = MODEL_ORDER$omcis))

BACK_df <- data.frame(
ymin = -Inf,
ymax = Inf,
xmin = c(-Inf, 3.5, 6.5),
xmax = c(3.5, 6.5, Inf)
)

Model_color_venn

main_plot <-
  ggplot(
    DAT %>% filter(R > 0),
  aes(x = omcis, y = R)) +
  geom_rect(data = BACK_df,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  fill = Model_color_venn, inherit.aes = F,
  alpha = 0.4
  ) +
    stat_boxplot(
    geom = "errorbar", width = 0.1, size = 2,
    coef = 4, position = position_dodge(0.7),
   color =  "#6e6e6e", fill = NA
  ) +
  geom_boxplot(
    coef = 2, lwd = 2, size = 2, width = 0.3,
    alpha = 1, position = position_dodge(0.7),
    color =  "#6e6e6e", fill = "white",
  ) +
  # scale_color_manual(
  # values = Time_color,
  #  guide = guide_legend(nrow = 2), name = NULL,
  #  labels = c("non-Linear i-trait", "Linear i-trait")
  #  ) +
    scale_x_discrete(labels = MODEL_ORDER$now) +
  labs(title = NULL,
       y = "Prediction accuracy") +
  Theme_bar_box +
  theme(axis.text.x = element_text(color = 'black', angle = 20)
  )

ggsave(filename = str_c(Save_Path,
str_c(selection_M, PHENOOO, MODELLL, sep = '_'), ".pdf"),
plot = main_plot, width = 16, height = 9)
}

All_COMBNS <-
All_train_combns %>% select(Var2:Var4) %>% distinct()

pmap(
list(
selection_M = COMBNS %>% select(V1, V4, V3) %>% distinct %$% V1,
MODELLL = COMBNS %>% select(V1, V4, V3) %>% distinct %$% V3,
PHENOOO = COMBNS %>% select(V1, V4, V3) %>% distinct %$% V4
), get_combns_prediction_result
)

# -------------- save the best result from RF and Linear Phenome result

quick_write_table(RF_Phenome_Best_lm_rf_result,
str_c(Save_Path, "RF_Phenome_Best_lm_rf_result_cor_result.txt"))
