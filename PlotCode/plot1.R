# -------------- color
source("~/machine_learning_project/train_again/plot/plot_color_match.R")
source("~/R_public_function/R_theme_bar_box.R")
source('~/R_public_function/useful_function.R')
# -------------- save path
Save_Path <- '~/machine_learning_project/train_again_V2/plot/Single_prediction_plot/'

# -------------- plot the phenomic prediction best result
get_one_omics_all_model_resample_result <-
function(modelll, Select_M, omcis, PHENO) {
  load(str_c(
  '~/machine_learning_project/train_again_V2/get_best_params_result/',
  Select_M, '_', omcis, '_', PHENO, '/',
  modelll, '.rda'))

  if (modelll %in% c("rrBLUP", "lightGBM"))
  {
  model_result %>% dplyr::select(R, RMSE, MAE, varNum) %>%
  mutate(Model = modelll, Select_M = Select_M, omcis = omcis, PHENO = PHENO)
  } else {
  model_result$Resample %>% dplyr::select(R, RMSE, MAE, varNum) %>%
  mutate(Model = modelll, Select_M = Select_M, omcis = omcis, PHENO = PHENO)
  }
}

#  -------------- plot ALL select_M All omics All trait result
All_train_combns  <-
list(
Model_order_all,
c('RF', 'Linear'),
All_omics,
All_PHENO
) %>% get_all_combination()

All_combns_best_train_results <-
pmap_dfr(
list(
modelll = All_train_combns [, 1],
Select_M = All_train_combns [, 2],
omcis = All_train_combns [, 3],
PHENO = All_train_combns [, 4]
), get_one_omics_all_model_resample_result
)

quick_write_table(All_combns_best_train_results,
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results.txt'
)

All_combns_best_train_results %>% head
# -- plot for single RF selection result
# 开始绘制图片

get_combns_prediction_result <- function(selection_M, OMICS, PHENOOO) {
DAT <- All_combns_best_train_results %>%
filter(Select_M == selection_M, omcis == OMICS, PHENO == PHENOOO)

MODEL_ORDER <-
DAT %>% group_by(Model) %>% summarise(R = mean(R, na.rm = T)) %>%
left_join(Model_Name, by = c('Model' = 'original')) %>%
left_join(THREE_MODEL_TYPE, by = c('now' = 'Model')) %>%
arrange(Type, R)

DAT <- DAT %>% mutate(Model = factor(Model, levels = MODEL_ORDER$Model))

BACK_df <- data.frame(
ymin = -Inf,
ymax = Inf,
xmin = c(-Inf, 6.5, 11.5),
xmax = c(6.5, 11.5, Inf)
)

Model_color_venn

main_plot <-
  ggplot(
    DAT %>% filter(R > 0),
  aes(x = Model, y = R)) +
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
  theme(axis.text.x = element_text(color = 'black')
  )

ggsave(filename = str_c(Save_Path,
str_c(selection_M, OMICS, PHENOOO, sep = '_'), ".pdf"),
plot = main_plot, width = 16, height = 9)
}

All_COMBNS <-
All_train_combns %>% select(Var2:Var4) %>% distinct()

pmap(
list(
selection_M = All_COMBNS [, 1],
OMICS = All_COMBNS [, 2],
PHENOOO = All_COMBNS [, 3]
), get_combns_prediction_result
)

# -------------- save the best result from RF and Linear Phenome result

quick_write_table(RF_Phenome_Best_lm_rf_result,
str_c(Save_Path, "RF_Phenome_Best_lm_rf_result_cor_result.txt"))
