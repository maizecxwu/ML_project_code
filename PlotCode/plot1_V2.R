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

# All_combns_best_train_results <-
# pmap_dfr(
# list(
# modelll = All_train_combns [, 1],
# Select_M = All_train_combns [, 2],
# omcis = All_train_combns [, 3],
# PHENO = All_train_combns [, 4]
# ), get_one_omics_all_model_resample_result
# )

# quick_write_table(All_combns_best_train_results,
# '~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results.txt'
# )


All_combns_best_train_results <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results.txt',
header = T, check.names = F
)

# -- plot for single RF selection result
# 开始绘制图片

get_combns_prediction_result <- function(OMICS, PHENOOO) {
DAT <- All_combns_best_train_results %>%
filter(omcis == OMICS, PHENO == PHENOOO)

MODEL_ORDER <-
DAT %>% filter(Select_M == 'RF') %>%
group_by(Model) %>% summarise(R = mean(R, na.rm = T)) %>%
left_join(Model_Name, by = c('Model' = 'original')) %>%
left_join(THREE_MODEL_TYPE, by = c('now' = 'Model')) %>%
arrange(Type, R)

DAT <- DAT %>% mutate(Model = factor(Model, levels = MODEL_ORDER$Model),
Select_M = factor(Select_M, levels = c('RF', 'Linear'))
)

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
  aes(x = Model, y = R, fill = Select_M)) +
  geom_rect(data = BACK_df,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  fill = Model_color_venn, inherit.aes = F,
  alpha = 0.4
  ) +
    stat_boxplot(
    geom = "errorbar", width = 0.1, size = 1,
    coef = 4, position = position_dodge(0.7),
   color =  "#6e6e6e", fill = NA
  ) +
  geom_boxplot(
    coef = 2, lwd = 1, size = 1, width = 0.3,
    alpha = 1, position = position_dodge(0.7),
    color =  "#6e6e6e",
  ) +
  scale_fill_manual(values = Time_color, name = NULL,
  labels = c('non-Linear i-trait', 'Linear i-trait')
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
  theme(axis.text.x = element_text(color = 'black'),
  legend.position = c(0, 1),
  legend.justification = c(0, 1)
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave(filename = str_c(Save_Path,
str_c(OMICS, PHENOOO, sep = '_'), ".pdf"),
plot = main_plot, width = 16, height = 9)
}

All_COMBNS <-
All_train_combns %>% select(Var3:Var4) %>% distinct()

pmap(
list(
OMICS = All_COMBNS [, 1],
PHENOOO = All_COMBNS [, 2]
), get_combns_prediction_result
)

# # -------------- save the best result from RF and Linear Phenome result

# quick_write_table(RF_Phenome_Best_lm_rf_result,
# str_c(Save_Path, "RF_Phenome_Best_lm_rf_result_cor_result.txt"))

# -------------- plot the venn plot of non-Linear and Linear features

All_Featrues_imp <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results_VAR_IMP.txt',
header = T, check.names = F
)

get_omics_top50_var_overlap <- function(OMICSCS) {

plot_df <-
All_Featrues_imp %>% filter(omcis == OMICSCS) %>%
group_by(Select_M) %>% summarise(Var = unique(Var))
# ------- 开始绘制Venn 图，展示两种方式选择的 top 100 features 的overlap数目
venn_plot <-
  ggvenn(list(`non-Linear` = plot_df %>% filter(Select_M == 'RF') %$% Var,
              Linear = plot_df %>% filter(Select_M == 'Linear') %$% Var),
         c("non-Linear", "Linear"),
         fill_color = Time_color,
         set_name_color = Time_color,
         digits = 0,
         stroke_size = 3,
         set_name_size = 14,
         text_size = 10
  ) + theme(plot.margin = margin(r = 5, l = 5, t = 5, b = 5)) +
  labs(x = "ll")

ggsave(filename = str_c(Save_Path,
str_c(OMICSCS), "_RF_Linear_top50_overlap.pdf"),
plot = venn_plot, width = 8, height = 9)

}

map(unique(All_train_combns$Var3), get_omics_top50_var_overlap)


# --------------  plot the linear and non-linear features cor denstity

get_OMICS_n_each_self_cor_result <- function(OMICSCS) {

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

impuated_X <- read_X_data(OMICSCS)

plot_df <-
All_Featrues_imp %>% filter(omcis == OMICSCS) %>%
group_by(Select_M) %>% summarise(Var = unique(Var))

Name_lm_cor <- cor(impuated_X[, plot_df %>% filter(Select_M == 'Linear') %$% Var],
use = "pairwise.complete.obs")
Name_rf_cor <- cor(impuated_X[, plot_df %>% filter(Select_M == 'RF') %$% Var],
use = "pairwise.complete.obs")

Name_lm_cor <- abs(Name_lm_cor)
Name_rf_cor <- abs(Name_rf_cor)
# --------------------- heatmap to show the linear and non- top 100 var's self-correlation ------------------------

get_self_cor_heatmap_funcs <- function(data) {
pheatmap(data, cluster_row = T, cluster_col = T, 
            show_rownames = F, border = FALSE,
             show_colnames = F,
            color = colorRampPalette(c("#543005",    "#8c510a",    "#bf812d",
                    "#dfc27d",    "#f6e8c3",    "#f5f5f5",
                "#c7eae5",   "#80cdc1",    "#35978f",
                "#01665e",    "#003c30"))(100),
                  cellwidth = 4, cellheight = 4, fontsize = 20,
                  treeheight_row = 0, treeheight_col = 0
                )
}

pdf(str_c(Save_Path,
str_c(OMICSCS), "Linear_top50_self_cor_heatmap.pdf"), width = 8, height = 8)
plot <- get_self_cor_heatmap_funcs(Name_lm_cor)
print(plot)
dev.off()
pdf(str_c(Save_Path,
str_c(OMICSCS), "RF_top50_self_cor_heatmap.pdf"), width = 8, height = 8)
plot <- get_self_cor_heatmap_funcs(Name_rf_cor)
print(plot)
dev.off()

}

map(unique(All_train_combns$Var3), get_OMICS_n_each_self_cor_result)
