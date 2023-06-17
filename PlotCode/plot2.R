# -------------- color
source("~/machine_learning_project/train_again/plot/plot_color_match.R")
source("~/R_public_function/R_theme_bar_box.R")
source('~/R_public_function/useful_function.R')
# -------------- save path
Save_Path <- '~/machine_learning_project/train_again_V2/plot/Model_compare/'


get_one_omics_all_model_var_IMP_result <-
function(modelll, Select_M, omcis, PHENO) {
  load(str_c(
  '~/machine_learning_project/train_again_V2/get_best_params_result/',
  Select_M, '_', omcis, '_', PHENO, '/',
  modelll, '.rda'))

  if (!(modelll %in% c("rrBLUP", "lightGBM")))
  {
    model_result$Var_IMP %>% dplyr::rename(IMP = Scale_Value) %>%
    mutate(Model = modelll, Select_M = Select_M,
    omcis = omcis, PHENO = PHENO) %>%
    mutate(Var = str_replace_all(Var, '`', '')) %>%
    mutate(Var = str_replace_all(Var, "\\\\", ''))
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

All_combns_best_train_results_IMP <-
pmap_dfr(
list(
modelll = All_train_combns [, 1],
Select_M = All_train_combns [, 2],
omcis = All_train_combns [, 3],
PHENO = All_train_combns [, 4]
), get_one_omics_all_model_var_IMP_result
)

quick_write_table(All_combns_best_train_results_IMP,
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results_VAR_IMP.txt'
)

plot_COMBNS_Var_VENN <-
function(MODELSS = c('pls', 'rf', 'gaussprRadial'),
selection_M, OMICS, PHENOOO) {

DAT <-
All_combns_best_train_results_IMP %>%
filter(Model %in% MODELSS, Select_M == selection_M,
omcis == OMICS, PHENO == PHENOOO)

VAR_order <-
DAT %>% filter(Model == MODELSS[1]) %>% arrange(IMP) %$% Var

DAT_plot <-
DAT %>% arrange(Model, IMP) %>%
mutate(IMP_rank = rep(1:(nrow(.) / 3), 3)) %>%
mutate(Var = factor(Var, levels = VAR_order)) %>%
mutate(Model = factor(Model, levels = MODELSS))

MODEL_plot_Name <-
Model_Name$now [match(MODELSS, Model_Name$original)]

DAT_plot [which(DAT_plot$Model == 'pls'), ]$IMP_rank <- 
DAT_plot [which(DAT_plot$Model == 'pls'), ]$IMP_rank + 0.5

plot <-
ggplot(data = DAT_plot) +
geom_point(aes(x = Var, y = IMP_rank, color = Model), size = 5) +
geom_line(aes(x = Var, y = IMP_rank, group = Model), size = 1, alpha = 0.5) +
scale_color_manual(values = Model_color_venn %>% as.character(),
labels = MODEL_plot_Name, name = NULL) +
theme_classic(base_size = 28, base_line_size = 0.1, base_rect_size = 0.1) +
theme(legend.position = c(1, 0),
legend.justification = c(1, 0),
axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
axis.title.y = element_text(face = 'bold')
) +
labs(x = NULL, y = 'Feature important value rank')


ggsave(filename = str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
str_c(selection_M, OMICS, PHENOOO, sep = '_'), '.pdf'),
plot = plot, width = 16, height = 9)


## -------------- get the top 20 var cor pvalue with pheno

get_model_top_20_var <- function(MODELLLL) {
DAT_plot %>% arrange(Model, -IMP) %>%
filter(Model == MODELLLL) %>% head(20)
}

Top_20_model_var <-
map_dfr(MODELSS, get_model_top_20_var)


get_OMICS_Ttest_result <- function() {
FILE_Path <- '~/machine_learning_project/T_test_result/'
if(OMICS == 'Genome')
{
read.table(str_c(FILE_Path, 'Bin_Agro_T_test_result.txt'),
header = T, sep = '\t', check.names = F)
} else if (OMICS == "metabolome") {
read.table(str_c(FILE_Path, 'Meta_Agro_cor_p_value_result.txt'),
header = T, sep = '\t', check.names = F)
} else {
read.table(str_c(FILE_Path, 'Pheno_Agro_cor_p_value_result.txt'),
header = T, sep = '\t', check.names = F)
}
}

Ttest_result <-
get_OMICS_Ttest_result()

Pheno_ref <-
Ttest_result$Agro %>%
set_names(c('Yield', 'PH', 'EH', 'LL', 'LW', 'TL', 'Biomass', 'TBN'))



get_each_model_sig_cor_per <- function(MODELSSSSS) {
VAR_top_20 <-
Top_20_model_var %>% filter(Model == MODELSSSSS) %$% Var

SIG_NUM <-
sum(Ttest_result %>% filter(Agro == Pheno_ref [PHENOOO]) %>%
select(VAR_top_20) %>% as.numeric() < 0.05)

data.frame(
percentages = c(SIG_NUM / 20, 1 - SIG_NUM / 20), 
Type = c('Sig', 'N-Sig')
) %>% mutate(Model = MODELSSSSS)

}

plot_df <-
map_dfr(MODELSS, get_each_model_sig_cor_per) %>%
mutate(Model = factor(Model, levels = MODELSS),
Type = factor(Type, levels = c('Sig', 'N-Sig')))

plot2 <-
ggplot(data = plot_df) +
geom_bar(aes(x = Model, y = percentages, fill = Type), stat = 'identity') +
theme_bw(base_size = 28, base_line_size = 0.1, base_rect_size = 0.1) +
theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, color = 'black')) +
scale_fill_manual(values = Time_color) +
scale_x_discrete(labels = MODEL_plot_Name) +
labs(x = NULL, y = 'Percentage')

ggsave(filename = str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
str_c(selection_M, OMICS, PHENOOO, sep = '_'), '_sig_cor_percentage.pdf'),
plot = plot2, width = 9, height = 8)

}


All_COMBNS <-
All_train_combns %>% select(Var2:Var4) %>% distinct() %>%
filter(Var4 %in% c('Yield', 'PH', 'EH'))

pmap(
list(
selection_M = All_COMBNS [, 1],
OMICS = All_COMBNS [, 2],
PHENOOO = All_COMBNS [, 3]
), plot_COMBNS_Var_VENN
)



