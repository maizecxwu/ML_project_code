

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

# All_combns_best_train_results_IMP <-
# pmap_dfr(
# list(
# modelll = All_train_combns [, 1],
# Select_M = All_train_combns [, 2],
# omcis = All_train_combns [, 3],
# PHENO = All_train_combns [, 4]
# ), get_one_omics_all_model_var_IMP_result
# )

# quick_write_table(All_combns_best_train_results_IMP,
# '~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results_VAR_IMP.txt'
# )

All_combns_best_train_results_IMP <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results_VAR_IMP.txt',
header = T, check.names = F
)

plot_COMBNS_Var_VENN <- function(MODELSS = c('pls', 'rf', 'gaussprRadial'),
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

# DAT_plot [which(DAT_plot$Model == 'pls'), ]$IMP_rank <- 
# DAT_plot [which(DAT_plot$Model == 'pls'), ]$IMP_rank + 0.5

# plot <-
# ggplot(data = DAT_plot) +
# geom_point(aes(x = Var, y = IMP_rank, color = Model), size = 5) +
# geom_line(aes(x = Var, y = IMP_rank, group = Model), size = 1, alpha = 0.5) +
# scale_color_manual(values = Model_color_venn %>% as.character(),
# labels = MODEL_plot_Name, name = NULL) +
# theme_classic(base_size = 28, base_line_size = 0.1, base_rect_size = 0.1) +
# theme(legend.position = c(1, 0),
# legend.justification = c(1, 0),
# axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
# axis.title.y = element_text(face = 'bold')
# ) +
# labs(x = NULL, y = 'Feature important value rank')


# ggsave(filename = str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
# str_c(selection_M, OMICS, PHENOOO, sep = '_'), '.pdf'),
# plot = plot, width = 16, height = 9)


## -------------- get the top 20 var cor pvalue with pheno

get_model_top_20_var <- function(MODELLLL) {
DAT_plot %>% arrange(Model, -IMP) %>%
filter(Model == MODELLLL) %>% head(20)
}

Top_20_model_var <-
map_dfr(MODELSS, get_model_top_20_var)


# ----------------------- plot the venn about Bin 3 best model's Var ----------------------------------------

venn_plot <-
  ggvenn(list(
              PLS = Top_20_model_var %>% filter(Model == "pls") %$% Var,
              RF = Top_20_model_var %>% filter(Model == "rf") %$% Var,
               GaussprRadial = Top_20_model_var %>% filter(Model == "gaussprRadial") %$% Var
               ),
         c("PLS", "RF", "GaussprRadial"),
         fill_color = unname(Model_color_venn),
         set_name_color = Model_color_venn,
         show_percentage = F,
         digits = 0,
         stroke_size = 3,
         set_name_size = 14,
         text_size = 10
  )

ggsave(filename = str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
str_c(selection_M, OMICS, PHENOOO, sep = '_'), '_top20_Var_Venn.pdf'),
plot = venn_plot, width = 8, height = 9)


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


get_each_model_sig_pvalue_and_IMP_rank <- function(MODELSSSSS) {
VAR_top_all <-
DAT_plot %>% filter(Model == MODELSSSSS) %$% Var

VAR_top_all_df <-
Ttest_result %>% filter(Agro == Pheno_ref [PHENOOO]) %>%
select(VAR_top_all) %>% t %>% as.data.frame() %>%
mutate(Var = rownames(.))

VAR_top_all_df %>% left_join(DAT_plot %>%
filter(Model == MODELSSSSS)) %>%
dplyr::rename(pvalue = V1)

}

plot_df <-
map_dfr(MODELSS, get_each_model_sig_pvalue_and_IMP_rank) %>%
mutate(Model = factor(Model, levels = MODELSS))

if(OMICS == "metabolome")
{
openxlsx::write.xlsx(
plot_df,
str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
str_c(selection_M, OMICS, PHENOOO, sep = '_'),
'_topall_Var_pvalue_and_IMPrank.xlsx'),
rowNames = F, colNames = T
)
} else {
write.table(
plot_df,
str_c(Save_Path, str_c(MODELSS, collapse = '_'), '__',
str_c(selection_M, OMICS, PHENOOO, sep = '_'),
'_topall_Var_pvalue_and_IMPrank.csv'),
row.names = F, col.names = T, sep = ',', quote = F
)
}



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

# -------------- plot each Model top 20 var barplot

Var_IMP <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_combns_best_train_results_VAR_IMP.txt',
header = T
)

plot_df <-
Var_IMP %>% filter(PHENO == 'Yield', omcis == 'metabolome', Select_M == 'RF') %>%
mutate(Color = 'black') %>%
mutate(Color = if_else(Var %in% (SNP %$% Var), "#d25c58", 'black'))

plot_df <- plot_df %>%
mutate(Var = str_replace_all(Var, 'reproductive', 'R')) %>%
mutate(Var = str_replace_all(Var, 'seeding', 'S'))

Plot_IMP_rank_barplot <- function(data, MM, topN) {
  Color <- switch(MM, "pls" = "#6fcdcf", "rf" = "#e09592", "gaussprRadial" = "#b38ccd")
  Title <- switch(MM, "pls" = "PLS", "rf" = "RF", "gaussprRadial" = "GaussprRadial")
  ggplot(
  data %>%
     filter(Model == MM) %>%
    arrange(IMP) %>% tail(topN) %>%
    mutate(Var = factor(Var, levels = Var)),
          aes(x = Var, y = IMP)) +
  geom_bar(fill = Color,
   stat = "identity",
   position = position_dodge(0.72), width = 0.4,
    size = 2, color = "NA") +
    scale_y_continuous(expand = c(0,0)) +
  labs(title = Title, x = "Features") +
  theme(
    panel.border = element_rect(color = "#696969",
                                 fill = NA, size = 1),
    axis.text.x = element_text(size = 20),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, color =
  ( data %>%
     filter(Model == MM) %>%
    arrange(IMP) %>% tail(topN)  %>%
    mutate(Var = factor(Var, levels = rev(Var))) %>%
         dplyr::select(Color) %>% unlist())
        ),
    axis.ticks.y = element_line(size = 2),
    axis.ticks.x = element_line(size = 2),
    axis.title.x = element_blank(),
    # axis.title.y = element_text(size = 24, face = "bold"),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 24, face = "bold"),
    title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    legend.key.size = unit(1.2, "cm"),
    legend.position = c(0.3, 0.99),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    plot.margin = margin(r = 20, l = 10, t = 10)
  ) + coord_flip()
}

p <-
ggarrange(
Plot_IMP_rank_barplot(plot_df, "rf", 20) +
theme(axis.text.y = element_text(size = 12)),
Plot_IMP_rank_barplot(plot_df, "pls", 20) +
theme(axis.text.y = element_text(size = 12),
axis.text.x = element_text(size = 16, color = "black")),
Plot_IMP_rank_barplot(plot_df, "gaussprRadial", 20) +
theme(axis.text.y = element_text(size = 12)),
nrow = 1)

p <-
annotate_figure(
p,
bottom = text_grob("Feature importance value",
color = "#000000", face = "bold", size = 26)
)

Save_Path_plot <-
'~/machine_learning_project/train_again_V2/plot/'

ggsave(str_c(Save_Path_plot, "3_Model_metabolome_Yield_top20_IMP.pdf"),
plot = p, device = "pdf", width = 16, height = 9)


# -------------- output the overlap 13 snps nearby 5 mb snp gene by RF Genome
library(tidyverse)
library(magrittr)

overlap_top20_result <-
openxlsx::read.xlsx('~/machine_learning_project/train_again_V2/plot/pls_rf_gaussprRadial__RF_metabolome_Yield_topall_Var_pvalue_and_IMPrank.xlsx',
rowNames = F, colNames = T)

SNP <-
overlap_top20_result %>%
arrange(Model, -IMP) %>%
group_by(Model) %>%
summarise(Var = head(Var, 20)) %>%
ungroup() %>% group_by(Var) %>%
summarise(counts = n()) %>%
filter(counts == 1) %>% select(Var)

write.table(
SNP, '~/machine_learning_project/train_again_V2/plot/RF_genome_Yield_overlap_13_snp.csv',
row.names = F, col.names = F, sep = ',', quote = F
)

# -------------- t-test of non-sig snp with all agro trait

SNP <-
overlap_top20_result %>%
arrange(Model, -IMP) %>%
group_by(Model) %>%
summarise(Var = head(Var, 20), pvalue = head(pvalue, 20)) %>%
filter(pvalue >= 0.05) %$% Var

Ttest_result <-
read.table(str_c('~/machine_learning_project/T_test_result/',
'Bin_Agro_T_test_result.txt'),
header = T, sep = '\t', check.names = F)

SNP_Agro <-
Ttest_result %>% select(SNP, Agro) %>% mutate(Type = 'Agro') %>%
rename(Trait = Agro)

Ttest_result <-
read.table(str_c('~/machine_learning_project/T_test_result/bin_Meta_t_test_result.txt'),
header = T, sep = '\t', check.names = F)

SNP_Meta <-
Ttest_result %>% select(SNP, Meta) %>% mutate(Type = 'Meta') %>%
rename(Trait = Meta)

Ttest_result <-
read.table(str_c('~/machine_learning_project/T_test_result/bin_Pheno_t_test_result.txt'),
header = T, sep = '\t', check.names = F)

SNP_Pheno <-
Ttest_result %>% select(SNP, Pheno) %>% mutate(Type = 'Pheno') %>%
rename(Trait = Pheno)

final_non_sig_snp <-
rbind(SNP_Agro,
SNP_Meta,
SNP_Pheno
)

openxlsx::write.xlsx(
final_non_sig_snp,
'~/machine_learning_project/train_again_V2/plot/RF_Genome_Yield_noSig_snp_ttest_with_Agro_Meta_Pheno.xlsx',
rowNames = F, colNames = T
)
