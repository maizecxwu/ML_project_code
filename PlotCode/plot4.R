
source("~/machine_learning_project/train_again/plot/plot_color_match.R")
source("~/R_public_function/R_theme_bar_box.R")
source('~/R_public_function/useful_function.R')
# -------------- save path
Save_Path <- '~/machine_learning_project/train_again_V2/plot/'

combination_result <-
read.table(
'~/machine_learning_project/train_again_V2/plot/All_Multi_omics_combns_best_train_results.txt',
header = T, sep = '\t'
)


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
Model_order_all [c(3, 9, 12)],
c('RF'),
combination_result$omcis %>% table %>% names,
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

write.table(
All_combns_best_train_results_IMP,
str_c(Save_Path, 'All_muti_omics_combination_Var_IMP.txt'),
row.names = F, col.names = T, sep = '\t', quote = F
)

# -------------- plot each Model top 20 var barplot

Var_IMP <-
read.table(
str_c(Save_Path, 'All_muti_omics_combination_Var_IMP.txt'),
header = T, sep = '\t'
)

Var_IMP2 <-
read.table(
str_c(Save_Path, 'All_combns_best_train_results_VAR_IMP.txt'),
header = T, sep = '\t'
)

for(NN in 1:3)
{

TOPNN = 30

MODEL_PLOT <-
c('rf', 'pls', 'gaussprRadial') [NN]

plot_df <-
rbind(
Var_IMP %>% filter(PHENO == 'Yield', str_detect(omcis, 'Genome'),
Select_M == 'RF', Model == MODEL_PLOT),
Var_IMP2 %>% filter(PHENO == 'Yield', str_detect(omcis, 'Genome'),
Select_M == 'RF', Model == MODEL_PLOT)
) %>%
mutate(Color = if_else(Var %in% Genome_Var, Five_type_text_color [1],
if_else(Var %in% META_Var, Five_type_text_color [2], Five_type_text_color[4])
))

plot_df2 <-
plot_df %>%
arrange(omcis, IMP) %>%
mutate(
Var_Type = if_else(
Var %in% Genome_Var, 'Genome',
if_else(Var %in% META_Var, 'Metabolome', 'Phenome')
)
) %>% group_by(
omcis
) %>% summarise(RANK = rank(IMP),
Var_Type = Var_Type,
Var = Var
) %>% mutate(
omcis = factor(omcis,
levels = c('Genome', 'Genome_metabolome', 'Phenome_Genome', 
'Phenome_Genome_metabolome')),
Var_Type = factor(
Var_Type, levels = c('Genome', 'Metabolome', 'Phenome')
)
)

plot_df <-
plot_df %>% mutate(
Var = str_replace_all(Var, '_seeding', '_S')
) %>%
mutate(
Var = str_replace_all(Var, '_reproductive', '_R')
)

# -------------- plot sankey plot of top 20 Var rank in 4 combination

TOP20_VAR <-
plot_df %>% group_by(Var) %>%
summarise(count = n()) %>% filter(count == 4) %$% Var

plot_df3 <- plot_df2 %>% filter(Var %in% TOP20_VAR)

p <-
ggplot(data = plot_df2) +
geom_point(aes(x = omcis, y = RANK, fill = Var_Type),
shape = 22, size = 3, color = 'white') +
geom_line(
data = plot_df3,
aes(x = omcis, y = RANK, group = Var), alpha = 0.5, inherit.aes = F) +
theme_bw(base_size = 34, base_line_size = 0.1, base_rect_size = 0.1) +
theme(axis.text.x = element_text(angle = 15, color = 'black',
vjust = 1, hjust = 1, face = 'bold'),
axis.title = element_text(color = 'black', face = 'bold'),
legend.position = c(0, 1),
legend.justification = c(0, 1),
legend.background = element_blank(),
legend.box.background = element_blank()
) + labs(x = NULL, y = 'Feature Important Value Rank') +
scale_fill_manual(
  values =
      c('Genome' = Five_type_color [c(1)],
'Metabolome' = Five_type_color [c(2)],
'Phenome' = Five_type_color [c(4)]
),
    name = NULL
) + guides(fill = guide_legend(nrow = 1, override.aes = list(size = 8)))
#  scale_color_manual(
#     values =
#        c('Genome' = Five_type_color [c(1)],
# 'Metabolome' = Five_type_color [c(2)],
# 'Phenome' = Five_type_color [c(4)]
# ),
#       name = NULL
#   ) 

Save_Path_plot <-
'~/machine_learning_project/train_again_V2/plot/'

ggsave(str_c(Save_Path_plot, MODEL_PLOT,
"_Model_4_omics_combn_Yield_All_Var_IMP_flow.pdf"),
plot = p, device = "pdf", width = 16, height = 7)


Plot_IMP_rank_barplot <- function(data, MM, topN, OMICS) {
  Color <- switch(MM, "pls" = "#6fcdcf", "rf" = "#e09592",
  "gaussprRadial" = "#b38ccd")
  Title <- OMICS
  ggplot(
  data %>%
     filter(omcis == OMICS) %>%
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
  data %>%
     filter(omcis == OMICS) %>%
    arrange(IMP) %>% tail(topN) %>%
    mutate(Var = factor(Var, levels = rev(Var))) %$% Color),
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

library(patchwork)

p <-
(Plot_IMP_rank_barplot(plot_df, MODEL_PLOT, TOPNN, 'Genome') +
theme(axis.text.y = element_text(size = 12))) +
(Plot_IMP_rank_barplot(plot_df, MODEL_PLOT, TOPNN, 'Genome_metabolome') +
theme(axis.text.y = element_text(size = 12),
axis.text.x = element_text(size = 16, color = "black"))) +
(Plot_IMP_rank_barplot(plot_df, MODEL_PLOT, TOPNN, 'Phenome_Genome') +
theme(axis.text.y = element_text(size = 12))) + 
(Plot_IMP_rank_barplot(plot_df, MODEL_PLOT, TOPNN, 'Phenome_Genome_metabolome') +
theme(axis.text.y = element_text(size = 12))) +
plot_layout(byrow = T, nrow = 2)


# p <-
# annotate_figure(
# p,
# bottom = text_grob("Feature importance value",
# color = "#000000", face = "bold", size = 26)
# )

Save_Path_plot <-
'~/machine_learning_project/train_again_V2/plot/'

ggsave(str_c(Save_Path_plot, MODEL_PLOT,
"_Model_4_omics_combn_Yield_top", TOPNN, "_IMP.pdf"),
plot = p, device = "pdf", width = 13, height = 14)


# -------------- plot combination top20 Var percentage

plot_df <-
rbind(
Var_IMP %>% filter(PHENO == 'Yield',
Select_M == 'RF', Model == MODEL_PLOT)
) %>%
mutate(omics_type = if_else(
Var %in% Genome_Var, 'Genome',
if_else(Var %in% PHENO_Var, 'Phenome', 'Metabolome')
))

plot_df <-
plot_df %>% mutate(
Var = str_replace_all(Var, '_seeding', '_S')
) %>%
mutate(
Var = str_replace_all(Var, '_reproductive', '_R')
)

get_each_combn_topN_Var <- function(TOPN, OMICSCS) {
plot_df %>% filter(omcis == OMICSCS) %>% arrange(IMP) %>%
tail(TOPN)
}

plot_df_2 <-
map_dfr(
unique(plot_df$omcis),
get_each_combn_topN_Var, TOPN = TOPNN
)

plot_df_3 <-
plot_df_2 %>% arrange(omcis, IMP) %>%
group_by(omcis, omics_type) %>%
summarise(counts = n()) %>%
mutate(omics_type = factor(omics_type,
levels = c('Genome', 'Metabolome', 'Phenome'))) %>%
mutate(omcis = str_replace_all(omcis, '_', ' + ')) %>%
mutate(omcis = 
factor(omcis, levels = c('metabolome + Phenome',
'Phenome + Genome', 'Genome + metabolome',
'Phenome + Genome + metabolome'
))
)

plot <-
ggplot(data = plot_df_3) +
geom_bar(aes(x = omcis, y = counts, fill = omics_type),
color = 'black', stat = 'identity', position = 'fill',
size = 1, width = 0.6) +
scale_fill_manual( name = NULL,
values = c('Genome' = Five_type_color [c(1)],
'Metabolome' = Five_type_color [c(2)],
'Phenome' = Five_type_color [c(4)]
)
) +
theme_classic(base_size = 28, base_line_size = 0.1, base_rect_size = 0.1) +
theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1,
face = 'bold', color = 'black'), axis.title = element_text(face = 'bold'),
legend.text = element_text(face = 'bold')) +
labs(x = NULL, y = 'Percentage') +
theme(legend.position = 'top',
legend.key.size = unit(1, 'cm')
) +
guides(fill = guide_legend(nrow = 2))

ggsave(plot = plot,
str_c(Save_Path, MODEL_PLOT, '_4_combn_select_var_percentage_top', TOPNN, '.pdf'),
height = 10, width = 6)

}

