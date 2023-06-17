
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




#########################################
#########################################
#########################################
# ------- Set the color corresponding to morning and evening ---------

Time_color <- c("#7ecfc9", "#dda779")

# ------ Set the color scheme of the three main models ----------
Model_color_barplot <- c(rf = "#f6c9c7", bartMachine = "#b9e3e4", gaussprRadial = "#dbcbe6")

Model_color_venn <- c(rf = "#e09592", bartMachine = "#6fcdcf", gaussprRadial = "#b38ccd")

Five_type_color <- c("#b2c7e6", "#ffba80", "#ffc2be", "#ade88d", "#a1e4d7")

Five_type_text_color <- c("#397de0", "#ef852c", "#f23728", "#6cd136", "#3de3c3")

Five_type_color_boxplot <- c("#7a9ac8", "#df985d", "#de908a", "#7bc355", "#65c7b4")

Five_type_text_color_stacked_barplot <- c("#96b7e8", "#ffaf6b", "#ffaca7", "#9eea75", "#77e7d1")

# -----Plot all models for the period and importance selection differences of the variables −

Var_Time_IMP_type <-
read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
                            header = T, check.names = F, sep = "\t")

Var_Time_IMP_type <- Var_Time_IMP_type %>% dplyr::rename(IMP = Scale_Value) %>%
                      mutate(Time =
                      if_else(Var %in% Early_Name,
                      true = "Early", false = "Late"))

# See if the vars of different models are biased towards morning and evening
# Here, the variable importance of all models is normalized for easy comparison
for(i in Var_Time_IMP_type$model)
{
  Var_Time_IMP_type[which(Var_Time_IMP_type$model == i),]$IMP <-
  (Var_Time_IMP_type[which(Var_Time_IMP_type$model == i),]$IMP -
  min(Var_Time_IMP_type[which(Var_Time_IMP_type$model == i),]$IMP)) /
  max(Var_Time_IMP_type[which(Var_Time_IMP_type$model == i),]$IMP)
}

# define the theme
source("~/R_public_function/R_theme_bar_box.R")

Model_order <-
c(
  "bartMachine", "earth", "enet",
  "lasso", "mlp", "neuralnet",
  "pls", "rf", "gaussprRadial")

X_axis_labels <-
c("BART", "MARS", "EN", "LASSO", "MLP", "ANN", "PLS", "RF", "GaussprRadial")

# ------ start ploting -----
# --- Plot the IMP's data
plot_IMP_df <-
Var_Time_IMP_type %>% filter(model != "leapseq") %>%
        mutate(model = factor(model, levels = Model_order),
        Time = factor(Time, levels = c("Early", "Late"))) %>%
      rename(value = IMP)

# --- plot count data
plot_count_df <-
Var_Time_IMP_type %>% filter(model != "leapseq") %>%
        # select(model:Time) %>%
        group_by(model, Time) %>% summarise(value =n()) %>%
        mutate(model = factor(model, levels = Model_order),
        Time = factor(Time, levels = c("Early", "Late")))

# ---- plot function -----
plot4_funcs <- function(data, Y_title, Geom_type, Legend_name) {
  if (Geom_type == "boxplot")
  {
    Scale_ <-
    scale_color_manual(values = Time_color,
         guide = guide_legend(nrow = 2),
         name = NULL, labels = Legend_name)
  } else {
    Scale_ <-
    scale_fill_manual(values = Time_color,
         guide = guide_legend(nrow = 2),
         name = NULL, labels = Legend_name)
  }
    ggplot(data, aes(x = model, y = value)) +
         Scale_ + labs(y = Y_title)
}

# ----- custom section -------
# IMP
p_IMP <- plot4_funcs(plot_IMP_df, "Feature importance value", "boxplot", c("Early i-trait", "Late i-trait")) +
geom_boxplot(
        aes(color = Time), fill = NA,
         size = 2, width = 0.5,
         position = position_dodge(0.7),
    outlier.shape = NA
  )  +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_discrete(labels = X_axis_labels) + Theme_bar_box


# counts

p_Count <- plot4_funcs(plot_count_df, "Feature count", "barplot", c("Early i-trait", "Late i-trait")) +
geom_bar(aes(fill = Time), color = NA,
    stat = "identity", position = position_dodge(0.72),
    width = 0.6,
    size = 2) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_discrete(labels = X_axis_labels) + Theme_bar_box
  
# ------- loop through these images
for(i in c("IMP", "Count"))
{
  ggsave(str_c("~/machine_learning_project/Plot_result/pdf/",
  "Diff_models_Var_", i, "_in_Time_rm_leapseq.pdf"), plot = get(str_c("p_", i)),
  device = "pdf", width = 16, height = 9)
}




#########################################
#########################################
#########################################
# ------------------------------ plot the best model result ----------------------------------------------------

Best_model_rf <- read.table("~/machine_learning_project/Plot_result/Best_model_rf.txt", header = T, check.names = F, sep = "\t")
Best_model_lm <- read.table("~/machine_learning_project/Plot_result/Best_model_lm.txt", header = T, check.names = F, sep = "\t")

j <- read.table("~/machine_learning_project/Plot_result/only_Meta_Best_result.txt",
                header = T, check.names = F, sep = "\t")

#########################################
#########################################
#########################################
# plot prediction results under linear and nonlinear characteristics

Model_order_all <-
c("rrBLUP", "leapseq", "pls", "lasso", "enet",
  "earth", "bartMachine", "svmLinear",
  "gaussprRadial", "gbm", "lightgbm", "rf",
  "mlp", "neuralnet")

Best_model <- rbind(Best_model_rf %>% mutate(Feature = "non-Linear"),
                    Best_model_lm %>% mutate(Feature = "Linear"))
Best_model[which(Best_model$Model == "rrBULP"), ]$Model <- "rrBLUP"

Best_model$Feature <- factor(Best_model$Feature, levels = c("non-Linear", "Linear"))
Best_model$Model <- factor(Best_model$Model, levels = Model_order_all)

X_axis_labels_all <-
c("rrBLUP", "SR", "PLS", "LASSO", "EN", "MARS", "BART",
"SVR", "GaussprRadial", "GBM", "LightGBM", "RF", "MLP", "ANN")

# Sort by predicted mean
# Convert the model name to correspond
Model_Name <- data.frame(original = Model_order_all, now = X_axis_labels_all)

New_order <-
Best_model %>% group_by(Model, Feature) %>%
 summarise(Mean_R = mean(R, na.rm = T)) %>%
 filter(Feature == "non-Linear") %>%
 arrange(Mean_R) %$%
 list(as.character(Model), Model_Name$now[match(as.character(Model), Model_Name$original)])

Best_model$Model <- factor(Best_model$Model, levels = New_order[[1]])

# plot
main_plot <-
  ggplot(Best_model %>% filter(R > 0), aes(x = Model, y = R, color = Feature)) +
  stat_boxplot(
    geom = "errorbar", width = 0.1, size = 2,
    coef = 4, position = position_dodge(0.7)
  ) +
  geom_boxplot(
    coef = 2, lwd = 2, size = 2, width = 0.3,
    alpha = 1, position = position_dodge(0.7)
  ) +
  scale_color_manual(values = Time_color,
   guide = guide_legend(nrow = 2), name = NULL,
   labels = c("non-Linear i-trait", "Linear i-trait")) +
    scale_x_discrete(labels = New_order[[2]]) +
  labs(title = NULL,
       y = "Prediction accuracy") +
  Theme_bar_box +
  theme(axis.text.x =
  element_text(color = c(rep("black", 2), "#9a9999", rep("black", 5),
  "#9a9999", rep("black", 2), rep("#d25050", 3))))

ggsave("~/machine_learning_project/Plot_result/Diff_models_and_features_cor_result.pdf",
plot = main_plot, device = "pdf", width = 16, height = 9)










#########################################
#########################################
#########################################
# ----------- plot the prediction result based on metabolome-----------------
ALL <-
read.table("~/machine_learning_project/Plot_result/only_Meta_Best_result.txt",
header = T, check.names = F, sep = "\t")

Model_order_all[c(2, 11)] <- c("leapSeq", "lightGBM")

Model_Name <- data.frame(original = Model_order_all, now = X_axis_labels_all)

New_order <-
ALL %>% group_by(model) %>%
 summarise(Mean_R = mean(R, na.rm = T)) %>%
 arrange(Mean_R) %$%
 list(as.character(model), Model_Name$now[match(as.character(model), Model_Name$original)])

main_plot <-
  ggplot(ALL %>% mutate(model =
  factor(model, levels = New_order[[1]])) %>%
            filter(R > 0),
            aes(x = model, y = R)) +
  stat_boxplot( color = "#6e6e6e", fill = NA,
    geom = "errorbar", width = 0.2, size = 2,
    coef = 4, position = position_dodge(0.7)
  ) +
  geom_boxplot( color = "#6e6e6e", fill = "white",
    coef = 1, lwd = 2, size = 2, width = 0.3,
    alpha = 1, position = position_dodge(0.7)
  ) +
    scale_x_discrete(labels = New_order[[2]]) +
  labs(title = NULL, y = "Prediction accuracy") +
   Theme_bar_box +
  theme(axis.text.x =
    element_text(color = c(rep("black", 6), "#9a9999", rep("black", 1),
  "#9a9999", rep("black", 1), rep("#d25050", 1), rep("black", 1), rep("#d25050", 2)),
    angle = 30))

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/Diff_models_cor_result_Meta.pdf",
plot = main_plot, device = "pdf", width = 16, height = 9)


# ---------------- get the 3 model about Meta var IMP diff ---------------------
# --------- feature IMP rank plot ------------------
rf_FS_best_IMP <-
read.table("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/Early_Meta_IMP_rf_bartMachine_gaussprRadial.txt",
            header = T, check.names = F, sep = "\t") %>% 
            mutate(Color = if_else(str_detect(Var, "seeding"), true = "#35978f", false = "#bf812d"),
                  Time = if_else(str_detect(Var, "seeding"), true = "Early", false = "Late")
                  # Var = gsub("(_seeding)|(_reproductive)", "", Var)
                  )

df_p <-
rf_FS_best_IMP %>% mutate(Var = gsub("seeding", "S", Var)) %>%
mutate(Var = gsub("reproductive", "R", Var))



# ------- IMP rank plot function  ---------------
Plot_IMP_rank_barplot <- function(data, MM) {
  Color <- switch(MM, "bartMachine" = "#6fcdcf", "rf" = "#e09592", "gaussprRadial" = "#b38ccd")
  Title <- switch(MM, "bartMachine" = "BART", "rf" = "RF", "gaussprRadial" = "GaussprRadial")
  ggplot(
  data %>%
     filter(model == MM) %>%
    arrange(Scale_Value) %>%
    mutate(Var = factor(Var, levels = Var)),
          aes(x = Var, y = Scale_Value)) +
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
  rf_FS_best_IMP %>%
     filter(model == MM) %>%
    arrange(Scale_Value)  %>%
    mutate(Var = factor(Var, levels = rev(Var))) %>%
         select(Color) %>% unlist()
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
ggarrange(Plot_IMP_rank_barplot(df_p, "rf"),
Plot_IMP_rank_barplot(df_p, "bartMachine") +
theme(axis.text.x = element_text(size = 18)),
Plot_IMP_rank_barplot(df_p, "gaussprRadial") +
theme(axis.text.x = element_text(size = 18)
),
nrow = 1)
p <-
annotate_figure(
p,
bottom = text_grob("Feature importance value",
color = "#000000", face = "bold", size = 26)
)

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/3_models_top_20_vars_IMP_2_Meta.pdf",
plot = p, device = "pdf", width = 17, height = 9)




# ---------------------------------- get the 3 model about Meta  var diff Time ------------------------------------

# plot
plot_count_df <-
rf_FS_best_IMP %>%
filter(model != "leapSeq") %>%
        select(model,Time) %>%
        group_by(model, Time) %>% summarise(value =n()) %>% 
        mutate(model = factor(model, levels = Model_order),
        Time = factor(Time, levels = c("Early", "Late")))

# 
p_Count <-
plot4_funcs(plot_count_df, "Count", "barplot", c("Early metabolites", "Late metabolites")) +
geom_bar(aes(fill = Time), color = NA,
    stat = "identity", position = position_dodge(0.72),
    width = 0.6,
    size = 2) +
  scale_y_continuous(expand = c(0.01, 0.01)) +
  scale_x_discrete(labels = X_axis_labels) + Theme_bar_box +
  theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 1))
  

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/only_Meta_3_models_Var_Counts_in_Time.pdf",
plot = p_Count, device = "pdf", width = 16, height = 9)




# ----------------------- plot the venn about Meta 3 best model's Var ----------------------------------------

venn_plot <-
  ggvenn(list(
          RF = rf_FS_best_IMP %>% filter(model == "rf") %>% select(Var) %>% unlist(),
          BART = rf_FS_best_IMP %>% filter(model == "bartMachine") %>% select(Var) %>% unlist(),
          GaussprRadial = rf_FS_best_IMP %>% filter(model == "gaussprRadial") %>% select(Var) %>% unlist()
            ),
      c("RF", "BART", "GaussprRadial"),
      fill_color = unname(Model_color_venn),
      set_name_color = Model_color_venn,
      show_percentage = F,
      digits = 0,
      stroke_size = 3,
      set_name_size = 14,
      text_size = 10
  )

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/Venn_Meta_3_model_Var_overlap.pdf",
plot = venn_plot, device = "pdf", width = 8, height = 9)








#########################################
#########################################
#########################################
# ----------- plot the prediction result based genome data -------------------
ALL <- read.table("~/machine_learning_project/Plot_result/only_Bin_Best_result.txt",
header = T, check.names = F, sep = "\t")

Model_order_all[c(2, 11)] <- c("leapSeq", "lightGBM")

Model_Name <- data.frame(original = Model_order_all, now = X_axis_labels_all)

New_order <-
ALL %>% group_by(model) %>%
 summarise(Mean_R = mean(R, na.rm = T)) %>%
 arrange(Mean_R) %$%
 list(as.character(model), Model_Name$now[match(as.character(model), Model_Name$original)])

main_plot <-
  ggplot(ALL %>% mutate(model =
  factor(model, levels = New_order[[1]])) %>%
            filter(R > 0),
            aes(x = model, y = R)) +
  stat_boxplot(color = "#6e6e6e", fill = NA,
    geom = "errorbar", width = 0.2, size = 2,
    coef = 4, position = position_dodge(0.7)
  ) +
  geom_boxplot( color = "#6e6e6e", fill = "white",
    coef = 1, lwd = 2, size = 2, width = 0.3,
    alpha = 1, position = position_dodge(0.7)
  ) +
    scale_x_discrete(labels = New_order[[2]]) +
  labs(title = NULL,
       y = "Prediction accuracy") +
  Theme_bar_box +
  theme(axis.text.x =
  element_text(color = c(rep("black", 3), "#9a9999", rep("black", 5),
  "#9a9999", rep("black", 1), rep("#d25050", 3))))

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/Diff_models_cor_result_Bin_results.pdf",
plot = main_plot, device = "pdf", width = 16, height = 9)

#########################################
#########################################
#########################################
# ---------------------------- get the 3 model about Bin var IMP diff -------------------------------------------

rf_FS_best_IMP <- read.table("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/only_Bin_10model_IMP.txt",
                header = T, check.names = F, sep = "\t") %>% 
                mutate(Color = if_else(str_detect(Var, "seeding"), true = "#35978f", false = "#bf812d"),
                      Time = if_else(str_detect(Var, "seeding"), true = "Early", false = "Late")
                      # Var = gsub("(_seeding)|(_reproductive)", "", Var)
                      )

  
df_p <-
rf_FS_best_IMP %>% mutate(Var = gsub("seeding", "S", Var)) %>%
mutate(Var = gsub("reproductive", "R", Var))


p <-
ggarrange(Plot_IMP_rank_barplot(df_p, "rf") +
theme(axis.text.y = element_text(color = "black")),
Plot_IMP_rank_barplot(df_p, "bartMachine") +
theme(axis.text.y = element_text(size = 8, color = "black"),
axis.text.x = element_text(size = 16, color = "black")),
Plot_IMP_rank_barplot(df_p, "gaussprRadial") +
theme(axis.text.y = element_text(color = "black")),
nrow = 1)
p <-
annotate_figure(
p,
bottom = text_grob("Feature importance value",
color = "#000000", face = "bold", size = 26)
)

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/only_Bin_3_models_top_20_vars_IMP_2.pdf",
plot = p, device = "pdf", width = 16, height = 9)



# ----------------------- plot the venn about Bin 3 best model's Var ----------------------------------------

venn_plot <-
  ggvenn(list(
              RF = rf_FS_best_IMP %>% filter(model == "rf") %$% Var,
              BART = rf_FS_best_IMP %>% filter(model == "bartMachine") %$% Var,
               GaussprRadial = rf_FS_best_IMP %>% filter(model == "gaussprRadial") %$% Var
               ),
         c("RF", "BART", "GaussprRadial"),
         fill_color = unname(Model_color_venn),
         set_name_color = Model_color_venn,
         show_percentage = F,
         digits = 0,
         stroke_size = 3,
         set_name_size = 14,
         text_size = 10
  )

ggsave("~/machine_learning_project/Plot_result/Meta_and_bin_single_omics/Venn_Bin_3_model_Var_overlap.pdf",
plot = venn_plot, device = "pdf",  width = 8, height = 9)
































#########################################
#########################################
#########################################
# ---------------------  get the same var in linear and non-linear features --------------------------

lm_feature <- read.table("~/machine_learning_project/feature_selection_recursive/lmFuncs_rm_0_variables.txt",
                         header = T, sep = "\t")

rf_feature <- read.table("~/machine_learning_project/feature_selection_recursive/rfFuncs_rm_0_variables.txt",
                         header = T, sep = "\t")

# treebag_feature <- read.table("~/machine_learning_project/feature_selection_recursive/treebagFuncs_rm_0_variables.txt",
#                               header = T, sep = "\t")

varNum <- 100

Name_lm <-
  lm_feature %>%
  filter(Variables == varNum) %>%
  select(var) %>%
  table() %>%
  sort() %>%
  rev() %>%
  .[1:varNum] %>%
  names()

Name_rf <-
  rf_feature %>%
  filter(Variables == varNum) %>%
  select(var) %>%
  table() %>%
  sort() %>%
  rev() %>%
  .[1:varNum] %>%
  names()

# ------- venn plot the top 100 features overlap 
venn_plot <-
  ggvenn(list(`non-Linear` = Name_rf,
              Linear = Name_lm),
         c("non-Linear", "Linear"),
         fill_color = Time_color,
         set_name_color = Time_color,
         digits = 0,
         stroke_size = 3,
         set_name_size = 14,
         text_size = 10
  ) + theme(plot.margin = margin(r = 5, l = 5, t = 5, b = 5)) +
  labs(x = "ll")

ggsave("~/machine_learning_project/Plot_result/Diff_models_and_features_venn.pdf",
plot = venn_plot, device = "pdf",  width = 8, height = 9)




#########################################
#########################################
#########################################
# plot the linear and non-linear features cor denstity

Name_lm_cor <- cor(impuated_X[, Name_lm], use = "pairwise.complete.obs")
Name_rf_cor <- cor(impuated_X[, Name_rf], use = "pairwise.complete.obs")

Name_lm_cor[!upper.tri(Name_lm_cor, diag = T)] <- 0
Name_rf_cor[!upper.tri(Name_rf_cor, diag = T)] <- 0

Name_lm_cor <- Name_lm_cor [-which(as.numeric(Name_lm_cor) == 0 )]
Name_rf_cor <- Name_rf_cor [-which(as.numeric(Name_rf_cor) == 0 )]

df <- data.frame(cor = c(abs(Name_lm_cor), abs(Name_rf_cor)),
                 Type = c(rep("Linear", 5050), rep("non-Linear", 5050)))


setwd("~/machine_learning_project/Plot_result/")

pdf("Diff_models_and_features_denstity_plot.pdf", width = 9, height = 9)

p_cor_plot <-
  ggplot(df %>% filter(cor < 1)) +
  geom_density(size = 1.5, aes(x = cor, colour = Type), show.legend = FALSE) +
  stat_density(aes(x = cor, colour = Type), geom = "line", position = "identity", size = 2) +
  scale_x_continuous(
    expand = expansion(mult = .02)
  ) +
  scale_y_continuous(expand = expansion(mult = .04)) +
  scale_color_manual(values = c("#AC88FF", "#FE7280" ), name = "Feature") +
  theme(
    panel.border = element_rect(size = 4, fill = NA, color = "black"),
    legend.position = c(0.82, 0.92),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 22, face = "bold"),
    legend.key.size = unit(0.4, "inches"), #
    legend.text = element_text(size = 22, face = "bold"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(
      color = NA, # 框线色
      fill = NA
    ),
    axis.text.x = element_text(size = 22, color = "black", 
                               hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title.x = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 24, face = "bold"),
    axis.ticks.x = element_line(size = 2),
    axis.ticks.y = element_line(size = 2),
    plot.margin = margin(l = 10, r = 20,t = 10)
  ) +
  labs(
    x = "Featrues correlation coefficient r"
    # expression(italic(R^2))
    , y = "Denstity"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = T))

ggsave("~/machine_learning_project/Plot_result/Diff_models_and_features_denstity_plot.pdf",
plot = p_cor_plot, device = "pdf",  width = 9, height = 9)

# --------------------- heatmap to show the linear and non- top 100 var's self-correlation ------------------------
library(pheatmap)

pdf("~/machine_learning_project/Plot_result/lm_top100_var_self_correlation.pdf", width = 8, height = 8)

Name_lm_cor <- abs(Name_lm_cor)
Name_rf_cor <- abs(Name_rf_cor)

pheatmap(Name_lm_cor, cluster_row = T, cluster_col = T, 
            show_rownames = F, border = FALSE,
             show_colnames = F,
            color = colorRampPalette(c("#543005",    "#8c510a",    "#bf812d",
                    "#dfc27d",    "#f6e8c3",    "#f5f5f5",
                "#c7eae5",   "#80cdc1",    "#35978f",
                "#01665e",    "#003c30"))(100),
                  cellwidth = 4, cellheight = 4, fontsize = 20,
                  treeheight_row = 0, treeheight_col = 0
                )

dev.off()












# -------------------------  Plot a boxplot of combined predictions -------------------------------------

ALL <- read.table("~/machine_learning_project/Plot_result/All_Pheno_3_Type_Early_joint_prediction_best_result.txt", 
                header = T, check.names = F, sep = "\t") %>% 
                group_by(varNum, model) %>% summarise(Mean = mean(R))

p <-
ggplot(ALL %>%
        mutate(varNum = factor(varNum,
        levels = c("All Pheno", "Early Pheno", "Early Pheno + Bin",
        "Early Pheno + Early Meta", "Early Pheno + Bin + Early Meta")),
        model = factor(model, levels =
        c("rf", "bartMachine", "gaussprRadial"))),
aes(x = model, y = R)) +
      geom_boxplot(
        aes(color = varNum), fill = NA,
         size = 2, width = 0.5,
         position = position_dodge(0.7),
    outlier.shape = NA
  ) +
  scale_x_discrete(labels = c("RF", "BART", "GaussprRadial")) +
  scale_color_manual(values = Five_type_color_boxplot,
                    guide = guide_legend(nrow = 2),
                    name = NULL,
                    labels = c("All i-traits", "Early i-traits",
                    "Early i-traits + SNPs", "Early i-traits + Early metabolites",
                    "Early i-traits + SNPs + Early metabolites")) +
  labs(title = NULL,
       y = "Prediction accuracy") +
  Theme_bar_box +
  theme(legend.position = c(-0.03, 1.2),
  axis.text.x = element_text(angle = 15),
  legend.text = element_text(size = 21)) +
  theme(plot.margin = margin(t = 90))

ggsave("~/machine_learning_project/Plot_result/Early_Pheno_fixed_and_joint_pred/3_models_All_Early_Pheno_add_Early_Meta_add_Bin_R.pdf",
plot = p, device = "pdf",  width = 16, height = 9)





#########################################
#########################################
#########################################
# -------------------------- get diff Time var to predict the yield --------------------------------------------
library(dplyr)

ALL <- data.frame()

# # Best Resulting Model for Combination Prediction
# rf_3_Meta_1_10_results_rf.txt
# gaussprRadial_1_Meta_1_10_results_rf.txt
# bartMachine_1_Meta_1_10_results_rf.txt


jj <- read.table(
  file = paste0(
    "~/machine_learning_project/Diff_Time_var_pred_test/result/bartMachine_110_130_results_rf.txt"
  ),
  header = T, check.names = F, sep = "\t"
)

jj %>% filter(varNum == "Late") %>%
  # filter(varNum == "Early") %>% 
  group_by(num_trees, varNum) %>%
  # group_by(sigma, varNum) %>%
  # group_by(num_trees, varNum) %>%
  summarise(R_Mean = mean(R, na.rm = T)) %>% 
  # as.data.frame() %>%
  # group_by( varNum, mtry) %>%
  # summarise(R_Max = max(R_Mean, na.rm = T)) %>%
  as.data.frame() %>%
  filter(R_Mean == max(R_Mean))

ALL <- rbind(ALL,
read.table(
  file = paste0(
    "~/machine_learning_project/Diff_Time_var_pred_test/result/bartMachine_110_130_resample_rf.txt"
  ),
  header = T, check.names = F, sep = "\t"
) %>% filter(num_trees == 122, varNum == "Late") %>% 
  select(R, Resample, varNum) %>%
 mutate(model = "bartMachine")
)

ALL <- rbind(ALL %>% select(-Resample),
read.table("~/machine_learning_project/Plot_result/All_Pheno_3_Type_Early_joint_prediction_best_result.txt",
            header = T, check.names = F, sep = "\t") %>% filter(varNum == "All Pheno")
    )



# write.table(ALL, "~/machine_learning_project/Plot_result/3_model_Diff_Time_var_predict_R.txt", 
#                   row.names = F, col.names = T, sep = "\t", quote = F)

# ----------- Plot phenotype data for different periods Predict yield results ----------------
ALL <- read.table("~/machine_learning_project/Plot_result/3_model_Diff_Time_var_predict_R.txt", 
                  header = T, check.names = F, sep = "\t")

p <-
ggplot(ALL %>%
        mutate(varNum = factor(varNum,
        levels = c("All Pheno", "Early", "Late")),
        model = factor(model, levels = c("rf", "bartMachine", "gaussprRadial"))), 
aes(x = model, y = R)) +
      geom_boxplot(
        aes(color = varNum), fill = NA,
         size = 2, width = 0.5,
         position = position_dodge(0.7),
    outlier.shape = NA
  ) +
  scale_x_discrete(labels = c("RF", "BART", "GaussprRadial")) +
  scale_color_manual(values = Five_type_color_boxplot[1:3],
                    name = NULL,
                    labels = c("All i-traits", "Early i-traits", "Late i-traits")) +
  labs(title = NULL,
       y = "Correlation coefficient r") +
  Theme_bar_box +
  theme(legend.position = c(0, 0),
  legend.justification = c(0, 0),
  axis.text.x = element_text(angle = 15))


ggsave("~/machine_learning_project/Plot_result/3_models_All_Early_Late_R.pdf",
plot = p, device = "pdf", width = 12, height = 9)












#########################################
#########################################
#########################################
# ---------------------------- phenome 3 best model rank barplot --------------------------------
# ---------------------------- what kind of vars  the 3 model choose -------------------------------------------------

# get the 5 type features and fill color

PMT <- colnames(impuated_X)[
  c(
    sapply(c(
      "PW", "MPH", "TBR", "PAR", "PP", "PC", "FDIC", "FDNIC",
      "NPH"
    ), function(x) {
      grep(x, colnames(impuated_X))
    }),
    c(433:448)
  )
]

LAT <- c(
  colnames(impuated_X)[
    sapply(
      c(
        "TLL", "SDSLL",
        "SDLNL", "SDLC", "SDLTA", "SDLSA", "SLL_below",
        "LNL_below", "LC_below", "LSA_below", "SLL_above",
        "LNL_above", "LC_above", "LSA_above"
      ),
      function(x) {
        grep(x, colnames(impuated_X))
      }
    )
  ],
  c(
    colnames(impuated_X)[177:192], colnames(impuated_X)[33:48],
    colnames(impuated_X)[1:16], colnames(impuated_X)[17:32],
    colnames(impuated_X)[65:80], colnames(impuated_X)[49:64]
  ),

  colnames(impuated_X)[
    sapply(
      c("LTA_below", "LTA_above"),
      function(x) {
        grep(x, colnames(impuated_X))
      }
    )
  ]
)

BRT <- colnames(impuated_X)[
  c(c(497:512), c(145:160), c(81:96),
    sapply(
      c("AGR", "RGR"),
      function(x) {
        grep(x, colnames(impuated_X))
      }
    ))
]

HTT <-  colnames(impuated_X)[
        grep("TEX", colnames(impuated_X))
  ]

CT <- colnames(impuated_X)[
        grep("GCV", colnames(impuated_X))
  ]

 Type_5_color <-
 data.frame(Feature = c(PMT, LAT, BRT, HTT, CT),
            Color = rep(Five_type_text_color[c(5, 4, 1, 3, 2)],
c(length(PMT), length(LAT), length(BRT), length(HTT), length(CT))))

# get the 5 type features
# 10 22 7 1

# ------------ rank barplot plot------------------------------
rf_FS_best_IMP <- read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
                              header = T, check.names = F, sep = "\t")

rf_FS_best_IMP <-
left_join(rf_FS_best_IMP, Type_5_color,
    by = c("Var" = "Feature"))

df_p <-
rf_FS_best_IMP

p <-
ggarrange(Plot_IMP_rank_barplot(df_p, "rf"),
Plot_IMP_rank_barplot(df_p, "bartMachine") +
theme(axis.text.x = element_text(size = 14),
axis.text.y = element_text(size = 10)),
Plot_IMP_rank_barplot(df_p, "gaussprRadial")+
theme(axis.text.x = element_text(size = 14)),
nrow = 1)
p <-
annotate_figure(
p,
bottom = text_grob("Feature importance value",
color = "#000000", face = "bold", size = 26)
)

ggsave("~/machine_learning_project/Plot_result/pdf/3_models_top_20_vars_IMP_2.pdf",
plot = p, device = "pdf", width = 16, height = 9)


# ----------- plot the venn --------------------
venn_plot <-
  ggvenn(list(
              RF = rf_FS_best_IMP %>% filter(model == "rf") %>% select(Var) %>% unlist(),
              BART = rf_FS_best_IMP %>% filter(model == "bartMachine") %>% select(Var) %>% unlist(),
              GaussprRadial = rf_FS_best_IMP %>% filter(model == "gaussprRadial") %>% select(Var) %>% unlist()
               ),
         c("RF", "BART", "GaussprRadial"),
         fill_color = unname(Model_color_venn),
         set_name_color = Model_color_venn,
                              show_percentage = F,
         digits = 0,
         stroke_size = 3,
         set_name_size = 14,
         text_size = 10
  )

ggsave("~/machine_learning_project/Plot_result/pdf/3_model_vars_intersect_venn.pdf",
plot = venn_plot, device = "pdf", width = 8, height = 9)

# rf_FS_best_IMP %>% filter(model == "rf") %>% filter(!(Var %in% 

# Reduce(intersect, list(c(rf_FS_best_IMP %>% filter(model == "bartMachine") %>% select(Var) %>% unlist()),

# c(rf_FS_best_IMP %>% filter(model == "rf") %>% select(Var) %>% unlist()),

# c(rf_FS_best_IMP %>% filter(model == "gaussprRadial") %>% select(Var) %>% unlist()))) ))

# # 
# Var Scale_Value model
# 1 LNL_above_T4   0.6858500    rf
# 2 LC_below_T14   0.6352308    rf

# # --------------------------------
# rf_FS_best_IMP %>% filter(model == "bartMachine") %>% filter(!(Var %in% 

# Reduce(intersect, list(c(rf_FS_best_IMP %>% filter(model == "bartMachine") %>% select(Var) %>% unlist()),

# c(rf_FS_best_IMP %>% filter(model == "rf") %>% select(Var) %>% unlist()),

# c(rf_FS_best_IMP %>% filter(model == "gaussprRadial") %>% select(Var) %>% unlist()))) ))

#           Var Scale_Value       model
# 1    SDLNL_T11 0.008027534 bartMachine
# 2 LC_below_T14 0.003272403 bartMachine
# 3 LNL_above_T4 0.008884214 bartMachine
# 4       NPH_T8 0.012631043 bartMachine
# 5        SL_T8 0.004917615 bartMachine
# 6      LNL_T11 0.007060510 bartMachine
# 7        SL_T7 0.010523274 bartMachine
# 8        SL_T1 0.004838117 bartMachine
# 9       SL_T16 0.008735875 bartMachine

# --Statistical differences in the selection ratios of the five phenotype groups by different models -----
# get 3 model the 5 type nums

Mid <-
  left_join(rf_FS_best_IMP,
  data.frame(
    color = Five_type_text_color,
    Type = c("PMT", "LAT", "BRT", "HTT", "CT")
  ),
  by = c("Color" = "color")
  ) %>%
  filter(model %in% c("bartMachine", "gaussprRadial", "rf")) %>%
  select(model, Type) %>%
  group_by(model, Type) %>%
  summarise(Counts = n())

Mid$Per <- Mid$Counts / rep(c(7 * 16, 1 * 16, 22 * 16, 10 * 16), 3)


# ----------------the differences in the Phenomics variable types selected by different models------
p <-
rbind(Mid, data.frame(model = "observed",
    Type = c("PMT", "LAT", "BRT", "HTT", "CT"),
    Counts = c(10, 22, 7, 6, 1)
  )) %>% mutate(
    model = factor(model,
      levels = c("observed", "rf", "bartMachine", "gaussprRadial")
    ),
    variable = factor(Type, levels = c("PMT", "LAT", "BRT", "CT", "HTT"))
  ) %>% as.data.frame %>%
  mutate(Percentage = Counts/c(rep(48,4), rep(39, 4), rep(41, 4), rep(46, 5))) %>%

  ggplot(aes(x = model, y = Percentage)) +
  geom_bar(aes(fill = Type),
    stat = "identity",
    color = "black",
    width = 0.6,
    size = 2,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = Five_type_text_color_stacked_barplot,
    name = NULL
  ) +
  labs(
    title = NULL, y = "Percentage"
  ) +
  scale_x_discrete(labels = c("Observed", "RF", "BART", "GaussprRadial")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
  Theme_bar_box +
  theme(axis.text.x = element_text(angle = 15),
  legend.position = c(0.83, 1.14),
  legend.justification = c(0.83, 1.14),
  plot.margin = margin(t = 70),
  legend.text = element_text(margin = margin(r = 10)))

ggsave("~/machine_learning_project/Plot_result/pdf/3model_var_4type_nums_stacked_plot.pdf",
plot = p, device = "pdf", width = 8.7, height = 10)































#########################################
#########################################
#########################################
# -------------------------- to know what the bin is about  -----------------

  Mid <- read.table("~/machine_learning_project/Early_Phenomics_Bin_Meta/result/rf_3_Meta_1_10_resample_rf.txt",
  header = T, check.names = F, sep = "\t")

#  Mid <- read.table("~/machine_learning_project/Meta_analysis/FS_result/rrblup_result.txt", header = T, 
#                     check.names = F, sep = "\t")
  
  head(Mid)

  Mid %>%
  # filter(varNum == "Early") %>%
  group_by(mtry, varNum) %>%
  # group_by(sigma, varNum) %>%
  # group_by(num_trees, varNum) %>%
  summarise(R_Mean = mean(R, na.rm = T)) %>%
  # as.data.frame() %>%
  # group_by( varNum, mtry) %>%
  # summarise(R_Max = max(R_Mean, na.rm = T)) %>%
  as.data.frame() %>%
  filter(R_Mean == max(R_Mean)) %>%
  select(num_trees) %>%
  unlist()

lm_var <- read.table("~/machine_learning_project/Meta_analysis/Early_Meta_rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)

varNum = 8

NameofVar <-
    lm_var %>%
    filter(Variables == varNum) %>%
    select(var) %>%
    table() %>%
    sort() %>%
    rev() %>%
    .[1:varNum] %>%
    names()

Bin_inf <- openxlsx::read.xlsx("~/machine_learning_project/bin_inf_predict_yield/Bin_inf.xlsx",
                             rows = 3:2499, rowNames = F, colNames = T)

QTL_inf <- openxlsx::read.xlsx("~/machine_learning_project/PP2016-01516R2_Supplemental_Data2.xlsx",
                             rows = 4:991, rowNames = F, colNames = F)

apply(
Bin_inf %>% filter(bin %in% NameofVar) %>% select(Chromosome, bin, `genetic.position.(cM)`), 
1, function(x){
  QTL_inf %>% filter(X6 == x[1], (X7 == x[2] | (X12 <= x[3] & X13 >= x[3])))
}) 


#------------------------ IMP about the Early Phenomcis and Bins ----------------------
# -- The importance information of each variable of the multi-omics combination predicting the outcome
IMP_LAST <- list()

IMP_LAST[[7]] <-  IMP_Overall %>% mutate(model = "rf", Type = "Bin Early Meta")

# write.table(do.call(rbind.data.frame, IMP_LAST),
#             "~/machine_learning_project/Plot_result/Early_Phenomics_Early_Meta_Bin_and_Meta_Bin_IMP.txt",
#             row.names = F, col.names = T, sep = "\t", quote = F)





#########################################
#########################################
#########################################
# what kind of vars  the 3 model choose
# ------------ rank barplot ------------------------------
rf_FS_best_IMP <-
read.table("~/machine_learning_project/Plot_result/Early_Phenomics_Early_Meta_Bin_and_Meta_Bin_IMP.txt",
            header = T, check.names = F, sep = "\t")

rf_FS_best_IMP <-
left_join(rf_FS_best_IMP, Type_5_color,
    by = c("Var" = "Feature"))

rf_FS_best_IMP %>% filter(model == "gaussprRadial")

df_p <-
rf_FS_best_IMP

# --------------------- IMP barplot --------------------------

IMP <- read.table("~/machine_learning_project/Plot_result/Early_Phenomics_Early_Meta_Bin_and_Meta_Bin_IMP.txt",
                  header = T, check.names = F, sep = "\t")
IMP$Var <- gsub('`',"", IMP$Var)

Bin <- openxlsx::read.xlsx("~/machine_learning_project/bin_inf_predict_yield/Bin_inf.xlsx",
                             rowNames = F, colNames = T, rows = 3:2499) %>% select(bin) %>% unlist()

Meta <- read.table("~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
             header = T, check.names = F, sep = "\t") %>% colnames() %>% .[3:71]

# ---------------- labels color -------------------------
rf_FS_best_IMP <-
IMP %>% filter(Type == "Bin Early Meta") %>%
 mutate(Color = if_else(Var %in% Meta, true = Five_type_text_color[2],
 if_else(Var %in% Bin ,true = Five_type_text_color[1],
 false = Five_type_text_color[4])))

# ---------- plot data --------------
IMP_data <-
IMP %>% filter(Type == "Bin Early Meta") %>%
mutate(Color = if_else(Var %in% Meta, true = "#FF9900",
if_else(Var %in% Bin ,true = "#CC3399", false = "#99CC33")))

# Avoid long metabolite names ---
IMP_data$Var <- gsub("seeding", "S", IMP_data$Var)

p <-
ggarrange(Plot_IMP_rank_barplot(IMP_data, "rf"),
Plot_IMP_rank_barplot(IMP_data, "bartMachine") +
theme(axis.text.x = element_text(size = 20),
axis.text.y = element_text(size = 13)),
Plot_IMP_rank_barplot(IMP_data, "gaussprRadial")+
theme(axis.text.x = element_text(size = 20)),
nrow = 1)
p <-
annotate_figure(
p,
bottom = text_grob("Feature importance value",
color = "#000000", face = "bold", size = 26)
)

ggsave("~/machine_learning_project/Plot_result/Early_Pheno_fixed_and_joint_pred/3_models_All_Early_Pheno_add_Early_Meta_add_Bin_IMP_results.pdf",
plot = p, device = "pdf", width = 18, height = 11)




#########################################
#########################################
#########################################
#---------------- Differences in variable omics types selected by different models

plot_data <-
IMP %>% filter(Type == "Bin Early Meta") %>%
 mutate(Type2 = if_else(Var %in% Meta, true = "Meta",
 if_else(Var %in% Bin ,true = "Bin", false = "Pheno"))) %>%
 group_by(model, Type2) %>% summarise(Count = n()) %>%
  filter(Type2 != "Pheno") %>%
 group_by(model) %>% summarise(Per = Count/sum(Count), Type = Type2) %>%
 rbind(
 data.frame(model = "observed",
    Per = c(0.9731099, 0.0268901),
    Type = c("Bin", "Meta")
  ) ) %>%
  mutate(model = factor(model, levels = c("observed", "rf", "bartMachine", "gaussprRadial")),
         Type = factor(Type, levels = c("Bin", "Meta"))) 

p <-
  ggplot(plot_data, aes(x = model, y = Per)) +
  geom_bar(aes(fill = Type),
    stat = "identity", 
    color = "black",
    width = 0.6,
    size = 2,
    alpha = 0.9
  ) +
  scale_fill_manual(
    values = Five_type_color[1:2],
    name = NULL,
    labels = c("SNP", "Metabolites")
  ) +
  labs(
    title = NULL,
    y = "Percentage"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.01,0.01)) +
  scale_x_discrete(labels = c("Observed", "RF", "BART", "GaussprRadial")) +
  Theme_bar_box +
  theme(axis.text.x = element_text(angle = 15),
  legend.position = c(0.4, 1.1),
  legend.justification = c(0.4, 1.1),
  plot.margin = margin(t = 70),
  legend.text = element_text(margin = margin(r = 60)))

ggsave("~/machine_learning_project/Plot_result/Early_Pheno_fixed_and_joint_pred/3model_var_2Type_nums_stacked_plot.pdf",
plot = p, device = "pdf", width = 7, height = 12)
















#########################################
#########################################
#########################################
# ------------ Explain why Early Meta cannot compensate prediction precision owing to late phenotypic deletions
# ------------ Plot Density Curve Function
Plot_density_func <- function(data, x_text) {
  ggplot(data) +
  geom_density(size = 1.5, aes(x = P, colour = Type), show.legend = FALSE) +
  stat_density(aes(x = P, colour = Type), geom = "line", position = "identity", size = 2) +
  scale_x_continuous(
    expand = expansion(mult = .02)
  ) +
  scale_y_continuous(expand = expansion(mult = .04)) +
  scale_color_manual(values = Time_color, name = "Feature") +
  
  theme(
    panel.border = element_rect(size = 0.5, fill = NA, color = "black"),
    legend.position = c(1, 0),
    legend.justification = c(1, 0),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "inches"),
    legend.text = element_text(size = 26, face = "bold"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(
      color = NA, 
      fill = NA
    ),
    axis.text.x = element_text(size = 26, color = "black", 
                               hjust = 0.5, vjust = 1),
    axis.text.y = element_text(size = 26, color = "black"),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.ticks.x = element_line(size = 2),
    axis.ticks.y = element_line(size = 2),
    plot.margin = margin(l = 10, r = 20,t = 10)
  ) +
  labs(
    x = x_text
    # expression(italic(R^2))
    , y = "Denstity"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = T))
}

# -------------- t-test function for data 
t_test_df <- function(data) {
  colnames(data) <- c("P", "var")
  data <- data %>% group_by(var) %>% summarise(P = list(P))
  t.test(data$P[[1]], data$P[[2]])$p.value
}

# --------- read data -----------------
Cor_Meta_Pheno <- read.table("~/machine_learning_project/T_test_result/Pheno_Meta_cor_p_value_result.txt", 
                              header = T, check.names = F, sep = "\t")

IMP_Pheno <-
read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
            header = T, check.names = F, sep = '\t')

# ----Correlation analysis in the late traits corresponding to the best parameters of the 3 models, and all results are compared
Traget_var <-
  IMP %>% filter(Type == "Early Meta") %>%
 mutate(Type2 = if_else(Var %in% Meta, true = "Meta",
 if_else(Var %in% Bin ,true = "Bin", false = "Pheno"))) %>% 
 filter(Type2 == "Meta") %$% Var %>% unique

observed <-
Cor_Meta_Pheno  %>% filter(Meta %in% Traget_var) %>%
select(Late_Name) %>% unlist %>% as.numeric

# ------- plot data 
plot_data <-
rbind(
  data.frame(P = observed, Type = "Late"),
  data.frame(P = Cor_Meta_Pheno %>% filter(Meta %in% Traget_var) %>%
  select(Early_Name) %>% unlist %>% as.numeric, Type = "Early")
) %>% mutate(Type = factor(Type, levels = c("Early", "Late")))


p <-
Plot_density_func(plot_data, "P-value between i-traits and metabolites") +
annotate("text", size = 12, x = 0.5, y = 0.6, fontface = "bold", label = str_c("P = ", round(t_test_df(plot_data), 4)))

ggsave("~/machine_learning_project/Plot_result/Early_Pheno_fixed_and_joint_pred/Meta/target_Meta_all_late_pheno_back_is_target_Meta_all_Early_pheno_-log10.pdf",
plot = p, device = "pdf", width = 10, height = 8)




#########################################
#########################################
#########################################
# ----------------- Bin's ability to compensate for late phenotypic missing precision-
# --------- read t-test data -----------------
Cor_Meta_Pheno <- read.table("~/machine_learning_project/T_test_result/bin_Pheno_t_test_result.txt", 
                              header = T, check.names = F, sep = "\t")

IMP_Pheno <-
read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
            header = T, check.names = F, sep = '\t')

# ------Correlation analysis in late traits corresponding to the best parameters of the 3 models, and all results are compared
Traget_var <-
  IMP %>% filter(Type == "Bin") %>%
 mutate(Type2 = if_else(Var %in% Meta, true = "Meta",
 if_else(Var %in% Bin ,true = "Bin", false = "Pheno"))) %>% 
 filter(Type2 == "Bin") %$% Var %>% unique


observed <-
Cor_Meta_Pheno %>% filter(Pheno %in% Late_Name) %>%
select(Traget_var) %>% unlist %>% as.numeric

# ------- plot data 
plot_data <-
rbind(
  data.frame(P = observed, Type = "Late"),
  data.frame(P = Cor_Meta_Pheno %>% filter(Pheno %in% Early_Name) %>%
  select(Traget_var) %>% unlist %>% as.numeric, Type = "Early")
) %>% mutate(Type = factor(Type, levels = c("Early", "Late")))

p <-
Plot_density_func(plot_data, "P-value between i-traits and genome")+
annotate("text", size = 12, x = 0.5, y = 0.6, fontface = "bold", label = str_c("P = ", signif(t_test_df(plot_data), 2)))

ggsave("~/machine_learning_project/Plot_result/Early_Pheno_fixed_and_joint_pred/Bin/target_Bin_all_late_pheno_back_target_bin_all_Early_pheno.pdf",
plot = p, device = "pdf", width = 10, height = 8)



# SIg <-
# j %>% gather(key = "Bin", value = "P", -Pheno) %>% filter(P < 0.05)

# j <- read.table("~/machine_learning_project/T_test_result/bin_Pheno_t_test_result.txt",
#                 header = T, check.names = F, sep = "\t")

# openxlsx::write.xlsx(SIg, "~/machine_learning_project/T_test_result/Sig_Pheno_Bin.xlsx",
#                         row.names = F)









#########################################
#########################################
#########################################
# --------- a case for a detailed explanation ------------------
# --------- a case for a detailed explanation ------------------
# --------- a case for a detailed explanation ------------------
All_useful_omics_var <-
IMP %>% filter(Type == "Bin Early Meta") %>%
 mutate(Type2 = if_else(Var %in% Meta, true = "Meta",
 if_else(Var %in% Bin ,true = "Bin", false = "Pheno")))

Phenomic_var <-
All_useful_omics_var %>% filter(Type2 == "Pheno") %$% unique(Var)
geomic_var <-
All_useful_omics_var %>% filter(Type2 == "Bin") %$% unique(Var)
meta_var <-
All_useful_omics_var %>% filter(Type2 == "Meta") %$% unique(Var)


Cor_Meta_Pheno %>% filter(Pheno %in% Phenomic_var) %>%
select(geomic_var, "Pheno") %>%
gather(key = "Bin", value = "P", -Pheno) %>% arrange(P) %>% head(30)


Cor_Meta_Pheno %>% filter(Meta %in% meta_var) %>%
select(Phenomic_var, "Meta") %>%
gather(key = "Pheno", value = "P", -Meta) %>% arrange(P) %>% head(30)

j <- read.table("~/MAGMA/Maize_All_known_gene_Loc_basedon_Zea_maysAGPv436chr.txt", header = F, sep = "\t")

# write.table(
# j %>% filter(V2 == 4, V3 >= 4413748, V4 <= 6413748) %>% select(V1),
# "~/machine_learning_project/Plot_result/SYN24044_SNP_1Mb_gene.txt",
# row.names = F, col.names = T, sep = "\t", quote = F)

# Zm00001d049196 (smk4 - small kernel4) Located near SYN24044
# Zm00001d049201 (ts5 - tassel seed5) Located near SYN24044
# View the V4 genomic location where the locus is located
Bin_V4_pos <- read.table("~/machine_learning_project/bb_map.v4.manual",
header = F, sep = "\t")

Bin_V4_pos %>% filter(V1 == 4, V2 >= 20000000, V2 <= 21000000)
# SYN4902
# SYN1483

# The SNP is closest to the smk gene,
# and the SNP is significantly associated with LTA_above in the early target i-trait
LTA_above_T2 SYN4902 7.731067e-04

Cor_Meta_Pheno[, c("SYN4902", "SYN1483", "Pheno")] %>%
gather(key = "SNP", value = "pvalue", -Pheno) %>% arrange(pvalue) %>% head(30)

# -----pull all the bins with significant differences in the target i-trait
# -----Then get the gene information under these bins to see if there are yield-related genes
# First extract these i-traits corresponding to significantly correlated bins
Sig_cor_bin <-
Cor_Meta_Pheno %>% filter(Pheno %in% Phenomic_var) %>%
gather(key = "SNP", value = "pvalue", -Pheno) %>% arrange(pvalue) %>%
filter(pvalue <= 0.0001)

# Then get the V4 genome location information of these bins
Sig_cor_bin_V4_pos <-
Sig_cor_bin %>% left_join(Bin %>% dplyr::select(Chromosome, bin, `genetic.position.(cM)`),
by = c("SNP" = "bin")) %>%
left_join(Bin_V4_pos, by = c("Chromosome" = "V1", "genetic.position.(cM)" = "V3"))

# Extract the left and right positions of each bin, and appropriately expand this area to get more genes
Bin_pos <-
Sig_cor_bin_V4_pos %>% group_by(SNP, Chromosome, `genetic.position.(cM)`) %>% 
summarise(start = min(V2), end = max(V2), size = max(V2) - min(V2))

# Expand 500kb left and right to find
Bin_pos <-
Bin_pos %>% mutate(start = if_else(start - 500000 < 0, 0, start - 500000),
end = end + 500000)

# ------ a function for extracting target segment genes from annotated genes --------
get_the_target_interval_gene_funcs <- function(Genome_anno_data, interval) {
  Genome_anno_data %>% filter(V2 == interval[1], V3 <= interval[3], V4 >= interval[2]) %$%
  V1
}
# Start getting the gene names within these enlarged bin regions
sig_bin_C_gene <-
Bin_pos %>% mutate(C_gene = list(get_the_target_interval_gene_funcs(j, c(Chromosome, start, end)))) %>%
unnest(C_gene)

# write.table(sig_bin_C_gene, "~/machine_learning_project/Plot_result/target_i-trait_sig_related_bin_C_gene.txt",
# row.names = F, col.names = T, sep = "\t", quote = F)

# write.table(sig_bin_C_gene %>% ungroup %>% select(C_gene) %>% distinct(),
# "~/machine_learning_project/Plot_result/target_i-trait_sig_related_bin_C_gene_id_column.txt",
# row.names = F, col.names = T, sep = "\t", quote = F)


# ---- read the gene inf under the bins --------------
bin_gene_inf <- openxlsx::read.xlsx("~/machine_learning_project/Plot_result/target_i-trait_sig_related_bin_C_gene_id_column_gene_description.xlsx",
sheet = 2, rowNames = F, colNames = T)
# Get the genes associated with the grain
yield_gene <- rbind(
bin_gene_inf %>% filter(str_detect(Description, "kernel")),
bin_gene_inf %>% filter(str_detect(Description, "yield")))

sig_bin_C_gene <- read.table("~/machine_learning_project/Plot_result/target_i-trait_sig_related_bin_C_gene.txt",
header = T, check.names = F, sep = "\t")

# Record the SNP-related phenotypes corresponding to these yield-related candidate genes
# openxlsx::write.xlsx(
# yield_gene %>% left_join(sig_bin_C_gene, by = c("Gene_ID" = "C_gene")) %>%
# left_join(Sig_cor_bin, by = c("SNP" = "SNP")),
# "~/machine_learning_project/Plot_result/yield_related_gene_candidate_i-trait.xlsx",
# rowNames = F)

source("~/R_public_function/t_test_p_value_var_num_violinplot.R")

# ------- correlation point plot function -----
 plot_cor_point_plot <- function(data, Pheno_Name) {
  ggplot(data, aes(x = Pheno, y = yeild)) +
  geom_point(size = 2, shape = 21,
  color = "black", fill = Five_type_text_color_stacked_barplot[1]) +
  geom_smooth(method = "lm", size = 2, color = "#a53838") +
  theme(axis.text = element_text(size = 28),
  axis.title = element_text(size = 30)) +
  labs(x = Pheno_Name, y = "Yield_Blup") +
  annotate("text", x = quantile(plot_df2[ , "Pheno"], 0.5), y = max(plot_df2[ , "yeild"]), size = 10,
  label = str_c("p = ", round(pvalue$p.value, 2), "; r = ", round(pvalue$estimate, 2)))
  }

# Plot SYN24044 versus yield
plot_df <-
Yield_Bin %>% select("SYN24044", Yield_Blup) %>%
filter(SYN24044 != 0) %>%
mutate(SYN24044 = factor(SYN24044, levels = c(2, 1))) 

p1 <- plot_t_test_violin(plot_df, c(1.5, 4.3))


# Analysis of the relationship between yield and GCV phenotype
plot_df2 <-
data.frame(Pheno = X[, "GCV_T7"], yeild = Y)
pvalue <- cor.test(plot_df2$Pheno, plot_df2$yeild)

p2 <- plot_cor_point_plot(plot_df2, "GCV_T7")

# Analyze the SNP and GCV relationship
plot_df3 <- left_join(plot_df, plot_df2, by = c("Yield_Blup" = "yeild"))

p3 <- plot_t_test_violin(plot_df3 %>% select(-Yield_Blup) %>% rename(GCV_T7 = Pheno),
c(160, 210), legend_pos = c(0, 1.07), top_blank = 40,
annoate_y_pos = c(sig_ano = 205, var_num = 160))

# Stitching pictures
library(patchwork)
p <-
p3 + p1 +  p2

ggsave("~/machine_learning_project/Plot_result/case_result.pdf", width = 18,
height = 8, device = "pdf", plot = p)

p <-
p3 + (p1 + theme(legend.position = "none"))
ggsave("~/machine_learning_project/Plot_result/case_result_2.pdf", width = 12,
height = 8, device = "pdf", plot = p)

# t-test analysis of phenotype and yield of SNPs associated with yield
yield_related_snp <- openxlsx::read.xlsx("~/machine_learning_project/Plot_result/yield_related_gene_candidate_i-trait.xlsx",
rowNames = F, colNames = T)

for(SNPP in 1:nrow(yield_related_snp))
{
  SNP_name = yield_related_snp[SNPP, ]$SNP
  Trait_name = yield_related_snp[SNPP, ]$Pheno
  Gene_name = yield_related_snp[SNPP, ]$Gene_Name
  # t-test results for SNP and yield
  plot_df <-
  Yield_Bin %>% select(SNP_name, Yield_Blup) %>%
  set_names(c("SNP", "Yield")) %>%
  filter(SNP != 0, SNP != 3) %>%
  mutate(SNP = factor(SNP, levels = c(2, 1))) %>%
  set_names(c(Gene_name, "Yield_Blup"))

  p1 <- plot_t_test_violin(plot_df, c(1.5, 4.3)) +
  labs(y = "Yield (Tons per hectare)")
  # Cor point results of yield and corresponding phenotype
  plot_df2 <-
  data.frame(Pheno = X[, Trait_name], yeild = Y) %>% na.omit()
  pvalue <- cor.test(plot_df2$Pheno, plot_df2$yeild)

  p2 <- plot_cor_point_plot(plot_df2, Trait_name) +
  labs(y = "Yield (Tons per hectare)")


  # SNP and this phenotype t-test results
  plot_df3 <- left_join(plot_df, plot_df2, by = c("Yield_Blup" = "yeild")) %>% na.omit()
  Y_lab_range <-
  c(range(plot_df3$Pheno)[1] - (range(plot_df3$Pheno)[2] - range(plot_df3$Pheno)[1]) / 20,
  range(plot_df3$Pheno)[2] + (range(plot_df3$Pheno)[2] - range(plot_df3$Pheno)[1]) / 4)

  p3 <- plot_t_test_violin(plot_df3 %>% select(-Yield_Blup) %>% set_names(c(Gene_name, Trait_name)),
  Y_lab_range, legend_pos = c(0, 1.07), top_blank = 40,
  annoate_y_pos = c(sig_ano = (range(plot_df3$Pheno)[2] + Y_lab_range[2]) / 2,
  var_num = Y_lab_range[1]))


p <-
  p3 + (p1 + theme(legend.position = "none"))

  ggsave(str_c("~/machine_learning_project/Plot_result/",
  Trait_name, "_", Gene_name, "_case_result.pdf"), width = 12,
  height = 8, device = "pdf", plot = p)

  ggsave(str_c("~/machine_learning_project/Plot_result/",
  Trait_name, "_yield_case_result.pdf"), width = 7,
  height = 8, device = "pdf", plot = p2 + theme(plot.margin = margin(r = 30, t = 20, l = 10)))

p_last <-
  p3 + (p1 + theme(legend.position = "none")) +  p2 + theme(plot.margin = margin(r = 30, t = 20, l = 10))

  ggsave(str_c("~/machine_learning_project/Plot_result/all_case_t_test_plot/",
  Trait_name, "_", Gene_name, "_yield_case_result.pdf"), width = 18,
  height = 8, device = "pdf", plot = p_last)

}

# # The SNP is closest to the smk gene, and the SNP is significantly associated with LTA_above in the early target i-trait
# LTA_above_T2 SYN4902 7.731067e-04

cor(X[, str_detect(colnames(X), "LTA_above")], use = "pairwise.complete.obs")







#########################################
#########################################
#########################################
# Compare whether the IMP sequence of the original phenotype group has changed
# after adding the omics data to prove the existence of interaction
for(Kind in IMP$Type %>% unique)
{

df_p <-
# -------- Ranking of the original early phenotype group traits
read.table("~/machine_learning_project/feature_tune_parallel/IMP/rf_FS_best_IMP.txt",
           header = T, check.names = F, sep = "\t") %>%
           filter(model %in% c("rf", "bartMachine", "gaussprRadial"),
           Var %in% Early_Name) %>%
              arrange(model, Scale_Value) %>%
              group_by(model) %>%
              summarise(Rank = order(Scale_Value),
              Scale_Value = Scale_Value, Var = Var) %>%
              select(-Scale_Value) %>% as.data.frame %>%
            left_join(
# --------- Ranking of phenotype group traits after adding omics data
IMP %>% filter(Type == Kind,
              Var %in% Early_Name) %>%
              arrange(model, Scale_Value) %>%
              group_by(model) %>%
              summarise(Rank = order(Scale_Value),
              Scale_Value = Scale_Value, Var = Var) %>%
              as.data.frame %>% select(-Scale_Value),
               by = c("model" = "model", "Var" = "Var")
               )


openxlsx::write.xlsx(
    df_p,
    str_c("~/machine_learning_project/Intermediate_data/Plot6_data/", Kind, ".xlsx"),
    row.names = F, overwrite = TRUE)

for(MM in c("rf", "bartMachine", "gaussprRadial"))
{
  
  p_data <-
  df_p %>% filter(model == MM) %>%
  select(-model) %>%
  set_names(c("Original rank", "Var", "Altered rank")) %>%
  gather(key = "Type", value = "IMP_rank", -Var) %>%
  mutate(
  Var = factor(Var,
  levels = c(df_p %>% filter(model == MM) %$% Var)),
  Type = factor(Type, levels = c("Original rank", "Altered rank"))
  ) %>% mutate(Var = as.integer(Var))

  p <-
  p_data %>%
  ggplot(aes(x = Var, y = IMP_rank)) +
  geom_line(aes(color = Type),
    position = position_dodge(0.72),
    size = 2,
    alpha = 0.9) +
  geom_point(aes(fill = Type), color = "grey", stroke = 2,
  shape = 21, size = 6, position = position_dodge(0.72)) +
  scale_color_manual(
    values = Five_type_color[1:2],
    name = NULL,
    guide = guide_legend(nrow = 2, byrow = T)
  ) +
  scale_fill_manual(
    values = Five_type_color[1:2],
    name = NULL,
    guide = guide_legend(nrow = 2, byrow = T)
  ) +
  labs(
    title = NULL,
    y = "Feature importance value",
    x = str_c("Early ", p_data$Var %>% unique %>% length(), " phenomic features ", "(", Model_Name %>% filter(original == MM) %$% now, ")")
  ) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  # facet_wrap(. ~interaction.depth,nrow = 2,labeller = label_both)+
  Theme_bar_box +
  theme(plot.margin = margin(t = 70),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  legend.position = c(0, 1.2),
  axis.title.x = element_text(size = 32, face = "bold"),
  legend.justification = c(0, 1.2),
  legend.text = element_text(margin = margin(r = 200)))

ggsave(str_c("~/machine_learning_project/Plot_result/Early_phenomics_IMP_rank_alter/", Kind, "_", MM, ".pdf"),
plot = p, device = "pdf", width = 15, height = 8)

}
}

# -------------- Manually merge the case for themes
mgs1_p3 <- p3
mgs1_p1 <- p1
zm24813_p3 <- p3
zm24813_p1 <- p1

mgs1 <- p3 + (p1 + theme(legend.position = "none"))

zm24813 <- p3 + (p1 + theme(legend.position = "none"))

corr_plot <- p2 + theme(plot.margin = margin(r = 30, t = 20, l = 10))

last_result <-
(mgs1_p3 + mgs1_p1 + corr_plot + zm24813_p3 + zm24813_p1 + plot_layout(nrow = 1, widths = c(1, 1, 1, 1, 1))) / 
(p2 + p)


ggsave(plot = last_result, device = "pdf", filename = "~/machine_learning_project/Plot_result/main_plot_case.pdf",
width = 32, height = 16)
















#########################################
#########################################
#########################################
# ---------------- export intermediate data -------------- 
# ---------------- export intermediate data -------------- 
# ---------------- export intermediate data -------------- 

jjj <- read.table("~/machine_learning_project/Early_Phenomics_Bin/result/rf_1_10_train_rf.txt",
                header = T, check.names = F, sep = "\t")

identical(j, jjj)

jjj[,1]


j <- read.table("~/machine_learning_project/Intermediate_data/Plot2_data/only_Meta_Best_result.txt",
                header = T, check.names = F, sep = "\t")

openxlsx::write.xlsx(
j %>% group_by(model, varNum) %>%
summarise(Mean = mean(R, na.rm = T), Median = median(R, na.rm = T),
          Max = max(R, na.rm = T), min = min(R, na.rm = T)),
          "~/machine_learning_project/Intermediate_data/Plot2_data/Best_model_Meta_summary.xlsx",
          row.names = F)

lm_var <- data.table::fread("~/machine_learning_project/feature_selection_recursive/rfFuncs_rm_0_variables.txt",
  header = T, check.names = F, sep = "\t"
)

varNum = 100

openxlsx::write.xlsx(
    lm_var %>% as.data.frame() %>%
    filter(Variables == varNum) %>%
    select(var) %>%
    table() %>%
    sort() %>%
    rev() %>% as.data.frame %>%
    rename(Trait = ".") %>% head(varNum),
    "~/machine_learning_project/Intermediate_data/Plot1_data/Best_model_rf_top_100_features_Freq.xlsx",
    row.names = F)

openxlsx::write.xlsx(
  Name_rf_cor %>% as.data.frame %>%  mutate(Trait2 = rownames(.)) %>%
   gather(key = "Trait", value = "cor", -Trait2) %>% filter(cor != 0),
   "~/machine_learning_project/Intermediate_data/Plot1_data/Best_model_rf_top_100_features_self_correlation_r.xlsx",
    row.names = F)

openxlsx::write.xlsx(
 left_join(
  rf_FS_best_IMP , Type_5_color, 
    by = c("Var" = "Feature")) %>%
    left_join(
 data.frame( Color = c("#df7342", "#068bd8", "#3abdbd", "#82cf3a", "#ca3eb8"),
                Type = c("PMT", "LAT", "BRT", "HTT", "CT")),
                by = c("Color" = "Color")
    ) %>% mutate(Time = if_else(Var %in% Early_Name, true = "Early", false = "Late")),
   "~/machine_learning_project/Intermediate_data/Plot3_data/c_plot_all_model_variable_IMP_type_color_time.xlsx",
    row.names = F)


  IMP <- read.table("~/machine_learning_project/Intermediate_data/Plot5_data/Early_Phenomics_Early_Meta_Bin_and_Meta_Bin_IMP.txt",
                     header = T, check.names = F, sep = "\t")
   
 Traget = IMP %>% filter(Type == "Early Meta", Var %in% Meta) %$% Var %>% unique

   j <- openxlsx::read.xlsx("~/machine_learning_project/Intermediate_data/Plot5_data/T_test_result/Sig_Meta_Pheno.xlsx",
                    rowNames = F, colNames = T)
 
  j %>% filter(Meta %in% Traget)

 openxlsx::write.xlsx(
  
IMP %>% filter(Type == "Bin Early Meta") %>%
 mutate(Type2 = if_else(Var %in% Meta, true = "Meta",
 if_else(Var %in% Bin ,true = "Bin", false = "Pheno"))) %>%
 group_by(model, Type2) %>% summarise(Count = n()) %>%
  filter(Type2 != "Pheno") %>%
 group_by(model) %>% summarise(Per = Count/sum(Count), Type = Type2) %>%
 rbind(
 data.frame(model = "observed",
    Per = c(0.9731099, 0.0268901),
    Type = c("Bin", "Meta")
  ) ) %>% mutate(model = factor(model, levels = c("observed", "rf", "bartMachine", "gaussprRadial")),
                  Type = factor(Type, levels = c("Bin", "Meta"))),
   "~/machine_learning_project/Intermediate_data/Plot6_data/b_plot_percentage.xlsx",
    row.names = F)












