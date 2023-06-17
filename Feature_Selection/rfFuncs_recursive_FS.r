# feature selection with Backwards Selection
# 数据预处理----

args <- commandArgs(TRUE)

if(args[1] == "--help")
{
  cat("
      params have 3;
      first: FS Methods (RF or Linear)
      Second: FS data (Phenome, metabolome, Genome, Early_metabolome)
      Third: Fold(1-3).Rep(01~33)
  ")
} else {

impuated_X <-
if(args[2] == 'Phenome')
{
   read.table("~/machine_learning_project/train_again/imputed_phenomics_data_sacled.txt",
  header = T, sep = "\t")
} else if (args[2] == 'metabolome') {
  read.table("~/machine_learning_project/train_again/imputed_Meta_data_sacled.txt",
  header = T, sep = "\t")
} else if (args[2] == 'Genome'){
  read.table("~/machine_learning_project/train_again/imputed_Bin_data_sacled.txt",
  header = T, sep = "\t")
} else {
   read.table("~/machine_learning_project/train_again/imputed_Meta_data_sacled.txt",
  header = T, sep = "\t")[, 1:69]
}


source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")
source("~/R_public_function/useful_function.R")
ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", rowNames = F, colNames = T)
load("~/machine_learning_project/train_again/REP_AND_FOLD_SET.rda")
Y <- ourdata[, ncol(ourdata)]
Y <- Y[-which(is.na(Y))]
Y <- Y[-excluded_observed]


xxx <- FOLD_AND_REP_var [[args[3]]]
# train set
Train_set_X <- impuated_X [xxx, ]
Train_set_Y <- Y[xxx]
# vaild set
Valid_set_X <- impuated_X [-xxx, ]
Valid_set_Y <- Y [-xxx]

# -------------- Choose a feature selection method
if(args[1] == "RF")
{
filterCtrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  repeats = 33, returnResamp = "all", number = 3,
  saveDetails = F
)

} else {
 filterCtrl <- rfeControl(
  functions = lmFuncs,
  method = "repeatedcv",
  repeats = 33, returnResamp = "all", number = 3,
  saveDetails = F
)
}

Sizes = case_when(args[2] == 'Phenome' ~ 736,
args[2] == 'metabolome' ~ 136,
args[2] == 'Genome' ~ 2492,
args[2] == 'Early_metabolome' ~ 69
)

# speed up the process by parallel
library(parallel)
library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)
options(java.parameters = "-Xmx100g")

# cannot choose the nums of vars customizely
# assign(str_c('rfWithFilter_', args[3]),
rfWithFilter <-
  rfe(Train_set_X, Train_set_Y, sizes = Sizes, rfeControl = filterCtrl)
  # )

if(!dir.exists(str_c("~/machine_learning_project/train_again/",
args[1], "_", args[2], "_FS_result/"))) {
  dir.create(str_c("~/machine_learning_project/train_again/",
args[1], "_", args[2], "_FS_result/"))
}

save(
  # str_c('rfWithFilter_', args[3]),
  rfWithFilter,
file = str_c("~/machine_learning_project/train_again/",
args[1], "_", args[2], "_FS_result/FS_",
args[1], "_", args[2], "_", args[3], ".rda"))

# print(rfWithFilter$bestSubset)

# write.table(rfWithFilter$results,
# str_c("~/machine_learning_project/train_again/RF_method_FS_result", args[6], ".txt"),
# row.names = F, col.names = T, sep = "\t", quote = F)

stopCluster(cl)


}





# # -------------- test manually

# load("~/machine_learning_project/train_again/test.rda")

# Select_by_mean_IMP <-
# rfWithFilter$variables %>% filter(Variables == 736) %>% group_by(var) %>%
# summarise(IMP_Mean = mean(Overall, na.rm = T)) %>%
# arrange(-IMP_Mean) %$% var %>% head(656)

# Select_by_src2 <- pickVars(rfWithFilter$variables,656)

# identical(Select_by_src2, Select_by_mean_IMP)

# intersect(Select_by_mean_IMP, Select_by_src)

# detach(plyr)

# detach("package:plyr", character.only = TRUE)


# quick_write_table(
# get_all_combination(
#   list(
# str_c("Fold", rep(1:3,33), ".Rep", rep(str_pad(1:33, width = 2, side = 'left', pad = "0"), each = 3)),
# c("RF", "Linear"),
# args)
# ), "~/machine_learning_project/train_again/All_need_FS_sets.txt"
# )

# for(i in 1:700)
# {
#   print(str_c("Start: num ", i))
#  if(identical(
# rfWithFilter$variables %>% filter(Variables == 736, Resample == "Fold1.Rep01") %>%
# head(i) %$% var,
# rfWithFilter$variables %>% filter(Variables == i, Resample == "Fold1.Rep01")%$% var))
# {
#   print("YES")
# }
# }
