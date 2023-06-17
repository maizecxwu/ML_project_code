
args <- commandArgs(TRUE)

if(args[1] == "--help")
{
  cat("
      params have 2;
      first: FS Methods (RF or Linear)
      Second: FS data (Phenome, metabolome, Genome, Early_metabolome)
  ")
} else {
source("~/machine_learning_project/ML_project_code/visualization_and_secondary_analysis_code/metric_evluation_funtion.R")
source("~/R_public_function/useful_function.R")

omics_file <-
c('imputed_phenomics_data_sacled.txt',
'imputed_Meta_data_sacled.txt',
'imputed_Bin_data_sacled.txt',
'imputed_Meta_data_sacled.txt') %>%
set_names(c('Phenome', 'metabolome', 'Genome', 'Early_metabolome'))

change_orig_name <- function(omics, omics_file2 = omics_file) {
    FILE = str_c("~/machine_learning_project/train_again/",  omics_file2[omics])
    change_Name <<-
        read.table(FILE,
        header = T, sep = "\t") %>% colnames()
    orig_Name <<-
        read.table(FILE,
        header = T, sep = "\t", check.names = F) %>% colnames()
}

change_orig_name(args[2])

ALL_combn <-
get_all_combination(list("Fold", 1:3, "Rep",
c(str_pad(1:33, width = 2, side = 'left', pad = '0')))) %$%
str_c(Var1,Var2, ".", Var3, Var4)

MID <- list()

for(FOLD_REP in ALL_combn)
{
load(str_c("~/machine_learning_project/train_again/", args[1], "_", args[2], "_FS_result/FS_",
args[1], "_", args[2], "_", FOLD_REP, ".rda"))

MID[[FOLD_REP]] <-
rfWithFilter$variables %>% group_by(var) %>%
summarise(IMP_Mean = mean(Overall, na.rm = T)) %>%
arrange(-IMP_Mean)
}

Result <-
MID %>% do.call(rbind.data.frame, .) %>%
group_by(var) %>%
summarise(IMP_Mean = mean(IMP_Mean, na.rm = T)) %>%
arrange(-IMP_Mean) %>%
mutate(var = orig_Name[match(var, change_Name)])

quick_write_table(Result,
str_c("~/machine_learning_project/train_again/", args[1], "_", args[2], "_mean_IMP_result.txt"))

}



# # -------------- test manualy
# test2 <-
# rfWithFilter$variables %>% filter(Variables == 200, Resample == "Fold1.Rep01") %>% head(100)

# identical(test1$var, test2$var)
