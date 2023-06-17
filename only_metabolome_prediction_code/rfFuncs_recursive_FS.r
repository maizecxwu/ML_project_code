
library(dplyr)
library(caret)
library(parallel)
library(doParallel)
library(openxlsx)
library(data.table)
library(rrBLUP)


ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)

# Meta_Kernel <- openxlsx::read.xlsx("~/machine_learning_project/Meta_analysis/Meta_kernel.xlsx",
#                              rowNames = F, colNames = T, rows = 3:85)
# Meta_seeding <- openxlsx::read.xlsx("~/machine_learning_project/Meta_analysis/Meta_seeding leaves.xlsx",
#                              rowNames = F, colNames = T, rows = 3:181)
# Meta_reproductive <- openxlsx::read.xlsx("~/machine_learning_project/Meta_analysis/Meta_reproductive_stage_leaves.xlsx",
#                              rowNames = F, colNames = T, rows = 3:174)

# colnames(Meta_Kernel)[2:ncol(Meta_Kernel)] <- 
#       paste0(colnames(Meta_Kernel)[2:ncol(Meta_Kernel)], "_Kernel")

# colnames(Meta_seeding)[2:ncol(Meta_seeding)] <- 
#       paste0(colnames(Meta_seeding)[2:ncol(Meta_seeding)], "_seeding")
      
# colnames(Meta_reproductive)[2:ncol(Meta_reproductive)] <- 
#       paste0(colnames(Meta_reproductive)[2:ncol(Meta_reproductive)], "_reproductive")

#   Yield_Meta <- left_join(ourdata %>% select(Yield_Blup, 2) %>% .[-c(1:2), ], 
#                         Meta_seeding, by = c("line.name" = "Line"))

#   Yield_Meta <- left_join(Yield_Meta, Meta_reproductive, 
#                             by = c("line.name" = "Line"))

#   Yield_Meta <- left_join(Yield_Meta, Meta_Kernel, 
#                             by = c("line.name" = "Line"))

# write.table(Yield_Meta, "~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
#             row.names = F, col.names = T, quote = F, sep = "\t")

Yield_Meta <- read.table("~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
             header = T, check.names = F, sep = "\t")

Yield_Meta <- Yield_Meta[, -c(72:ncol(Yield_Meta))]

impuated_Meta <- Yield_Meta[,-c(1:2)]

Impute_Method <- preProcess(impuated_Meta, method = c("medianImpute"))

impuated_Meta <-
  predict(Impute_Method, impuated_Meta)


# cannot customize the metric fuction
# caretFuncs; rfFuncs   rfFuncs nbFuncs treebagFuncs
filterCtrl <- rfeControl( functions = rfFuncs,
                         method = "repeatedcv", 
                         repeats = 33,returnResamp = "all",number=3,
                         saveDetails = F
)
# speed up the process by parallel
library(parallel)
library(doParallel)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
options(java.parameters = "-Xmx5g") 

# cannot choose the nums of vars customizely
rfWithFilter <- rfe(impuated_Meta, Yield_Meta[,1], 
                      sizes = c(1:68), rfeControl = filterCtrl)

write.table(rfWithFilter$variables,"~/machine_learning_project/Meta_analysis/Early_Meta_rfFuncs_variables.txt",
            row.names = F,col.names = T,sep = "\t",quote = F)

write.table(rfWithFilter$resample,"~/machine_learning_project/Meta_analysis/Early_Meta_rfFuncs_resample.txt",
       row.names = F,col.names = T,sep = "\t",quote = F)




