## --------------------------------------t.test --------------------------------------------

library(dplyr)
library(caret)
library(parallel)
library(doParallel)
library(openxlsx)
library(data.table)
library(rrBLUP)
library(Hmisc)

ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)
Y <- ourdata[, ncol(ourdata)] 
rownames(ourdata) <- ourdata$line.name
ourdata <- ourdata[, -c(1:2)]
X <- ourdata[, -ncol(ourdata)] 
X <- X[-which(is.na(Y)), ]
Y <- Y[-which(is.na(Y))]

Impute_Method <- preProcess(X, method = c("knnImpute"))
impuated_X <-predict(Impute_Method, X) 


# get Meta data 


Yield_Meta <- read.table("~/machine_learning_project/Meta_analysis/All_Meta_add_Yield.txt",
             header = T, check.names = F, sep = "\t")

Impute_Method <- preProcess(Yield_Meta[,3:135], method = c("center", "scale"))

impuated_Meta <- predict(Impute_Method, Yield_Meta[,3:135])



# get Bin data 


ourdata <- read.xlsx("~/machine_learning_project/8801a79384a6d889.xlsx", 
                        rowNames = F, colNames = T)

 Bin <- openxlsx::read.xlsx("~/machine_learning_project/bin_inf_predict_yield/Bin_inf.xlsx",
                             rowNames = F, colNames = T, rows = 3:2499)

Bin <- Bin %>% select(-c(Chromosome, `genetic.position.(cM)`)) 

rownames(Bin) <- Bin$bin

Bin <- Bin %>% mutate(bin = NULL) %>% t() %>% as.data.frame()

Bin$Line <- rownames(Bin)

Yield_Bin <- left_join(ourdata %>% select(Yield_Blup, 2), 
                        Bin , by = c("line.name" = "Line"))


 Yield_Bin <- Yield_Bin[-c(1:2),] 

 Bin_matrix <- Yield_Bin[, -c(1:2)]

# 0 is heterozygous, 1 and -1 are parental genotypes
  Bin_matrix [ Bin_matrix == 0] <- NA
  Bin_matrix [ Bin_matrix == 3] <- 0
   Bin_matrix [ Bin_matrix == 1] <- -1
   Bin_matrix [ Bin_matrix == 2] <- 1


# get the agro Traits

Agro <- openxlsx::read.xlsx("~/machine_learning_project/RIL_Agro_traits.xlsx", 
                            rowNames = F, colNames = T)

Agro <- left_join(ourdata %>% select(2, Yield_Blup) %>% na.omit(), Agro, by =c("line.name" = "Line"))

Agro_imputated <- Agro [,-1]

# Start a t.test test

 cl <- makeCluster(3)

    clusterExport(cl, list("impuated_Meta"))

    clusterEvalQ(cl, {
      library(dplyr)
    })



 jj <- parApply(cl = cl, X = impuated_X, MARGIN = 2, function(x) {
          # partition the training data

            apply(impuated_Meta, 2, function(l){
               cor.test(x,l)$p.value
            })
          # Alternatively, you can put in dense matrix, i.e. basic R-matrix
        })

jj <- 
jj %>% as.data.frame() %>% mutate(Meta = rownames(jj)) 

write.table(jj, "~/machine_learning_project/T_test_result/Pheno_Meta_cor_p_value_result.txt",
                row.names = F, col.names = T, quote = F, sep = "\t")


j<-
cor.test(impuated_Meta[,1], Agro_imputated[,1])


j <- read.table("~/machine_learning_project/T_test_result/bin_Pheno_t_test_result.txt",
                header = T, check.names = F, sep = "\t")

j [1:3, 1:3]



