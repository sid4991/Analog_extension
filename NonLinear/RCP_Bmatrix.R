library(furrr, warn.conflicts = FALSE)
library(tidyverse)
library(reshape2, warn.conflicts = FALSE)
library(data.table)
options(dplyr.summarise.inform = FALSE)
options(warn = -1)

data_dir <- path.expand("/home/siddharth.chaudhary/CONUS_Model_52k_rcp_85_10_12_1")
filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)
output1<- lapply(filePaths,fread)
output1 <- rbindlist(output1, use.names = TRUE)
output1 <- dplyr::filter(output1,year>= 2020 & year<=2050)
output1 <- output1[,-c(1,2,3,4,5,7)]
length(output1)

Historical <- output1
Amatrix <- Historical %>% group_by(State_County,model) %>% 
  summarise_all(.funs = c("median"))
Amatrix <- cbind(year = 2000, Amatrix)
print("Matrix created")

write.csv(Amatrix,"/data/project/agaid/rajagopalan_agroecosystems/chaudhary/AnalogData_Sid/Results/Conus/Bmatrix_RCP85_20_50_agland_NonLin.csv",row.names = F)