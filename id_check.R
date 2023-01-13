#  ID Check
# Check that Parental and Parental Detail have the same movies 
# and remove any that are not in both

library(rvest)
library(dplyr)
library(rmutil)
library(stringr)

options(timeout= 4000000)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"/data/")
FILE_DIR    <- paste0(DATA_DIR,"/tsv/")

load(file=paste0(DATA_DIR,"parental.RData"))
load(file=paste0(DATA_DIR,"parental_guide.RData"))
load(file=paste0(DATA_DIR,"parental_detail.RData"))
load(file=paste0(DATA_DIR,"parental_detail_guide.RData"))

parental_ids        <- parental %>% select(tconst) %>% unique()
parental_detail_ids <- parental_detail %>% select(tconst) %>% unique()

common_ids <- parental_ids %>% inner_join(parental_detail_ids)

parental              <- parental %>% inner_join(common_ids)
parental_guide        <- parental_guide %>% inner_join(common_ids)
parental_detail       <- parental_detail %>% inner_join(common_ids)
parental_detail_guide <- parental_detail_guide %>% inner_join(common_ids)

save(parental             ,file=paste0(DATA_DIR,"parental.Rdata"))
save(parental_guide       ,file=paste0(DATA_DIR,"parental_guide.Rdata"))
save(parental_detail      ,file=paste0(DATA_DIR,"parental_detail.Rdata"))
save(parental_detail_guide,file=paste0(DATA_DIR,"parental_detail_guide.Rdata"))
