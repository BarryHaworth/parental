#  Investigate the composition of the family guide data
#  Looking at those titles where guide information is incomplete.
#
# To Do:  Look at which movies have or do not have guides, keywords, summary & Synopsis.
# Note: Many records have synopsis "It looks like we don't have any synopsis for this title yet. Be the first to contribute.Learn more"

library(dplyr)
library(ggplot2)
library(forcats)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")
PLOT_DIR    <- paste0(PROJECT_DIR,"plot/")

load(paste0(DATA_DIR,"parental_detail_guide.RData"))
load(paste0(DATA_DIR,"pg_keywords.RData"))
load(paste0(DATA_DIR,"pg_summary.RData"))

pg_synopsis <- pg_summary %>% 
  select(-"title_summary") %>% 
  filter(title_synopsis!="It looks like we don't have any synopsis for this title yet. Be the first to contribute.Learn more")
pg_summary <- pg_summary %>% select(-"title_synopsis") 

pg_key_count <- pg_keywords %>% 
  group_by(tconst) %>%
  summarise(key_count=n())

table(pg_key_count$key_count)
  
#names(parental_guide)
names(parental_detail_guide)

# Filter movies with no parental guides
parental_detail_none <- parental_detail_guide %>% filter(sex==""&violence==""&profanity==""&drugs==""&intense=="")
parental_detail_any  <- parental_detail_guide %>% filter(sex!=""|violence!=""|profanity!=""|drugs!=""|intense!="")
parental_detail_all  <- parental_detail_guide %>% filter(sex!=""&violence!=""&profanity!=""&drugs!=""&intense!="")

parental_detail_part <- parental_detail_any %>% anti_join(parental_detail_all)

print(paste("Number of distinct titles:          ",length(unique(parental_detail_guide$tconst))))
print(paste("Titles with no guides:               ",length(unique(parental_detail_none$tconst))))
print(paste("Titles with some but not all guides: ",length(unique(parental_detail_part$tconst))))
print(paste("Titles with all the guides:          ",length(unique(parental_detail_all$tconst))))

parental_detail_guide$sex       <- factor(parental_detail_guide$sex,      levels=c("None","Mild","Moderate","Severe"))
parental_detail_guide$violence  <- factor(parental_detail_guide$violence, levels=c("None","Mild","Moderate","Severe"))
parental_detail_guide$profanity <- factor(parental_detail_guide$profanity,levels=c("None","Mild","Moderate","Severe"))
parental_detail_guide$drugs     <- factor(parental_detail_guide$drugs,    levels=c("None","Mild","Moderate","Severe"))
parental_detail_guide$intense   <- factor(parental_detail_guide$intense,  levels=c("None","Mild","Moderate","Severe"))

guides    <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables
summary(parental_detail_part[guides])

