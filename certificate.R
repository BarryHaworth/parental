# Parse the Certificate field
# The certificate field contains individual country censorship certifications
# Countries are separated by the | character, and country name and rating
# are separated by a colon.
# This program parses this field and outputs one line per country/rating 
# combination, then saves this to a separate file.  Note that because a 
# movie rating may be re-appraised over time that a movie may have more
# than one rating in the same country.

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(rmutil)

PROJECT_DIR <- "c:/R/parental"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

load(file=paste0(DATA_DIR,"/parental_guide.Rdata"))

# Get the Certificate field

country_certificate <- parental_guide %>% select(tconst,certificate) %>% 
  filter(certificate != "") %>% 
#  head(100) %>%  
  mutate(certificates=str_split(certificate,"\\|")) %>% 
  select(-certificate) %>% unnest(certificates) %>%
  mutate(country=word(certificates,1,sep=":"),
         certificate=word(word(certificates,2,sep=":"),1,sep=" "))
  
head(country_certificate)

save(country_certificate,file=paste0(DATA_DIR,"/country_certificate.Rdata"))
write.csv(country_certificate,paste0(DATA_DIR,"/country_certificate.csv"),row.names = FALSE)
