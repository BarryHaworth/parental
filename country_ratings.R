# read the certificate data and determine the major rating for each movie
# by selected countries

library(dplyr)
library(ggplot2)
library(forcats)

PROJECT_DIR <- "c:/R/parental"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
PLOT_DIR    <- paste0(PROJECT_DIR,"/plot")

load(paste0(DATA_DIR,"/parental_guide.RData"))
load(paste0(DATA_DIR,"/country_certificate.RData"))

names(parental_guide)
names(country_certificate)

table(country_certificate$country)

# How many Movies? = 49192
print(paste("Number of distinct presentations =",length(unique(parental_guide$tconst))))

cc_table <- data.frame(table(country_certificate$country)) %>% arrange(-Freq)
names(cc_table) <- c("country","freq")

print ("Top ten countries")
head(cc_table,10)

for (c_name in head(cc_table$country,10)){
  cc <- country_certificate %>% filter(country==c_name)
  rating_num <- length(cc$tconst)
  movie_num  <- length(unique(cc$tconst))
  print(paste("Country:",c_name,"has",movie_num,"movies and",rating_num,"ratings"))
  print(table(cc$certificate))
  print("")
}

# To Do:
# Filter countries with more than a threshold of movies Certified
# Top five: USA, UK, Canada, Australia, Germany
# 24 countries with >10K ranked movies.
# List the countries
# For each country, list the major certifications.
# Identify the ones we will work with.


# United States ratings
# https://en.wikipedia.org/wiki/Motion_Picture_Association_film_rating_system
mpaa <- c("G","PG","PG-13","R","X","NC-17")  # MPAA ratings
cc_us <- country_certificate %>% filter(country=="United States") %>% filter(certificate %in% mpaa)
pg_us <- parental_guide %>% select(-certificate) %>% inner_join(cc_us,by="tconst")
save(pg_us,file=paste0(DATA_DIR,"/pg_us.Rdata"))

table(pg_us$certificate)

# United Kingdom
# https://en.wikipedia.org/wiki/British_Board_of_Film_Classification
bbfc <- c("U","PG","12A","12","15","18","R18")
cc_uk <- country_certificate %>% filter(country=="United Kingdom") %>% filter(certificate %in% bbfc)
pg_uk <- parental_guide %>% select(-certificate) %>% inner_join(cc_uk,by="tconst")
save(pg_uk,file=paste0(DATA_DIR,"/pg_uk.Rdata"))

table(pg_uk$certificate)

# Canada
# https://en.wikipedia.org/wiki/Canadian_motion_picture_rating_system
cmprs <- c("G","PG","14A","18A","R","A","13+","16+","18+")
cc_can <- country_certificate %>% filter(country=="Canada") %>% filter(certificate %in% cmprs)
pg_can <- parental_guide %>% select(-certificate) %>% inner_join(cc_can,by="tconst")
save(pg_can,file=paste0(DATA_DIR,"/pg_can.Rdata"))

table(pg_can$certificate)


# Australia
# https://en.wikipedia.org/wiki/Australian_Classification_Board
acb <- c("G","PG","M","MA","MA15+","R","R18+")
cc_aus <- country_certificate %>% filter(country=="Australia") %>% filter(certificate %in% acb)
pg_aus <- parental_guide %>% select(-certificate) %>% inner_join(cc_aus,by="tconst")
save(pg_aus,file=paste0(DATA_DIR,"/pg_aus.Rdata"))

table(pg_aus$certificate)


# Germany (DEU)
# https://www.dw.com/en/how-germanys-film-age-rating-system-works/a-41551312
fsk <- c("0","6","12","16","18")
cc_deu <- country_certificate %>% filter(country=="Germany") %>% filter(certificate %in% fsk)
pg_deu <- parental_guide %>% select(-certificate) %>% inner_join(cc_deu,by="tconst")
save(pg_deu,file=paste0(DATA_DIR,"/pg_deu.Rdata"))

table(pg_deu$certificate)

