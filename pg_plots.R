# Parental Guide plots
# Plot the average value of Parental Guide scores over time.
# For each MPAA rating (PG, PG-13, R) look at trend in Parental Guides over time
#  This is a first go - still needs some work.
#  Still needs some work.
#
# To do:
# 1) Identify the ratings for main countries (US, UK, etc.  24 countries with )
#  - Done for top five countries
# 2) Plot changes in the five parental guides over time
#  Guide by guide?
# For each country, for each classification:
#  Guide by guide stacked histogram
# Guide by guide line plot
#  All guides line plot
# Country by guide ratings plot

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

cc_table <- data.frame(table(country_certificate$country)) %>% arrange(-Freq)
names(cc_table) <- c("country","freq")

head(cc_table,10)

# To Do:
# Filter countries with more than a threshold of movies Certified
# Top five: USA, UK, Canata, Australia, Germany
# 24 countries with >10K ranked movies.
# List the countries
# For each country, list the major certifications.
# Identify the ones we will work with.


guides <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables

parental_guide$sex       <- factor(parental_guide$sex,levels=c("None","Mild","Moderate","Severe"))
parental_guide$violence  <- factor(parental_guide$violence,levels=c("None","Mild","Moderate","Severe"))
parental_guide$profanity <- factor(parental_guide$profanity,levels=c("None","Mild","Moderate","Severe"))
parental_guide$drugs     <- factor(parental_guide$drugs,levels=c("None","Mild","Moderate","Severe"))
parental_guide$intense   <- factor(parental_guide$intense,levels=c("None","Mild","Moderate","Severe"))

summary(parental_guide %>% select(sex,violence,profanity,drugs,intense))

# United States ratings
# https://en.wikipedia.org/wiki/Motion_Picture_Association_film_rating_system
mpaa <- c("G","PG","PG-13","R","X","NC-17")  # MPAA ratings
cc_us <- country_certificate %>% filter(country=="United States") %>% filter(certificate %in% mpaa)
pg_us <- parental_guide %>% select(-certificate) %>% inner_join(cc_us,by="tconst")

table(pg_us$certificate)

# United Kingdom
# https://en.wikipedia.org/wiki/British_Board_of_Film_Classification
bbfc <- c("U","PG","12A","12","15","18","R18")
cc_uk <- country_certificate %>% filter(country=="United Kingdom") %>% filter(certificate %in% bbfc)
pg_uk <- parental_guide %>% select(-certificate) %>% inner_join(cc_uk,by="tconst")

table(pg_uk$certificate)

# Canada
# https://en.wikipedia.org/wiki/Canadian_motion_picture_rating_system
cmprs <- c("G","PG","14A","18A","R","A","13+","16+","18+")
cc_can <- country_certificate %>% filter(country=="Canada") %>% filter(certificate %in% cmprs)
pg_can <- parental_guide %>% select(-certificate) %>% inner_join(cc_can,by="tconst")

table(pg_can$certificate)


# Australia
# https://en.wikipedia.org/wiki/Australian_Classification_Board
acb <- c("G","PG","M","MA","MA15+","R","R18+")
cc_aus <- country_certificate %>% filter(country=="Australia") %>% filter(certificate %in% acb)
pg_aus <- parental_guide %>% select(-certificate) %>% inner_join(cc_aus,by="tconst")

table(pg_aus$certificate)


# Germany
# https://www.dw.com/en/how-germanys-film-age-rating-system-works/a-41551312
fsk <- c("0","6","12","16","18")
cc_ger <- country_certificate %>% filter(country=="Germany") %>% filter(certificate %in% fsk)
pg_ger <- parental_guide %>% select(-certificate) %>% inner_join(cc_ger,by="tconst")

table(pg_ger$certificate)

# Plots
ggplot(data=pg_us) +
  geom_histogram((aes(x=startYear)),  fill="cornflowerblue" ,binwidth = 1)

ggplot(data=pg_us %>% filter(startYear>= 1970), aes(x=startYear, fill=certificate)) +
  geom_bar()

ggplot(data=pg_us %>% filter(startYear>= 1970, certificate=="G",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=pg_us %>% filter(startYear>= 1970, certificate=="PG",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=pg_us %>% filter(startYear>= 1970, certificate=="PG-13",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=pg_us %>% filter(startYear>= 1970, certificate=="R",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")


