# Parental Guide plots
# Plot the average value of Parental Guide scores over time.
# For each MPAA rating (PG, PG-13, R) look at trend in Parental Guides over time
#  This is a first go - still needs some work.
#  Still needs some work.

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

cc_table <- data.frame(table(country_certificate$country)) 
names(cc_table) <- c("country","freq")

# To Do:
# Filter countries with more than a threshold of movies Certified
# Note: 24 countries with >10K ranked movies.
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

mpaa <- c("G","PG","PG-13","R","X","NC-17")  # MPAA ratings

cc_us <- country_certificate %>% filter(country=="United States") %>% filter(certificate %in% mpaa)

pg_us <- parental_guide %>% select(-certificate) %>% inner_join(cc_us,by="tconst")

data.frame(table(pg_us$certificate)) 

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




