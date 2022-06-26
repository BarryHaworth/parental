# Parental Guide plots
# Plot the average value of Parental Guide scores over time.
# For each MPAA rating (PG, PG-13, R) look at trend in Parental Guides over time
#  This is a first go - still needs some work.

library(dplyr)
library(ggplot2)
library(forcats)

PROJECT_DIR <- "c:/R/parental"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

load(paste0(DATA_DIR,"/parental_guide.RData"))

names(parental_guide)
table(parental_guide$mpaa_rating)

guides <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables

parental_guide$sex <- factor(parental_guide$sex,levels=c("None","Mild","Moderate","Severe"))
parental_guide$violence <- factor(parental_guide$violence,levels=c("None","Mild","Moderate","Severe"))
parental_guide$profanity <- factor(parental_guide$profanity,levels=c("None","Mild","Moderate","Severe"))
parental_guide$drugs <- factor(parental_guide$drugs,levels=c("None","Mild","Moderate","Severe"))
parental_guide$intense <- factor(parental_guide$intense,levels=c("None","Mild","Moderate","Severe"))

summary(parental_guide %>% select(sex,violence,profanity,drugs,intense))

# Keep the known ratings
ratings =c("PG","PG-13","R")
parental_guide <- parental_guide %>% filter(mpaa_rating %in% ratings) %>% filter(startYear > 1989) 

ggplot(data=parental_guide) +
  geom_histogram((aes(x=startYear)),  fill="cornflowerblue" ,binwidth = 1)

ggplot(data=parental_guide, aes(x=startYear, fill=mpaa_rating)) +
  geom_bar()

ggplot(data=parental_guide %>% filter(mpaa_rating=="PG",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=parental_guide %>% filter(mpaa_rating=="PG-13",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=parental_guide %>% filter(mpaa_rating=="R",sex !=""), aes(x=startYear, fill=fct_rev(sex) )) +
  geom_bar(position="fill")

ggplot(data=parental_guide %>% filter(mpaa_rating=="PG",violence !=""), aes(x=startYear, fill=fct_rev(violence))) +
  geom_bar(position="fill")


