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

# Load Country guides
load(paste0(DATA_DIR,"/pg_us.RData"))
load(paste0(DATA_DIR,"/pg_uk.RData"))
load(paste0(DATA_DIR,"/pg_can.RData"))
load(paste0(DATA_DIR,"/pg_aus.RData"))
load(paste0(DATA_DIR,"/pg_deu.RData"))

# Filter the Titles.  Year range 1980 - 2019.  This is the year range used by u/joker_penguin (Pablo)

parental_guide <- parental_guide %>% filter(startYear>=1980 & startYear<=2019)
table(parental_guide$startYear)

names(parental_guide)
names(country_certificate)

guides <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables
codes  <- c("sex_code","violence_code","profanity_code","drug_code","intense_code")  # List of Parental Guide variables

# Set guides as ordered factors
parental_guide$sex       <- factor(parental_guide$sex,levels=c("None","Mild","Moderate","Severe"))
parental_guide$violence  <- factor(parental_guide$violence,levels=c("None","Mild","Moderate","Severe"))
parental_guide$profanity <- factor(parental_guide$profanity,levels=c("None","Mild","Moderate","Severe"))
parental_guide$drugs     <- factor(parental_guide$drugs,levels=c("None","Mild","Moderate","Severe"))
parental_guide$intense   <- factor(parental_guide$intense,levels=c("None","Mild","Moderate","Severe"))

summary(parental_guide[guides])
summary(parental_guide[codes])

# Plots
png(paste0(PLOT_DIR,"/total_titles.png"),width=800,height=800)
ggplot(data=parental_guide) +
  geom_histogram((aes(x=startYear)),  fill="cornflowerblue" ,binwidth = 1) +
  ggtitle("Total Titles by Year 1980 - 2019")
dev.off()

png(paste0(PLOT_DIR,"/total_types.png"),width=800,height=800)
ggplot(data=parental_guide, aes(x=startYear,fill=titleType)) +
  geom_bar() +
  ggtitle("Titles by Type by Year 1980 - 2019")
dev.off()

# Parental Guide scores by Year

pg_year <- parental_guide %>% group_by(startYear) %>%
           summarise(count = n(), 
                     sex = mean(sex_code,na.rm=T),
                     violence = mean(violence_code,na.rm=T),
                     profanity = mean(profanity_code,na.rm=T),
                     drugs = mean(drug_code,na.rm=T),
                     intense = mean(intense_code,na.rm=T)
                     )

png(paste0(PLOT_DIR,"/ave_ratings.png"),width=800,height=800)
ggplot(data=pg_year, aes(x=startYear)) +
  geom_line(aes(y=sex,       colour="sex"),size=1) +
  geom_line(aes(y=drugs,     colour="drugs"),size=1) +
  geom_line(aes(y=violence,  colour="violence"),size=1) +
  geom_line(aes(y=profanity, colour="profanity"),size=1) +
  geom_line(aes(y=intense,   colour="intense"),size=1) +
  ggtitle("Average Parental Guide Scores for all Titles")
dev.off()

# Separate plots for different US Rating groups.
# Note: Try redoing this as a melt
# One plot for each guide, separate lines by rating

pg_year_us <- parental_guide %>% select(-c(mpaa,certificate)) %>% 
  inner_join(pg_us %>% select(tconst,certificate)) %>%
  group_by(startYear, certificate) %>%
  summarise(count = n(), 
            sex = mean(sex_code,na.rm=T),
            violence = mean(violence_code,na.rm=T),
            profanity = mean(profanity_code,na.rm=T),
            drugs = mean(drug_code,na.rm=T),
            intense = mean(intense_code,na.rm=T)
  )

mpaa <- c("G","PG","PG-13","R","X","NC-17")  # MPAA ratings

png(paste0(PLOT_DIR,"/us_rating_g.png"),width=800,height=800)
ggplot(data=pg_year_us %>% filter(certificate=="G") , aes(x=startYear)) +
  geom_line(aes(y=sex,       colour="sex"),size=1) +
  geom_line(aes(y=drugs,     colour="drugs"),size=1) +
  geom_line(aes(y=violence,  colour="violence"),size=1) +
  geom_line(aes(y=profanity, colour="profanity"),size=1) +
  geom_line(aes(y=intense,   colour="intense"),size=1) +
  ggtitle("Average Parental Guide Scores for G rated Titles") +
  xlab("Year")+ylab("Rating") +
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"/us_rating_pg.png"),width=800,height=800)
ggplot(data=pg_year_us %>% filter(certificate=="PG") , aes(x=startYear)) +
  geom_line(aes(y=sex,       colour="sex"),size=1) +
  geom_line(aes(y=drugs,     colour="drugs"),size=1) +
  geom_line(aes(y=violence,  colour="violence"),size=1) +
  geom_line(aes(y=profanity, colour="profanity"),size=1) +
  geom_line(aes(y=intense,   colour="intense"),size=1) +
  ggtitle("Average Parental Guide Scores for PG rated Titles")+
  xlab("Year")+ylab("Rating") +
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"/us_rating_pg-13.png"),width=800,height=800)
ggplot(data=pg_year_us %>% filter(certificate=="PG-13") , aes(x=startYear)) +
  geom_line(aes(y=sex,       colour="sex"),size=1) +
  geom_line(aes(y=drugs,     colour="drugs"),size=1) +
  geom_line(aes(y=violence,  colour="violence"),size=1) +
  geom_line(aes(y=profanity, colour="profanity"),size=1) +
  geom_line(aes(y=intense,   colour="intense"),size=1) +
  ggtitle("Average Parental Guide Scores for PG-13 rated Titles")+
  xlab("Year")+ylab("Rating") + 
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"/us_rating_r.png"),width=800,height=800)
ggplot(data=pg_year_us %>% filter(certificate=="R") , aes(x=startYear)) +
  geom_line(aes(y=sex,       colour="sex"),size=1) +
  geom_line(aes(y=drugs,     colour="drugs"),size=1) +
  geom_line(aes(y=violence,  colour="violence"),size=1) +
  geom_line(aes(y=profanity, colour="profanity"),size=1) +
  geom_line(aes(y=intense,   colour="intense"),size=1) +
  ggtitle("Average Parental Guide Scores for R rated Titles")+
  xlab("Year")+ylab("Rating") + 
  theme(legend.position = "bottom")
dev.off()

