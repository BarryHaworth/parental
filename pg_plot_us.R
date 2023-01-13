# Parental Guide plots for US data 
# Plot the average value of Parental Guide scores over time.
# For each MPAA rating (PG, PG-13, R) look at trend in Parental Guides over time
#

library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")
PLOT_DIR    <- paste0(PROJECT_DIR,"plot/")

#load(paste0(DATA_DIR,"parental_guide.RData"))
#load(paste0(DATA_DIR,"country_certificate.RData"))
# Load Country guides
load(paste0(DATA_DIR,"pg_us.RData"))

table(pg_us$startYear)
par(mar=c(2,2,2,2))
hist(pg_us$startYear)

guides   <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables
codes    <- c("sex_code","violence_code","profanity_code","drug_code","intense_code")  # List of Parental Guide variables
mpaa     <- c("G","PG","PG-13","R","X","NC-17")  # MPAA ratings
mpaa_sub <- c("G","PG","PG-13","R")  # Subset of MPAA ratings for plots

# Filter the Titles.  
# Filter by Year: 1980 - 2022
pg_us_filt <- pg_us %>% filter(startYear>=1980 & startYear<=2022)  
# Filter by Major MPAA Categories (G, PG, PG-13, R)
pg_us_filt <- pg_us_filt %>% filter(certificate %in% mpaa_sub)  
# Filter on Titles with all five Parental Guides
pg_us_filt <- pg_us_filt %>% filter(sex!=""&violence!=""&profanity!=""&drugs!=""&intense!="")

# Set guides as ordered factors
pg_us_filt$sex       <- factor(pg_us_filt$sex,      levels=c("None","Mild","Moderate","Severe"))
pg_us_filt$violence  <- factor(pg_us_filt$violence, levels=c("None","Mild","Moderate","Severe"))
pg_us_filt$profanity <- factor(pg_us_filt$profanity,levels=c("None","Mild","Moderate","Severe"))
pg_us_filt$drugs     <- factor(pg_us_filt$drugs,    levels=c("None","Mild","Moderate","Severe"))
pg_us_filt$intense   <- factor(pg_us_filt$intense,  levels=c("None","Mild","Moderate","Severe"))

pg_us_filt$certificate <- factor(pg_us_filt$certificate,  levels=c("G","PG","PG-13","R"))

table(pg_us_filt$startYear)
hist(pg_us_filt$startYear)

summary(pg_us_filt[guides])
summary(pg_us_filt[codes])
summary(pg_us_filt$certificate)

# Melt the data
pg_melt <- melt(pg_us_filt %>% select(tconst,titleType,primaryTitle,startYear,certificate,
                                      sex,violence,profanity,drugs,intense),
                id.vars = c("tconst","titleType","primaryTitle","startYear","certificate")) %>% rename(guide=variable,level=value) 

pg_melt$level <- factor(pg_melt$level,levels=c("None","Mild","Moderate","Severe"))
pg_melt$rating <- as.numeric(pg_melt$level)

png(paste0(PLOT_DIR,"pg_us_all.png"),width=800,height=800)
ggplot(pg_melt, aes(x=startYear, y=rating, color=guide)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Parental Guides by Year and Guide") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="Type")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

# Redo these as a loop?
for (guide in guides){
  
}


png(paste0(PLOT_DIR,"pg_us_sex.png"),width=800,height=800)
ggplot(pg_melt %>% filter(guide=="sex"), aes(x=startYear, y=rating, color=certificate)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Sex Rating by Year and Certificate") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"pg_us_violence.png"),width=800,height=800)
ggplot(pg_melt %>% filter(guide=="violence"), aes(x=startYear, y=rating, color=certificate)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Violence Rating by Year and Certificate") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"pg_us_profanity.png"),width=800,height=800)
ggplot(pg_melt %>% filter(guide=="profanity"), aes(x=startYear, y=rating, color=certificate)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Profanity Rating by Year and Certificate") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"pg_us_drugs.png"),width=800,height=800)
ggplot(pg_melt %>% filter(guide=="drugs"), aes(x=startYear, y=rating, color=certificate)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Drugs & Alcohol Rating by Year and Certificate") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

png(paste0(PLOT_DIR,"pg_us_intense.png"),width=800,height=800)
ggplot(pg_melt %>% filter(guide=="intense"), aes(x=startYear, y=rating, color=certificate)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Frightening & Intense Rating by Year and Certificate") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
  scale_y_continuous(limits=c(1,4),breaks=c(1,2,3,4),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

