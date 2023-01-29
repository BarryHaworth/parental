#  Examine the quality of the Parental Guide data.
#  This program takes the first part of the pg_plot_all program and 
# looks at the data quality.  Things like completeness fof 

library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)
library(stringr)
library(forcats)
library(tidyverse)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")
PLOT_DIR    <- paste0(PROJECT_DIR,"plot/")

countries <- c("us","can","uk","deu","aus")
guides    <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables
codes     <- c("sex_code","violence_code","profanity_code","drug_code","intense_code")  # List of Parental Guide variables

us_certificate   <- c("G","PG","PG-13","R")  
can_certificate  <- c("G","PG","14A","18A","R","A","13+","16+","18+")
uk_certificate   <- c("U","PG","12A","12","15","18")
deu_certificate  <- c("0","6","12","16","18")
aus_certificate  <- c("G","PG","M","MA","MA15+","R","R18+")

for (cty in countries){
  # read the data
  pg <- loadRData(paste0(DATA_DIR,"pg_",cty,".RData"))
  if (cty=="us")  {
    certificates <- us_certificate
    name <- "United States"
  }
  if (cty=="can") {
    certificates <- can_certificate
    name <- "Canada"
  }
  if (cty=="uk")  {
    certificates <- uk_certificate
    name <- "United Kingdom"
  }
  if (cty=="deu") {
    certificates <- deu_certificate
    name <- "Germany"
  }
  if (cty=="aus"){
    certificates <- aus_certificate
    name <- "Australia"
  } 
  print(paste("Quality report for country",name))
  
  # Filter the Titles.  
  # Filter by Year: 1980 - 2022
  pg_filt <- pg %>% filter(startYear>=1980 & startYear<=2022)  
  # Filter by Certificate Categories (G, PG, PG-13, R)
  pg_filt <- pg_filt %>% filter(certificate %in% certificates)  
  # Filter on Titles with all five Parental Guides
  pg_filt <- pg_filt %>% filter(sex!=""|violence!=""|profanity!=""|drugs!=""|intense!="")
  
  # Define missing values variables
  pg_filt <- pg_filt %>%
    mutate(sex_miss       = as.numeric(is.na(sex_code)),
           violence_miss  = as.numeric(is.na(violence_code)),
           profanity_miss = as.numeric(is.na(profanity_code)),
           drugs_miss     = as.numeric(is.na(drug_code)),
           intense_miss   = as.numeric(is.na(intense_code)),
           num_miss       = (sex_miss+violence_miss+profanity_miss+drugs_miss+intense_miss)
           )
  
  png(paste0(PLOT_DIR,"pg_",cty,"_num_miss.png"),width=800,height=800)
  print(ggplot(pg_filt, aes(x=startYear, y=num_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Mean number missing parental codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
  dev.off()
  
  png(paste0(PLOT_DIR,"pg_",cty,"_sex_miss.png"),width=800,height=800)
  print(ggplot(pg_filt, aes(x=startYear, y=sex_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Proportion of Missing Sex codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
  dev.off()
  
  png(paste0(PLOT_DIR,"pg_",cty,"_violence_miss.png"),width=800,height=800)
    print(ggplot(pg_filt, aes(x=startYear, y=violence_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Proportion of Missing Violence codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
    dev.off()
  
    png(paste0(PLOT_DIR,"pg_",cty,"_profanity_miss.png"),width=800,height=800)
    print(ggplot(pg_filt, aes(x=startYear, y=profanity_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Proportion of Missing Profanity codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
    dev.off()
  
    png(paste0(PLOT_DIR,"pg_",cty,"_drugs_miss.png"),width=800,height=800)
    print(ggplot(pg_filt, aes(x=startYear, y=drugs_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Proportion of Missing Drugs codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
    dev.off()
  
    png(paste0(PLOT_DIR,"pg_",cty,"_intense_miss.png"),width=800,height=800)
    print(ggplot(pg_filt, aes(x=startYear, y=intense_miss)) +
    stat_summary(fun=mean, geom="line") +
    ggtitle(paste(name,"Proportion of Missing Intensity codes")) +
    ylim(0,NA) +
    xlab("Year")+ylab("Level") )
    dev.off()
  
  # Set guides as ordered factors
  pg_filt$sex       <- factor(pg_filt$sex,      levels=c("None","Mild","Moderate","Severe"))
  pg_filt$violence  <- factor(pg_filt$violence, levels=c("None","Mild","Moderate","Severe"))
  pg_filt$profanity <- factor(pg_filt$profanity,levels=c("None","Mild","Moderate","Severe"))
  pg_filt$drugs     <- factor(pg_filt$drugs,    levels=c("None","Mild","Moderate","Severe"))
  pg_filt$intense   <- factor(pg_filt$intense,  levels=c("None","Mild","Moderate","Severe"))

  pg_filt$certificate <- factor(pg_filt$certificate,  levels=certificates)
  pg_filt$titleType <- factor(pg_filt$titleType)
  
  table(pg_filt$startYear)
  hist(pg_filt$startYear)
  
  print(summary(pg_filt$titleType))
  print(summary(pg_filt[guides]))
  print(summary(pg_filt[codes]))
  print(summary(pg_filt$certificate))
  print(table(pg_filt$num_miss))
  print(paste(name,"Mean number of missing Sex",mean(pg_filt$sex_miss)))
  print(paste(name,"Mean number of missing Violence",mean(pg_filt$violence_miss)))
  print(paste(name,"Mean number of missing Profanity",mean(pg_filt$profanity_miss)))
  print(paste(name,"Mean number of missing Drugs",mean(pg_filt$drugs_miss)))
  print(paste(name,"Mean number of missing Intense",mean(pg_filt$intense_miss)))
}

