# Parental Guide plots for All saved country data 
# Plot the average value of Parental Guide scores over time.
# For each Certificate rating look at trend in Parental Guides over time
#
# Changed file format to SVG instead of PNG
#
#  This version includes all titles with at least one parental guide
# and encodes missing values as "no rating"

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
countries <- c("us")
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
  
  # Filter the Titles.  
  pg_filt <- pg %>% 
    # Filter by Year: 1980 - 2022
    filter(startYear>=1980 & startYear<=2022)  %>% 
  # Filter by Certificate Categories (G, PG, PG-13, R)
    filter(certificate %in% certificates)  %>%
  # Filter on Titles with at least one Parental Guides
    filter(sex!=""|violence!=""|profanity!=""|drugs!=""|intense!="") 
  
  pg_filt$sex[pg_filt$sex==""] <- "No Rating"
  pg_filt$violence[pg_filt$violence==""] <- "No Rating"
  pg_filt$profanity[pg_filt$profanity==""] <- "No Rating"
  pg_filt$drugs[pg_filt$drugs==""] <- "No Rating"
  pg_filt$intense[pg_filt$intense==""] <- "No Rating"
  
  pg_filt
  
  # Set guides as ordered factors
  pg_filt$sex       <- factor(pg_filt$sex,      levels=c("No Rating","None","Mild","Moderate","Severe"))
  pg_filt$violence  <- factor(pg_filt$violence, levels=c("No Rating","None","Mild","Moderate","Severe"))
  pg_filt$profanity <- factor(pg_filt$profanity,levels=c("No Rating","None","Mild","Moderate","Severe"))
  pg_filt$drugs     <- factor(pg_filt$drugs,    levels=c("No Rating","None","Mild","Moderate","Severe"))
  pg_filt$intense   <- factor(pg_filt$intense,  levels=c("No Rating","None","Mild","Moderate","Severe"))
  
  pg_filt$certificate <- factor(pg_filt$certificate,  levels=certificates)
  
  table(pg_filt$startYear)
  hist(pg_filt$startYear)
  
  summary(pg_filt[guides])
  summary(pg_filt[codes])
  summary(pg_filt$certificate)
  
  # Melt the data
  pg_melt <- melt(pg_filt %>% select(tconst,titleType,primaryTitle,startYear,certificate,
                                        sex,violence,profanity,drugs,intense),
                  id.vars = c("tconst","titleType","primaryTitle","startYear","certificate")) %>% rename(guide=variable,level=value) 
  
  pg_melt$level  <- factor(pg_melt$level,levels=c("No Rating","None","Mild","Moderate","Severe"))
  pg_melt$certificate <- factor(pg_melt$certificate,levels=certificates)
  pg_melt$score  <- as.numeric(pg_melt$level)  # Numeric value to parental guide 
  pg_melt$score  <- pg_melt$score-1             # Adjust to scale 0-3 instead of 1-4
  
  png(paste0(PLOT_DIR,"pg_nr_",cty,"_all.png"),width=800,height=800)
  print(ggplot(pg_melt, aes(x=startYear, y=score, color=guide)) + 
    stat_summary(fun=mean, geom="line", size=1)+
    ggtitle(paste(name,"Parental Guides by Year and Guide")) +
    theme(plot.title=element_text(hjust=0.5))+
    xlab("Year")+ylab("Level") + labs(color="Type")+ 
    scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
    theme(legend.position = "bottom") )
  dev.off()
  
  #  certificates by Guide as a loop
  for (gde in guides){
    single_guide <- pg_melt %>% filter(guide==gde)
    print(paste0("Plotting ",gde," to ","pg_nr_nr_",cty,"_",gde,".png"))
    png(paste0(PLOT_DIR,"pg_nr_nr_",cty,"_",gde,".png"),width=800,height=800)
    print(ggplot(single_guide , aes(x=startYear, y=score, color=certificate)) + 
            stat_summary(fun=mean, geom="line", size=1)+
            ggtitle(paste(name,str_to_title(gde),"Rating by Year and Certificate")) +
            theme(plot.title=element_text(hjust=0.5))+
            xlab("Year")+ylab("Level") + labs(color="Certificate")+ 
            scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
            theme(legend.position = "bottom"))
    dev.off()
  }
  
  # Ratings plots as a loop
  for (cert in certificates){
    single_cert <- pg_melt %>% filter(certificate==cert)
    print(paste0("Plotting ",cert," to ","pg_nr_nr_",cty,"_",cert,".png"))
    png(paste0(PLOT_DIR,"pg_nr_nr_",cty,"_",cert,".png"),width=800,height=800)
    print(ggplot(single_cert , aes(x=startYear, y=score, color=guide)) + 
            stat_summary(fun=mean, geom="line", size=1)+
            ggtitle(paste(name,cert,"Rating by Year and Parental Guide")) +
            theme(plot.title=element_text(hjust=0.5))+
            xlab("Year")+ylab("Level") + labs(color="Parental Guide")+ 
            scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
            theme(legend.position = "bottom"))
    dev.off()
  }
  
  # Stacked Area Cert by Guide with ave line
  for (cert in certificates){
    for (gde in guides){
      single_gc <- pg_melt %>% filter(guide==gde) %>% filter(certificate==cert) %>% 
        group_by(startYear, level) %>% summarise(titles=n()) %>% 
        mutate(title_pct = 3*titles/sum(titles), 
               sum_titles = sum(titles),
               sum_pct = sum(title_pct) )
      
      sum_titles <- single_gc %>% select(startYear,sum_titles) %>% unique()
      
      single_null <- data.frame(startYear=rep(seq(1980,2022),4),
                                level=rep(c("None","Mild","Moderate","Severe"),43),
                                titles=rep(0,172),
                                title_pct=rep(0,172),
                                sum_pct=rep(3,172))
      single_null <- single_null %>% left_join(sum_titles)
      
      single_null$level <- factor(single_null$level,levels=c("No Rating","None","Mild","Moderate","Severe"))
      
      single_miss <- single_null %>% anti_join(single_gc %>% select(startYear,level) %>% unique())
      
      single_gc <- rbind(single_gc,single_miss) %>% arrange(startYear,level) %>% unique()
      
      single_gc_ave <- pg_melt %>% filter(guide==gde) %>% filter(certificate==cert) %>% 
        group_by(startYear) %>% summarise(ave_score = mean(score))
      
      #Line Superimposed on Area
      print(paste0("Plotting Certificate ",cert," and Guide ",gde," to ","pg_nr_",cty,"_",cert,"_",gde,".png"))
      png(paste0(PLOT_DIR,"pg_nr_",cty,"_",cert,"_",gde,".png"),width=800,height=800)
      print(ggplot() +
              geom_area(data=single_gc , aes(x=startYear, y=title_pct,  fill=fct_rev(level)), alpha=0.6 , size=0.1) +
              geom_line(data=single_gc_ave , aes(x=startYear, y=ave_score)) +
              ggtitle(paste(name,"Certificate:",cert,"Parental Guide:",str_to_title(gde),"Rating by Year")) +
              theme(plot.title=element_text(hjust=0.5))+
              xlab("Year")+ylab("Level") + labs(color="Parental Guide")+ 
              guides(fill=guide_legend(title="Level:")) +
              scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("No Rating","None","Mild","Moderate","Severe")) +
              theme(legend.position = "bottom"))
      dev.off()
    }
  }
}

