# Parental Guide plots for US data 
# Plot the average value of Parental Guide scores over time.
# For each MPAA rating (PG, PG-13, R) look at trend in Parental Guides over time
#
# Changed file format to SVG instead of PNG

library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)
library(stringr)
library(forcats)

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

pg_melt$level  <- factor(pg_melt$level,levels=c("None","Mild","Moderate","Severe"))
pg_melt$rating <- factor(pg_melt$certificate,levels=c("G","PG","PG-13","R"))
pg_melt$score  <- as.numeric(pg_melt$level)  # Numeric value to parental guide 
pg_melt$score  <- pg_melt$score-1             # Adjust to scale 0-3 instead of 1-4

#png(paste0(PLOT_DIR,"pg_us_all.png"),width=800,height=800)
svg(paste0(PLOT_DIR,"pg_us_all.svg"))
ggplot(pg_melt, aes(x=startYear, y=score, color=guide)) + 
  stat_summary(fun=mean, geom="line", size=1)+
  ggtitle("Parental Guides by Year and Guide") +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("Level") + labs(color="Type")+ 
  scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
  theme(legend.position = "bottom")
dev.off()

# US Ratings by Guide as a loop
for (gde in guides){
  single_guide <- pg_melt %>% filter(guide==gde)
  print(paste0("Plotting ",gde," to ","pg_us_",gde,".png"))
  #png(paste0(PLOT_DIR,"pg_us_",gde,".png"),width=800,height=800)
  svg(paste0(PLOT_DIR,"pg_us_",gde,".svg"))
  print(ggplot(single_guide , aes(x=startYear, y=score, color=certificate)) + 
    stat_summary(fun=mean, geom="line", size=1)+
    ggtitle(paste(str_to_title(gde),"Rating by Year and Certificate")) +
    theme(plot.title=element_text(hjust=0.5))+
    xlab("Year")+ylab("Level") + labs(color="MPAA Certificate")+ 
      scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
      theme(legend.position = "bottom"))
  dev.off()
}

# US Ratings by MPAA as a loop
for (cert in mpaa_sub){
  single_cert <- pg_melt %>% filter(certificate==cert)
  print(paste0("Plotting ",cert," to ","pg_us_",cert,".png"))
  #png(paste0(PLOT_DIR,"pg_us_",cert,".png"),width=800,height=800)
  svg(paste0(PLOT_DIR,"pg_us_",cert,".svg"))
  print(ggplot(single_cert , aes(x=startYear, y=score, color=guide)) + 
    stat_summary(fun=mean, geom="line", size=1)+
    ggtitle(paste(cert,"Rating by Year and Parental Guide")) +
    theme(plot.title=element_text(hjust=0.5))+
    xlab("Year")+ylab("Level") + labs(color="Parental Guide")+ 
      scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe"))+
      theme(legend.position = "bottom"))
  dev.off()
}

# Stacked Area Cert by Guide with ave line
for (cert in mpaa_sub){
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

    single_null$level <- factor(single_null$level,levels=c("None","Mild","Moderate","Severe"))
    
    single_miss <- single_null %>% anti_join(single_gc %>% select(startYear,level) %>% unique())

    single_gc <- rbind(single_gc,single_miss) %>% arrange(startYear,level) %>% unique()
    
    single_gc_ave <- pg_melt %>% filter(guide==gde) %>% filter(certificate==cert) %>% 
      group_by(startYear) %>% summarise(ave_score = mean(score))

    #Line Superimposed on Area
    print(paste0("Plotting Certificate ",cert," and Guide ",gde," to ","pg_us_",cert,"_",gde,".png"))
    #png(paste0(PLOT_DIR,"pg_us_",cert,"_",gde,".png"),width=800,height=800)
    svg(paste0(PLOT_DIR,"pg_us_",cert,"_",gde,".svg"))
    print(ggplot() +
      geom_area(data=single_gc , aes(x=startYear, y=title_pct,  fill=fct_rev(level)), alpha=0.6 , size=0.1) +
      geom_line(data=single_gc_ave , aes(x=startYear, y=ave_score)) +
        ggtitle(paste("MPAA Certificate:",cert,"Parental Guide:",str_to_title(gde),"Rating by Year")) +
        theme(plot.title=element_text(hjust=0.5))+
        xlab("Year")+ylab("Level") + labs(color="Parental Guide")+ 
        guides(fill=guide_legend(title="Level:")) +
        scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),labels=c("None","Mild","Moderate","Severe")) +
        theme(legend.position = "bottom"))
    dev.off()
  }
}
