# Keyword Cloud
#  Things to look at:
#  Word clouds by Rating Category
# Word Clouds by Guide Category (sex=none, sex=mild etc.)
# Changes over time?
# ggplot wordcloud using code from 
#  https://semba-blog.netlify.app/11/05/2019/wordclouds-plotting-with-ggwordcloud-package-in-r/

library(dplyr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggwordcloud)
library(stringr)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")
PLOT_DIR    <- paste0(PROJECT_DIR,"plot/")

load(paste0(DATA_DIR,"pg_keywords.RData"))
load(paste0(DATA_DIR,"parental_guide.RData"))

guides  <- c("sex","violence","profanity","drugs","intense")  # List of Parental Guide variables
levels  <- c("None","Mild","Moderate","Severe")

key_tab <- data.frame(table(pg_keywords$keywords),stringsAsFactors = F) %>% rename(keyword=Var1) %>% arrange(desc(Freq))
key_tab$keyword <- as.character(key_tab$keyword)

write.csv(key_tab,paste0(DATA_DIR,"keyword_table.csv"),row.names=FALSE)

key_tab <- key_tab[nchar(key_tab$keyword) <=20,] # Filter longer keywords

png(paste0(PLOT_DIR,"keyword_all.png"),width=800,height=800)

wordcloud(key_tab$keyword,key_tab$Freq,
          min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),
          scale=c(2, .5),
          main="Title")

dev.off()

for (guide in guides){
  for (level in levels){
    print(paste("Processing",guide,"Level",level))
    
    filtered <- parental_guide %>% filter((!!sym(guide))== level)
    keywords <- pg_keywords %>% inner_join(filtered %>% select(tconst))
    key_tab <- data.frame(table(keywords$keywords)) %>% rename(keyword=Var1) %>% arrange(desc(Freq))
    key_tab$keyword <- as.character(key_tab$keyword)
    write.csv(key_tab,paste0(DATA_DIR,"keyword_table_",guide,"_",level,".csv"),row.names=FALSE)
    key_tab <- key_tab[nchar(key_tab$keyword) <=20,] # Filter longer keywords
    
    png(paste0(PLOT_DIR,"keyword_",guide,"_",level,".png"),width=800,height=800)
    
    plot.new()
    wordcloud(key_tab$keyword,key_tab$Freq,
              min.freq = 100,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"),
              scale=c(2, .5),
              main="Title")
    dev.off()
    
  }
}
