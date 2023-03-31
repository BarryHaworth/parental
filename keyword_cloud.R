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

docs  <- pg_keywords$keywords %>%  VectorSource() %>% Corpus() 

customStop <- c()   # Custom Stopwords
docs <- tm_map(docs, content_transformer(tolower))          # Convert the text to lower case
docs <- tm_map(docs, removeNumbers)                         # Remove numbers
docs <- tm_map(docs, removeWords, stopwords("english"))     # Remove common english stopwords
docs <- tm_map(docs, removeWords, customStop)               # Remove custom stopwords
docs <- tm_map(docs, removePunctuation)                     # Remove punctuation
docs <- tm_map(docs, stripWhitespace)                       # Eliminate extra white spaces

png(paste0(PLOT_DIR,"keyword_all.png"),width=800,height=800)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Keywords All Titles")
wordcloud(words = docs, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"),
          main="Title")
dev.off()

for (guide in guides){
  for (level in levels){
    print(paste("Processing",guide,"Level",level))
    
    filtered <- parental_guide %>% filter((!!sym(guide))== level)
    keywords <- pg_keywords %>% inner_join(filtered %>% select(tconst))
    docs     <- keywords$keywords %>%  VectorSource() %>% Corpus() 
    
    customStop <- c()   # Custom Stopwords
    docs <- tm_map(docs, content_transformer(tolower))          # Convert the text to lower case
    docs <- tm_map(docs, removeNumbers)                         # Remove numbers
    docs <- tm_map(docs, removeWords, stopwords("english"))     # Remove common english stopwords
    docs <- tm_map(docs, removeWords, customStop)               # Remove custom stopwords
    docs <- tm_map(docs, removePunctuation)                     # Remove punctuation
    docs <- tm_map(docs, stripWhitespace)                       # Eliminate extra white spaces
    
    png(paste0(PLOT_DIR,"keyword_",guide,"_",level,".png"),width=800,height=800)
    
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, paste("Keywords for",str_to_title(guide),"=",level))
    wordcloud(words = docs, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"),
              main="Title")
    dev.off()
    
  }
}
