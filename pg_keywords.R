# Code copied from two_goats old code.
# Rip IMDB to find tags
# All movies with Parental Guide data

library(dplyr)
library(rmutil)
library(tidyr)
library(rvest)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")

# Read the data
load(file=paste0(DATA_DIR,"/parental_guide.RData"))

# create a keywords dataframe
movie_keys <- function(id){
  url <- paste0('https://www.imdb.com/title/',id,'/keywords')
  webpage <- read_html(url)
  tag_html <- html_nodes(webpage,'.sodatext')
  tags <- trimws(gsub('[\n]', '', html_text(tag_html)))
  if (length(tags)==0) tags="No Keywords"
  keywords <- data.frame("tconst"=id,"keywords"=tags)
  return(keywords)
}

# If the saved Movie keywords data exists, read it.
if (file.exists(paste0(DATA_DIR,"/pg_keywords.RData"))) {
  load(paste0(DATA_DIR,"/pg_keywords.RData"))
}

# If the local file does not exist, initialise it
if (!exists("pg_keywords")) {
  pg_keywords <- movie_keys(parental_guide$tconst[1])  # Initialise the keywords data frame
}


# Get the list of IDs looked up

looked_up     <- pg_keywords$tconst %>% unique()
movies_notyet <- parental_guide %>% filter(!(tconst %in% looked_up))

while(nrow(movies_notyet)>0){
  looked_up <- pg_keywords$tconst %>% unique()
  movies_notyet <- parental_guide %>% filter(!(tconst %in% looked_up))
  print(paste("Movies Looked up:",length(looked_up),"Remaining:",nrow((movies_notyet))))
  for (i in 1:min(100,nrow(movies_notyet))){
    tryCatch({
      print(paste(i,"Movie",movies_notyet$tconst[i]
                  ,movies_notyet$primaryTitle[i]))
      keys <- movie_keys(movies_notyet$tconst[i])
      pg_keywords <- bind_rows(pg_keywords,keys)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Keywords Data Frame")
  save(pg_keywords,file=paste0(DATA_DIR,"/pg_keywords.RData"))
}
