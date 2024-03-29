# Code copied from two_goats old code.
# Rip IMDB to find tags
# All movies with Parental Guide data
#
# Update 05/11/2023
# Update to keyword ripping to take into account new format
# PROBLEM - only gets first 50 keywords for each movie.

library(dplyr)
library(rmutil)
library(tidyr)
library(rvest)
library(xml2)
library(stringr)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")

# Read the data
load(file=paste0(DATA_DIR,"parental_guide.RData"))

# create a keywords dataframe
movie_keys <- function(id){
  url <- paste0('https://www.imdb.com/title/',id,'/keywords')
  webpage <- read_html(url)
  tag_html <- html_nodes(webpage,'.ipc-metadata-list-summary-item__t')
  keyframe <- bind_rows(lapply(xml_attrs(tag_html), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  href <- keyframe$href
  tags <- str_extract(href,"(?<=keywords=).*(?=&ref)")
  if (length(tags)==0) tags="No Keywords"
  keywords <- data.frame("tconst"=id,"keywords"=tags)
  return(keywords)
}

#movie_keys("tt0111161")

# If the saved Movie keywords data exists, read it.
if (file.exists(paste0(DATA_DIR,"pg_keywords.RData"))) {
  load(paste0(DATA_DIR,"pg_keywords.RData"))
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
      keys <- movie_keys(movies_notyet$tconst[i])
      print(paste(i,"Movie:",movies_notyet$tconst[i],
                  "votes:",movies_notyet$numVotes[i],
                  "# keywords:",nrow(keys),
                  "title:",movies_notyet$primaryTitle[i]))
      pg_keywords <- bind_rows(pg_keywords,keys)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Keywords Data Frame")
  save(pg_keywords,file=paste0(DATA_DIR,"pg_keywords.RData"))
}

write.csv(pg_keywords,paste0(DATA_DIR,"pg_keywords.csv"),row.names = FALSE)
