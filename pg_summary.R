# Code copied from the Keywords program
# Rip IMDB to find Plot Summary and Synopsis
# All movies with Parental Guide data

library(dplyr)
library(rmutil)
library(tidyr)
library(rvest)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")

# Read the data
load(file=paste0(DATA_DIR,"parental_guide.RData"))

# create a keywords dataframe
scrape_summary <- function(id){
  url <- paste0('https://www.imdb.com/title/',id,'/plotsummary')
  webpage <- read_html(url)
  summary_html <- html_nodes(webpage,'.ipc-page-section--base')
  tconst <- id
  title_summary <- trimws( html_text(summary_html[1]))
  title_summary <-  gsub("\n"," ",substr(title_summary,10,9999999))
  title_synopsis <- trimws( html_text(summary_html[2]))
  title_synopsis <-  gsub("\n"," ",substr(title_synopsis,9,9999999))
  plotsummary    <- data.frame(tconst,title_summary,title_synopsis,stringsAsFactors=FALSE)
  return(plotsummary)
}

# If the saved Movie keywords data exists, read it.
if (file.exists(paste0(DATA_DIR,"pg_summary.RData"))) {
  load(paste0(DATA_DIR,"pg_summary.RData"))
}

# If the local file does not exist, initialise it
if (!exists("pg_summary")) {
  pg_summary <- scrape_summary(parental_guide$tconst[1])  # Initialise the keywords data frame
}


# Get the list of IDs looked up

looked_up     <- pg_summary$tconst %>% unique()
movies_notyet <- parental_guide %>% filter(!(tconst %in% looked_up))

while(nrow(movies_notyet)>0){
  looked_up <- pg_summary$tconst %>% unique()
  movies_notyet <- parental_guide %>% filter(!(tconst %in% looked_up))
  print(paste("Movies Looked up:",length(looked_up),"Remaining:",nrow((movies_notyet))))
  for (i in 1:min(100,nrow(movies_notyet))){
    tryCatch({
      title_summary <- scrape_summary(movies_notyet$tconst[i])
      print(paste(i,"Movie:",movies_notyet$tconst[i],
                  "votes:",movies_notyet$numVotes[i],
                  "title:",movies_notyet$primaryTitle[i]))
      pg_summary <- bind_rows(pg_summary,title_summary)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Plot Summary Data Frame")
  save(pg_summary,file=paste0(DATA_DIR,"pg_summary.RData"))
}

write.csv(pg_summary %>% select(-"title_synopsis"),paste0(DATA_DIR,"pg_summary.csv"),row.names = FALSE)
write.csv(pg_summary %>% select(-"title_summary"),paste0(DATA_DIR,"pg_synopsis.csv"),row.names = FALSE)
