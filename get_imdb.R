# Get copies of files
# Files are downloaded from IMDB.
# Files are filtered to include movies & TV series/miniseries only
# 22/06/2022  Fixed a reading error where quote in text were messing things up.
# copied from two_goats to the new parental project
# 26/01/2023  Added dir.create for running in new environment.

library(tidyr)
library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/parental/"
DATA_DIR    <- paste0(PROJECT_DIR,"data/")
FILE_DIR    <- paste0(DATA_DIR,"tsv/")

dir.create(DATA_DIR)
dir.create(FILE_DIR)

get_title <- function(file){
  local_file <- paste0(FILE_DIR,file,".tsv.gz")
  remote_file <- paste0("https://datasets.imdbws.com/",file,".tsv.gz")
  if (!file.exists(local_file)) {
    print(paste("Downloading New File:",remote_file,"to Local file:",local_file)) 
    download.file(remote_file,local_file)
  }
  else if (as.Date(file.info(local_file)$mtime) != Sys.Date()){
    print(paste("File",local_file,"last updated",as.Date(file.info(local_file)$mtime),", updating"))
    download.file(remote_file,local_file)
  } else {
    print(paste("File",local_file,"already Up to date"))
  }
}

# Download the files
get_title("title.ratings")
get_title("title.episode")
get_title("title.basics")

#get_title("name.basics")
#get_title("title.crew")
#get_title("title.principals")  
#get_title("title.akas")

# Episodes
episode  <- read.delim(paste0(FILE_DIR,"title.episode.tsv.gz") ,stringsAsFactors = FALSE ,quote="")
save(episode,file=paste0(DATA_DIR,"episode.RData"))   # Save Episode Data Frame

episodes <- episode %>% select(parentTconst) %>% group_by(parentTconst) %>% 
            summarise(episodes = n()) %>% rename(tconst=parentTconst)
save(episodes,file=paste0(DATA_DIR,"episodes.RData"))   # Save Episodes Data Frame

basics  <- read.delim(paste0(FILE_DIR,"title.basics.tsv.gz") ,stringsAsFactors = FALSE ,quote="")

# Set types for columns
basics$isAdult   <- as.numeric(basics$isAdult)
basics$startYear <- as.numeric(basics$startYear)
basics$endYear   <- as.numeric(basics$endYear)
basics$runtimeMinutes <- as.numeric(basics$runtimeMinutes)

# Clean Basics
keeptypes <- c("movie","tvMovie","tvSeries","tvMiniSeries","tvSpecial","video","videoGame")  # List of types to keep
basics    <- basics %>% filter(titleType %in% keeptypes)  # Only keep selected types

# Impute unknown run time with average of type
basics <- basics %>% mutate(runtimeMinutes= ifelse(is.na(runtimeMinutes), mean(runtimeMinutes, na.rm=TRUE), runtimeMinutes))

basics <- basics[basics$startYear <= as.numeric(substr(Sys.Date(),1,4)),]   # drop release date after this year

# Define total run time.  
# Note that the runimeMinutes variable from IMDB is the total for movies or miniseries, 
# but must be multiplied by total number of episodes for a TV series
basics <- basics %>% 
  left_join(episodes,by="tconst") %>% 
  replace_na(list(episodes=1))    %>%  
  mutate(totalRuntime=ifelse(titleType=="tvSeries", episodes*runtimeMinutes,runtimeMinutes))

save(basics,file=paste0(DATA_DIR,"basics.RData"))

# Ratings
ratings <- read.delim(paste0(FILE_DIR,"title.ratings.tsv.gz") ,stringsAsFactors = FALSE ,quote="")
save(ratings,file=paste0(DATA_DIR,"ratings.RData"))
