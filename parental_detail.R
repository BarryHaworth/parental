# Rip Parental Guidance details
# 23/06/2022 - first version.  Some problems where a movie did not have all five guides.
# 24/06/2022  Updated to return blank values if not included.
# 26/06/2022  Moved from IMDB project to separate parental guidance project
# 27/06/2022  Changed delimiter character with certificate from comma to | 
#
# 04/07/2022  Try to capture the number of votes for each parental guide

library(rvest)
library(dplyr)
library(rmutil)
library(stringr)

options(timeout= 4000000)

print("Program started")
timestamp()

PROJECT_DIR <- "c:/R/parental"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
FILE_DIR    <- paste0(DATA_DIR,"/tsv")

load(paste0(DATA_DIR,"/basics.RData"))
load(paste0(DATA_DIR,"/ratings.RData"))

# Read Parental Guidance details for a single movie
guide_rip <- function(tconst){
  url <- paste0('https://www.imdb.com/title/',tconst,'/parentalguide')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  mpaa_html  <- html_nodes(webpage,'td')
  if (length(mpaa_html)==4){
    mpaa <- html_text(mpaa_html[2])  
  } else {
       mpaa <- ""
  }
  cert_html   <- html_nodes(webpage,'.ipl-inline-list')
  if (length(cert_html)==2){  certificate <- gsub("\n","|",html_text2(cert_html[2]))
  } else {certificate <- ""}
  guide_html <- html_nodes(webpage,'.ipl-status-pill')
  if (length(guide_html)==0){
    sex       <- ""
    violence  <- ""
    profanity <- ""
    drugs     <- ""
    intense   <- ""
  } else {
    i=1
    sex <- html_text(guide_html[i])
    if (sex==""){i=i+1}  else {i=i+2}
    violence <- html_text(guide_html[i])
    if (violence==""){i=i+1}  else {i=i+2}
    profanity <- html_text(guide_html[i])
    if (profanity==""){i=i+1}  else {i=i+2}
    drugs <- html_text(guide_html[i])
    if (drugs==""){i=i+1}  else {i=i+2}
    intense <- html_text(guide_html[i])
  }
  
  detail_html <- html_nodes(webpage,'.advisory-severity-vote__vote-button-container')
  sex_details  <- strsplit(html_text(detail_html[[1]]),"\n")[[1]]
  sex_none     <- as.numeric(gsub(",","",sex_details[4]))
  sex_mild     <- as.numeric(gsub(",","",sex_details[8]))
  sex_moderate <- as.numeric(gsub(",","",sex_details[12]))
  sex_severe   <- as.numeric(gsub(",","",sex_details[16]))
  sex_total    <- sex_none+sex_mild+sex_moderate+sex_severe
  violence_details  <- strsplit(html_text(detail_html[[2]]),"\n")[[1]]
  violence_none     <- as.numeric(gsub(",","",violence_details[4]))
  violence_mild     <- as.numeric(gsub(",","",violence_details[8]))
  violence_moderate <- as.numeric(gsub(",","",violence_details[12]))
  violence_severe   <- as.numeric(gsub(",","",violence_details[16]))
  violence_total    <- violence_none+violence_mild+violence_moderate+violence_severe
  profanity_details  <- strsplit(html_text(detail_html[[3]]),"\n")[[1]]
  profanity_none     <- as.numeric(gsub(",","",profanity_details[4]))
  profanity_mild     <- as.numeric(gsub(",","",profanity_details[8]))
  profanity_moderate <- as.numeric(gsub(",","",profanity_details[12]))
  profanity_severe   <- as.numeric(gsub(",","",profanity_details[16]))
  profanity_total    <- profanity_none+profanity_mild+profanity_moderate+profanity_severe
  drugs_details  <- strsplit(html_text(detail_html[[4]]),"\n")[[1]]
  drugs_none     <- as.numeric(gsub(",","",drugs_details[4]))
  drugs_mild     <- as.numeric(gsub(",","",drugs_details[8]))
  drugs_moderate <- as.numeric(gsub(",","",drugs_details[12]))
  drugs_severe   <- as.numeric(gsub(",","",drugs_details[16]))
  drugs_total    <- drugs_none+drugs_mild+drugs_moderate+drugs_severe
  intense_details  <- strsplit(html_text(detail_html[[5]]),"\n")[[1]]
  intense_none     <- as.numeric(gsub(",","",intense_details[4]))
  intense_mild     <- as.numeric(gsub(",","",intense_details[8]))
  intense_moderate <- as.numeric(gsub(",","",intense_details[12]))
  intense_severe   <- as.numeric(gsub(",","",intense_details[16]))
  intense_total    <- intense_none+intense_mild+intense_moderate+intense_severe
  
  guide     <- data.frame(tconst,
                          sex, sex_none, sex_mild, sex_moderate,sex_severe, sex_total,
                          violence,violence_none, violence_mild, violence_moderate,violence_severe, violence_total,
                          profanity,profanity_none, profanity_mild, profanity_moderate,profanity_severe, profanity_total,
                          drugs,drugs_none, drugs_mild, drugs_moderate,drugs_severe, drugs_total,
                          intense,intense_none, intense_mild, intense_moderate,intense_severe, intense_total,
                          mpaa,certificate,stringsAsFactors=FALSE)
  return(guide)
}

# Test movies
#guide_rip("tt0452694")
#guide_rip("tt8783930")
# Some problem movies:
#  
# guide_rip("tt0385267") 
# guide_rip("tt0183869")
# guide_rip("tt0245803")
# guide_rip("tt0417217")
#guide_rip("tt1772925") # no ratings
#guide_rip("tt2574698") # some, not all ratings
#guide_rip("tt1179782") # table is empty
#guide_rip("tt0216707")
#guide_rip("tt2404435")
#guide_rip("tt0944947")

# Movies to get parental guides
keeptypes <- c("movie","tvMovie","tvMiniSeries","tvSeries","videoGame")  # List of types to keep

movies <- basics %>%  filter(titleType %in% keeptypes) %>%
  left_join(ratings %>% select(tconst,averageRating,numVotes),by="tconst") %>%
  select(-endYear) %>%
  filter(numVotes>1000) %>% arrange(-numVotes)

movie_ids <- movies %>% select(tconst)

if (file.exists(paste0(DATA_DIR,"/parental_detail.RData"))){
  load(file=paste0(DATA_DIR,"/parental_detail.RData"))
} else {
  parental_detail <- guide_rip(movie_ids$tconst[1])  # Initialise votes data frame
}

parent_ids <- parental_detail %>% select(tconst)

movie_ids <- movie_ids %>% anti_join(parent_ids)

while(nrow(movie_ids)>0){
  # Identify movies to get guide
  print(paste("Movies to Rip :",nrow((movie_ids))))
  for (i in 1:min(nrow(movie_ids),100)){
    tryCatch({
      id <- movie_ids$tconst[i]
      movieTitle <- as.character(movies %>% filter(tconst==id) %>% select(primaryTitle))
      numVotes <- as.numeric(movies %>% filter(tconst==id) %>% select(numVotes))
      print(paste(i,"Movie",id,"votes",numVotes,movieTitle))
      parental_detail <- bind_rows(parental_detail,guide_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  parental_detail <- parental_detail %>% unique()
  save(parental_detail,file=paste0(DATA_DIR,"/parental_detail.RData"))
  parent_ids <- parental_detail %>% select(tconst)
  movie_ids <- movie_ids %>% anti_join(parent_ids)
}

parental_detail_guide <- movies %>% inner_join(parental_detail,by="tconst") %>%
  mutate(sex_code = case_when(sex=="None"~1,
                              sex=="Mild"~2,
                              sex=="Moderate"~3,
                              sex=="Severe"~4),
         violence_code = case_when(violence =="None"~1,
                                   violence =="Mild"~2,
                                   violence =="Moderate"~3,
                                   violence =="Severe"~4),
         profanity_code = case_when(profanity=="None"~1,
                                    profanity=="Mild"~2,
                                    profanity=="Moderate"~3,
                                    profanity=="Severe"~4),
         drug_code = case_when(drugs=="None"~1,
                               drugs=="Mild"~2,
                               drugs=="Moderate"~3,
                               drugs=="Severe"~4),
         intense_code = case_when(intense=="None"~1,
                                  intense=="Mild"~2,
                                  intense=="Moderate"~3,
                                  intense=="Severe"~4)
  )


save(parental_detail_guide,file=paste0(DATA_DIR,"/parental_detail_guide.Rdata"))
write.csv(parental_detail_guide,paste0(DATA_DIR,"/IMDB_parental_detail_guide.csv"),row.names = FALSE)

summary(parental_detail_guide$sex_total)
hist(parental_detail_guide$sex_total)
parental_detail_guide %>% filter(is.na(sex_total))
