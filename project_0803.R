##################### DATA CLEANING - PART 2 ##################### 

setwd("C:/Users/strak/Desktop/MAE Spring Semester/MLT/Project/youtube-new")

install_and_load = function(name, char = T){
  if (!require(name, character.only = char)) {
    install.packages(name)
  }
  require(name, character.only = char)
}

sapply(
  c("data.table","tidyverse","magrittr",
    "readxl", "class", "dplyr","tidyr","cluster", "caret", "rjson", "Matrix"),
  install_and_load
)

rm(install_and_load)

#loading the data obtained from kaggle.com

metadata <- fromJSON(file = "GB_category_id.json")
print(metadata)

mydata <- read.csv("GBvideos.csv",stringsAsFactors = FALSE)
str(mydata)

# renaming category_id based on metadata
glimpse(mydata)
str(mydata$category_id) 
mydata$category_id = as.character(mydata$category_id)  
inds = which(mydata$category_id == "43")
mydata$category_id[inds] = "Shows"
unique(mydata$category_id)

# to lower letters in tags column
mydata01 <- mydata %>% select(tags)

for (i in 1:38916){
  mydata01[i,] <- tolower(mydata01[i,])  
}

# separate each tag into unique column
mydata <- mydata %>% select(video_id, title, publish_time, category_id, views, likes, dislikes, comment_count, comments_disabled, ratings_disabled, video_error_or_removed, tags)
mydata01 <- mydata01 %>% separate(tags, c("0", 1:99), sep = "[|]", remove = TRUE, convert = FALSE, extra = "merge", fill = "right")

# remove empty columns
mydata01 <- mydata01[ -c(50:100) ]
str(mydata01)

# merging simular values (tags) like music - musics or celebrities - celebrity
for (i in 1:38916	){
  for (j in 1:length(mydata01)){
    mydata01[i,j] <- gsub("celebrities","celebrity",mydata01[i,j]) 
  }
}

# find 30 most frequently used KWs
Top_KWs <- sort(table(unlist(as.data.frame(mydata01))), TRUE)[1:32] %>%
      print()

# create 30 new columns for dummies
mydata$funny <- 0
mydata$comedy <- 0
mydata$music <- 0
mydata$pop <- 0
mydata$none <- 0
mydata$rap <- 0
mydata$music_video <- 0
mydata$official <- 0
mydata$trailer <- 0
mydata$hip-hop <- 0
mydata$year_18 <- 0
mydata$interview <- 0
mydata$video <- 0
mydata$live <- 0
mydata$records <- 0
mydata$humor <- 0
mydata$celebrity <- 0
mydata$movie <- 0
mydata$vlog_official_video <- 0
mydata$film <- 0
mydata$rap <- 0
mydata$television <- 0
mydata$hollywood <- 0
mydata$comedian <- 0
mydata$new <- 0
mydata$the <- 0
mydata$entertainment <- 0
mydata$talk_show <- 0
mydata$star_wars <- 0
mydata$fashion <- 0
mydata$year_17 <- 0

# filling dummy columns 
mydataAH=mydata01
mydataAH[is.na(mydataAH)] = 0

for (i in 1:38916)
  {
  for (j in 1:length(mydataAH))
  { 
    if (mydataAH[i,j] == "funny")
      mydata$funny[i] <- 1
  }
   }