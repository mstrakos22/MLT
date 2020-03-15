##################### DATA CLEANING ##################### 
# Michal Strakos
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
mydata02 <- mydata %>% select(tags)

for (i in 1:38916){
  mydata02[i,] <- tolower(mydata02[i,])  
}

# separate each tag into unique column
mydata <- mydata %>% select(video_id, title, publish_time, category_id, views, likes, dislikes, comment_count, comments_disabled, ratings_disabled, video_error_or_removed, tags)
mydata02 <- mydata02 %>% separate(tags, c("0", 1:99), sep = "[|]", remove = TRUE, convert = FALSE, extra = "merge", fill = "right")

# remove empty columns
mydata02 <- mydata02[ -c(50:100) ]
str(mydata02)

# find 30 most frequently used KWs
Top_KWs <- sort(table(unlist(as.data.frame(mydata02))), TRUE)[1:34] %>%
  print()

mydata02[is.na(mydata02)] <- 0

# merging simular values (tags) like  celebrities - celebrity
for (i in 1:38916	){
  for (j in 1:49){
    mydata02[i,j] <- gsub("\"celebrities\"","\"celebrity\"",mydata02[i,j]) 
    
  }
}

# merging simular values (tags) like music video - music
for (i in 1:38916	){
  for (j in 1:49){
    mydata02[i,j] <- gsub("\"music video\"","\"music\"",mydata02[i,j]) 
    
  }
}

# create 30 new columns for dummies
mydata02$funny <- 0
mydata02$comedy <- 0
mydata02$pop <- 0
mydata02$rap <- 0
mydata02$music_video <- 0
mydata02$official <- 0
mydata02$trailer <- 0
mydata02$year_18 <- 0
mydata02$interview <- 0
mydata02$video <- 0
mydata02$live <- 0
mydata02$records <- 0
mydata02$humor <- 0
mydata02$celebrities <- 0
mydata02$movie <- 0
mydata02$vlog <- 0
mydata02$official_video <- 0
mydata02$film <- 0
mydata02$television <- 0
mydata02$hollywood <- 0
mydata02$comedian <- 0
mydata02$new <- 0
mydata02$the <- 0
mydata02$entertainment <- 0
mydata02$talk_show <- 0
mydata02$star_wars <- 0
mydata02$fashion <- 0
mydata02$year_17 <- 0
mydata02$late_night <- 0
mydata02$news <- 0

# filling 30 new columns with dummies

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"funny\"")    #1
      mydata02$funny[i] <- 1 
    if(mydata02[i,j] == "\"comedy\"")   
      mydata02$comedy[i] <- 1   
    
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"music\"")  #2
      mydata02$music_video[i] <- 1  
    if(mydata02[i,j] == "\"pop\"")
      mydata02$pop[i] <- 1   
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"rap\"")      #3
      mydata02$rap[i] <- 1 
    if(mydata02[i,j] == "\"2018\"")   
      mydata02$year_18[i] <- 1 
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"official\"")   #4
      mydata02$official[i] <- 1 
    if(mydata02[i,j] == "\"trailer\"")
      mydata02$trailer[i] <- 1 
  }
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"interview\"")   #5
      mydata02$interview[i] <- 1 
    if(mydata02[i,j] == "\"video\"")
      mydata02$video[i] <- 1
  }
}  

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"live\"")   #6
      mydata02$live[i] <- 1 
    if(mydata02[i,j] == "\"records\"")
      mydata02$records[i] <- 1 
  }
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"humor\"")   #7
      mydata02$humor[i] <- 1 
    if(mydata02[i,j] == "\"celebrity\"")
      mydata02$celebrities[i] <- 1
  }
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"movie\"")   #8
      mydata02$movie[i] <- 1 
    if(mydata02[i,j] == "\"vlog\"")
      mydata02$vlog[i] <- 1  
  }
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"official video\"")       #9
      mydata02$official_video[i] <- 1  
    if(mydata02[i,j] == "\"film\"")
      mydata02$film[i] <- 1 
  }
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"television\"")    #10
      mydata02$television[i] <- 1  
    if(mydata02[i,j] == "\"hollywood\"")
      mydata02$hollywood[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"comedian\"")   #11
      mydata02$comedian[i] <- 1  
    if(mydata02[i,j] == "\"new\"")
      mydata02$new[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"the\"")      #12
      mydata02$the[i] <- 1  
    if(mydata02[i,j] == "\"entertainment\"")
      mydata02$entertainment[i] <- 1 
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"talk show\"")  #13
      mydata02$talk_show[i] <- 1 
    if(mydata02[i,j] == "\"star wars\"")
      mydata02$star_wars[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"fashion\"")  #14
      mydata02$fashion[i] <- 1  
    if(mydata02[i,j] == "\"2017\"")
      mydata02$year_17[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"news\"")    #15
      mydata02$news[i] <- 1  
    if(mydata02[i,j] == "\"late night\"")
      mydata02$late_night[i] <- 1  
  }
}



dumies <- subset(mydata02, select=c("funny", "comedy", "pop", "rap", "music_video", "official", "trailer", "year_18", "interview", "video", "live", "records", "humor", "celebrities", "movie", "vlog", "official_video", "film", "television", "hollywood", "comedian", "new", "the", "entertainment", "talk_show", "star_wars", "fashion", "year_17", "news", "late_night"))
req_var <- subset(mydata, select=c( "category_id", "views", "likes", "dislikes", "comment_count", "comments_disabled", "ratings_disabled", "video_error_or_removed"))

model_data <- cbind(req_var,dumies)
model_data <- as.data.frame(model_data)

##############################################################################

####################### ANALYSIS PART - 1. Naive Baise ################################
library (naivebayes)

#Spliting training set into two parts based on outcome: 75% and 25%
smp_siz = floor(0.75*nrow(model_data)) 
index = sample(seq_len(nrow(model_data)),size = smp_siz)  
trainSet <- model_data[ index,]
testSet <-  model_data[-index,]


#Naive Bayes Model
model <- naive_bayes(category_id ~.,data= trainSet)
model
str(model)

# visualization of model
plot(model, n = 1000, legendplot = TRUE, lty, col,
       ylab = "Density", main = "Naive Bayes Plot", ...)

par(mfrow=c(2,2))
plot(model)
str(model)

#prediction of model
p <- predict(model, newdata=testSet)
head(cbind(p,testSet))



#confusion matrix - test data
p1 <- predict(model,testSet)
(tab1 <- table(p1, testSet$category_id))
1 - sum(diag(tab1))/sum(tab1)

#confusion matrix - train data
p2 <- predict(model,trainSet)
(tab2 <- table(p2, trainSet$category_id))
1 - sum(diag(tab2))/sum(tab2)

##################################################################################

##################### ANALYSIS PART - 2.knn mode ########################
library (naivebayes)

#Spliting training set into two parts based on outcome: 75% and 25%
smp_siz = floor(0.75*nrow(model_data)) 
index = sample(seq_len(nrow(model_data)),size = smp_siz)  
trainSet <- model_data[ index,]
testSet <-  model_data[-index,]


#Naive Bayes Model
model <- naive_bayes(category_id ~.,data= trainSet)
model

# visualization of model
plot(model, n = 1000, legendplot = TRUE, lty, col,
        ylab = "Density", main = "Naive Bayes Plot", ...)

par(mfrow=c(2,2))
plot(model)
str(model)

#prediction of model
p <- predict(model, newdata=testSet)
head(cbind(p,testSet))



#confusion matrix - test data
p1 <- predict(model,testSet)
(tab1 <- table(p1, testSet$category_id))
1 - sum(diag(tab1))/sum(tab1)

#confusion matrix - train data
p2 <- predict(model,trainSet)
(tab2 <- table(p2, trainSet$category_id))
1 - sum(diag(tab2))/sum(tab2)

##################################################################################

##################### knn mode ########################
#install.packages("class")
library(class)

outcome <- data.frame (model_data$category_id)

model_data$category_id <- NULL

smp_siz = floor(0.75*nrow(outcome)) 
index = sample(seq_len(nrow(model_data)),size = smp_siz)  
trainSet <- model_data[ index,]
testSet <-  model_data[-index,]

trainSet_outcome <- outcome[ index,]
testSet_outcome <-  outcome[-index,]


#Training the knn model

knn_model <- knn(train= trainSet,test= testSet,cl=trainSet_outcome ,k=17)
knn_model


# visualization of model
plot(knn_model)

# put in a data frame
testSet_outcome <- data.frame(testSet_outcome)

# merge "mjob_pred_knn" and "mjob_outcome_test" 
class_comparison <- data.frame(knn_model, testSet_outcome)

# specify column names for "class_comparison"
names(class_comparison) <- c("Predicted", "Observed")

# inspect "class_comparison" 
head(class_comparison)


####### accoring to result KNN is best for this data set    if(mydata02[i,j] == "\"film\"")
mydata02$film[i] <- 1 
}
} 

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"television\"")    #10
      mydata02$television[i] <- 1  
    if(mydata02[i,j] == "\"hollywood\"")
      mydata02$hollywood[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"comedian\"")   #11
      mydata02$comedian[i] <- 1  
    if(mydata02[i,j] == "\"new\"")
      mydata02$new[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"the\"")      #12
      mydata02$the[i] <- 1  
    if(mydata02[i,j] == "\"entertainment\"")
      mydata02$entertainment[i] <- 1 
  }
}

for (i in 1:38916) {
  for (j in 1:48){
    if(mydata02[i,j] == "\"talk show\"")  #13
      mydata02$talk_show[i] <- 1 
    if(mydata02[i,j] == "\"star wars\"")
      mydata02$star_wars[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"fashion\"")  #14
      mydata02$fashion[i] <- 1  
    if(mydata02[i,j] == "\"2017\"")
      mydata02$year_17[i] <- 1  
  }
}

for (i in 1:38916) {
  for (j in 1:49){
    if(mydata02[i,j] == "\"news\"")    #15
      mydata02$news[i] <- 1  
    if(mydata02[i,j] == "\"late night\"")
      mydata02$late_night[i] <- 1  
  }
}



dumies <- subset(mydata02, select=c("funny", "comedy", "pop", "rap", "music_video", "official", "trailer", "year_18", "interview", "video", "live", "records", "humor", "celebrities", "movie", "vlog", "official_video", "film", "television", "hollywood", "comedian", "new", "the", "entertainment", "talk_show", "star_wars", "fashion", "year_17", "news", "late_night"))
req_var <- subset(mydata, select=c( "category_id", "views", "likes", "dislikes", "comment_count", "comments_disabled", "ratings_disabled", "video_error_or_removed"))

model_data <- cbind(req_var,dumies)
model_data <- as.data.frame(model_data)

##############################################################################

####################### Naive Baise ################################
library (naivebayes)

#Spliting training set into two parts based on outcome: 75% and 25%
smp_siz = floor(0.75*nrow(model_data)) 
index = sample(seq_len(nrow(model_data)),size = smp_siz)  
trainSet <- model_data[ index,]
testSet <-  model_data[-index,]


#Naive Bayes Model
model <- naive_bayes(category_id ~.,data= trainSet)
model



######## visualization of model
# plot(model, n = 1000, legendplot = TRUE, lty, col,
#        ylab = "Density", main = "Naive Bayes Plot", ...)

par(mfrow=c(2,2))
plot(model)
str(model)

#prediction of model
p <- predict(model, newdata=testSet)
head(cbind(p,testSet))



#confusion matrix - test data
p1 <- predict(model,testSet)
(tab1 <- table(p1, testSet$category_id))
1 - sum(diag(tab1))/sum(tab1)

#confusion matrix - train data
p2 <- predict(model,trainSet)
(tab2 <- table(p2, trainSet$category_id))
1 - sum(diag(tab2))/sum(tab2)

##################################################################################

##################### knn mode ########################
#install.packages("class")
library(class)

outcome <- data.frame (model_data$category_id)

model_data$category_id <- NULL

smp_siz = floor(0.75*nrow(outcome)) 
index = sample(seq_len(nrow(model_data)),size = smp_siz)  
trainSet <- model_data[ index,]
testSet <-  model_data[-index,]

trainSet_outcome <- outcome[ index,]
testSet_outcome <-  outcome[-index,]


#Training the knn model

knn_model <- knn(train= trainSet,test= testSet,cl=trainSet_outcome ,k=17)
knn_model