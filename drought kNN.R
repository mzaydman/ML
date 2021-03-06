

rm(list = ls())


#package management
#install.packages("jsonlite")
#install.packages("rjson")
#install.packages("ROCR")
#install.packages("gplots")
#install.packages("gmodels")
#install.packages("tm")
#install.packages("class")
#install.packages("xtable")
#install.packages("hmeasure")
library(hmeasure)
library(rjson)
library(plyr)
library(jsonlite)
library(ROCR)
library(gmodels)
library(tm)
library(class)
library(xtable)
require(rjson)


###creating a dataset###
setwd("C:\\Users\\mzaydman\\Documents\\class third year\\ML\\Twitter data\\California Drought - Full Historic Query JSON Data") #source directory

#identify all JSONS
files = list.files()

require(rjson) #using this library to import JSONs

#flatten and collate all JSONS
twitter_corpus = data.frame()
for(file in files){
  temp = data.frame(fromJSON(file, flatten = T))
  twitter_corpus = rbind.fill(twitter_corpus,temp)
}

#select more relevant features
twitter_corpus2 = twitter_corpus[,c(4,6,8,12, 14, 16, 19, 20, 22, 23, 24, 28, 31, 34, 41, 42, 43, 44, 45, 54, 67, 68, 73, 74, 76)]

#cleaning the text, poorly
twitter_corpus2$interactions.interaction.content <- sapply(twitter_corpus2$interactions.interaction.content,function(row) iconv(row, "UTF-8", "ASCII", sub="")) #dealing with ecoding issues
twitter_corpus2$interactions.interaction.content = gsub("&amp","",twitter_corpus2$interactions.interaction.content) #changing the encoding still leaves artifacts
twitter_corpus2$interactions.interaction.content = gsub("&gt;","",twitter_corpus2$interactions.interaction.content) #ibid. 

#clean text
#corpus <- Corpus(VectorSource(twitter_corpus2$interactions.interaction.content))

# Remove all the numbers
#corpus_clean <- tm_map(corpus, removeNumbers)
# Remove punctuation
#corpus_clean <- tm_map(corpus_clean, removePunctuation)



hashtags<-twitter_corpus$interactions.twitter.hashtags
names(hashtags)<-twitter_corpus$  interactions.twitter.id
head(hashtags)
test<-as.data.frame(as.matrix(unlist(hashtags)),stringsAsFactors=F)
test$real.name<-vector(mode="character",length=nrow(test))
head(test)
### This loop would be more elegant if I wrote it into a function and used 'apply' instead.  In really large data we would have to do that, but here we don't so I'm going to be lazy and loop it.
for (i in 1:nrow(test)) {
  test$real.name[i]<-substr(row.names(test)[i],1,20)
}
head(test)










###constructed variables###


#outcome for long tweets
twitter_corpus2$tweet_length = nchar(twitter_corpus2$interactions.interaction.content) #length of content
q = quantile(twitter_corpus2$tweet_length) #quartiles for length of tweet
twitter_corpus2$outcome = ifelse(twitter_corpus2$tweet_length >= q[[4]], T, F ) #indicator of being 75 percentile

#indicator of geotagged
twitter_corpus2$geo_tagged = ifelse(is.na(twitter_corpus2$interactions.interaction.geo.latitude), 0, 1)

#indicator of having a link
twitter_corpus2$links = ifelse(as.character(twitter_corpus2$interactions.twitter.domains) == "NULL", 0, 1)

#gender tag accounting for NAs
twitter_corpus2$gender = ifelse(is.na(twitter_corpus2$interactions.demographic.gender), "unknown", twitter_corpus2$interactions.demographic.gender)

#presence of a hashtag
twitter_corpus2$hashtag = ifelse(as.character(twitter_corpus2$interactions.interaction.hashtags) == "NULL", 0, 1)


#tweet source constructed variables
#top 10 sources, making up ~82% of all sources, ranging from ~25% to ~3%
#anything outside of the top 10 gets called other
main_platforms = c("Twitter for iPhone", "Twitter Web Client", 
                   "Twitter for Android", "Twitter for Websites", 
                   "Hootsuite", "Facebook", 
                   "Instagram", "twitterfeed", 
                   "dlvr.it", "TweetDeck")

twitter_corpus2$source_platform = ifelse(twitter_corpus2$interactions.interaction.source %in% main_platforms, twitter_corpus2$interactions.interaction.source, "other")
twitter_corpus2$interactions.salience.content.sentiment_z  = ifelse(twitter_corpus2$interactions.salience.content.sentiment == 0, 1, 0)
twitter_corpus2$interactions.twitter.user.favourites_count_z  = ifelse(as.numeric(twitter_corpus2$interactions.twitter.user.favourites_count) == 0, 1, 0)
twitter_corpus2$interactions.twitter.user.followers_count_z  = ifelse(twitter_corpus2$interactions.twitter.user.followers_count == 0, 1, 0)
twitter_corpus2$interactions.twitter.user.friends_count_z  = ifelse(twitter_corpus2$interactions.twitter.user.friends_count == 0, 1, 0)



twitter_corpus3 = twitter_corpus2[,c(  "interactions.klout.score",                  
                                       "interactions.salience.content.sentiment",          
                                       "interactions.salience.content.sentiment_z",
                                       "interactions.twitter.user.favourites_count",     
                                       "interactions.twitter.user.favourites_count_z",
                                       "gender",
                                       "interactions.twitter.user.followers_count",     
                                       "interactions.twitter.user.followers_count_z", 
                                       "interactions.twitter.user.friends_count",       
                                       "interactions.twitter.user.friends_count_z",
                                       "interactions.twitter.user.statuses_count",     
                                       "interactions.twitter.user.geo_enabled",
                                       "source_platform",
                                       "links",
                                       "hashtag",
                                       "outcome")]


#converting factors into binaries
a = model.matrix( ~ source_platform - 1, data=twitter_corpus3 )
b = model.matrix( ~ gender - 1, data=twitter_corpus3 )

twitter_corpus3 = subset(twitter_corpus3, select=-c(source_platform,gender)) #dropping the categorical versions
twitter_corpus4 = cbind(twitter_corpus3, a, b) #combining data sets 




#fancy way of doing this with lapply which is actually messier than single line commands
#twitter_corpus4[c("interactions.klout.score",
#                  "interactions.salience.content.sentiment", 
#                  "interactions.twitter.user.favourites_count",
#                  "interactions.twitter.user.followers_count",
#                  "interactions.twitter.user.friends_count",
#                  "interactions.twitter.user.statuses_count")] = lapply(twitter_corpus4[c("interactions.klout.score",
#                                                                                        "interactions.salience.content.sentiment", 
#                                                                                        "interactions.twitter.user.favourites_count",
#                                                                                        "interactions.twitter.user.followers_count",
#                                                                                        "interactions.twitter.user.friends_count",
#                                                                                        "interactions.twitter.user.statuses_count")], function(x) scale(x,center = T, scale = T) )

#z-transform numerics to allow KNN distance to be acuretely computed. Outliers are still preserved (not sure if this is good or not)
#lapply looks too ugly
twitter_corpus4$interactions.klout.score  = scale(twitter_corpus4$interactions.klout.score, center = TRUE, scale = TRUE)
twitter_corpus4$interactions.salience.content.sentiment = scale(twitter_corpus4$interactions.salience.content.sentiment, center = TRUE, scale = TRUE)
twitter_corpus4$interactions.twitter.user.favourites_count = scale(twitter_corpus4$interactions.twitter.user.favourites_count, center = TRUE, scale = TRUE)                                     
twitter_corpus4$interactions.twitter.user.followers_count = scale(twitter_corpus4$interactions.twitter.user.followers_count, center = TRUE, scale = TRUE)  
twitter_corpus4$interactions.twitter.user.friends_count = scale(twitter_corpus4$interactions.twitter.user.friends_count, center = TRUE, scale = TRUE)
twitter_corpus4$interactions.twitter.user.statuses_count = scale(twitter_corpus4$interactions.twitter.user.statuses_count, center = TRUE, scale = TRUE)

#kNN does not handle NA values, so for now NAs are dropped. 

twitter_corpus4 = na.omit(twitter_corpus3)

###split into training and test data sets###
#splitting the data, only using individual tweets for the sake of simplicity as KNN has strict requirments of df structure 

set.seed(123) #for consistent splits

#sample observations
smp_size_r <- floor(0.75 * nrow(twitter_corpus4))
train_ind_r <- sample(seq_len(nrow(twitter_corpus4)), size = smp_size_r)

train_r <- twitter_corpus4[train_ind_r, ]
test_r <- twitter_corpus4[-train_ind_r, ]


####moving onto the kNN!
require(class)

#strip out the label/dependent/outcome variable
train_r_label = train_r$outcome
test_r_label = test_r$outcome

train_r = subset(train_r, select= -c(outcome))
test_r = subset(test_r, select= -c(outcome))

#defining k using the sqrt rule of thumb
k = sqrt(nrow(train_r))

#using kNN predication
p = knn(train = train_r,test = test_r, cl = train_r_label, k = 11, prob = T, use.all = T)

#looking at performance
CrossTable(x = test_r_label, y = p, propr.chisq = F)

#ROCs using libary(hmeasure)

require(hmeasure)

scores.knn <- attr(p,"prob") #getting the probability measure from the kNN prediction
scores.knn[p==F] <- 1-scores.knn[p==F]
results = HMeasure(test_r_label, scores.knn)
plotROC(results) #ROC curve
summary(results) #view AUC
