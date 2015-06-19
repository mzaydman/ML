

rm(list = ls())


#package management
#install.packages("jsonlite")
#install.packages("rjson")
#install.packages("ROCR")
#install.packages("gplots")
#install.packages("gmodels")
#install.packages("tm")
library(rjson)
library(plyr)
library(jsonlite)
library(ROCR)
library(gmodels)
library(tm)

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

#normalizing zero skewed variables and creating an indicator for mixture of zeroes and non-zeroes
#twitter_corpus2$interactions.salience.content.sentiment_ln    = log(twitter_corpus2$interactions.salience.content.sentiment)          #normal with a huge spike at 0
twitter_corpus2$interactions.salience.content.sentiment_z  = ifelse(twitter_corpus2$interactions.salience.content.sentiment == 0, T, F)

twitter_corpus2$interactions.twitter.user.favourites_count_ln = log(as.numeric(twitter_corpus2$interactions.twitter.user.favourites_count)+0.00000001)       #extreme right side outliers and left skew
twitter_corpus2$interactions.twitter.user.favourites_count_z  = ifelse(as.numeric(twitter_corpus2$interactions.twitter.user.favourites_count) == 0, T, F)

twitter_corpus2$interactions.twitter.user.followers_count_ln  = log(twitter_corpus2$interactions.twitter.user.followers_count+0.00000001)        #extreme right side outliers and left skew
twitter_corpus2$interactions.twitter.user.followers_count_z  = ifelse(twitter_corpus2$interactions.twitter.user.followers_count == 0, T, F)

twitter_corpus2$interactions.twitter.user.friends_count_ln    = log(twitter_corpus2$interactions.twitter.user.friends_count+.0000000001)          #extreme right side outliers and left skew
twitter_corpus2$interactions.twitter.user.friends_count_z  = ifelse(twitter_corpus2$interactions.twitter.user.friends_count == 0, T, F)

twitter_corpus2$interactions.twitter.user.statuses_count_ln   = log(twitter_corpus2$interactions.twitter.user.statuses_count+.00000000001)         #extreme right side outliers and left skew



###split into training and test data sets###
set.seed(123) #for consistent splits

#sample observations
smp_size_r <- floor(0.75 * nrow(twitter_corpus2))
train_ind_r <- sample(seq_len(nrow(twitter_corpus2)), size = smp_size_r)

train_r <- twitter_corpus2[train_ind_r, ]
test_r <- twitter_corpus2[-train_ind_r, ]

#sample users (authors of tweets)
users = unique(twitter_corpus2$interactions.interaction.author.username)
smp_size_u = floor(0.75 * length(users))
user_sample = sample(users, smp_size_u)

train_u = twitter_corpus2[twitter_corpus2$interactions.interaction.author.username %in% user_sample, ]
test_u = twitter_corpus2[!twitter_corpus2$interactions.interaction.author.username %in% user_sample, ]


###example logit###
options(na.action = "na.exclude")
fit <- glm(outcome~interactions.klout.score+                   #normal
             interactions.salience.content.sentiment+          #normal with a huge spike at 0
             interactions.salience.content.sentiment_z+
             interactions.twitter.user.favourites_count_ln+     #skewed on 0 because of large outlier
             interactions.twitter.user.favourites_count_z+
             gender+
             interactions.twitter.user.followers_count_ln+     #skewed to 0 because of large outlier
             interactions.twitter.user.followers_count_z+ 
             interactions.twitter.user.friends_count_ln+        #skewed to 0 because of large outlier
             interactions.twitter.user.friends_count_z+
             interactions.twitter.user.statuses_count_ln+      #skewed to 0 because of large outlier
             interactions.twitter.user.geo_enabled+
             source_platform+
             links+
             hashtag
           ,
          data=train_r,family=binomial())
summary(fit)

#get predictions
results_r = test_r #a new data set to minimize chance of messing up the source data
results_r$predictions = predict(fit, newdata=test_r, type ="response") #logistic predictions
results_r$predictions_binary = ifelse(results_r$predictions > .5, T, F) #get outcome values

#generate ROC/AUC values
pred = prediction(predictions = results_r$predictions, labels = results_r$outcome)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))
abline(a=0,b=1,lwd=2,lty=2)
perf.auc = performance(pred,measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)

#KS statistic
#is KS appropriate for binaries? Wouldnt that create too many ties
ks.test(as.numeric(results_r$outcome),as.numeric(results_r$predictions_binary))


#confusion matrix
CrossTable(results_r$outcome, results_r$predictions_binary)






















#attempts to make strings do things.All fail GHAH!


#cobbering stuff with table write function
a = twitter_corpus2[, c("interactions.interaction.hashtags", "interactions.twitter.id")]

write.table(a, file = "C:\\Users\\mzaydman\\Documents\\class third year\\ML\\h4ck3d.csv", sep = ",", col.names = NA, row.names = T)
b = read.table("C:\\Users\\mzaydman\\Documents\\class third year\\ML\\h4ck3d.csv", sep = ",", header = F, fill = T)



#other failures
a$interactions.interaction.hashtags = strsplit(as.character(twitter_corpus2$interactions.interaction.hashtags),',') 


library(stringr)

b = str_split(as.character(twitter_corpus2$interactions.interaction.hashtags), ",")
d.matrix <- matrix(unlist(b), ncol = 20, byrow = TRUE) 
d.df <- as.data.frame(d.matrix) 

c = data.frame(b)

a = twitter_corpus2$interactions.interaction.hashtags
a= a[!sapply(a, is.null)]
splitdat = do.call("rbind", strsplit(as.character(a), ','))



out <- strsplit(as.character(twitter_corpus2$interactions.interaction.hashtags),',') 
blah = do.call(rbind, out)

blah = do.call(rbind, strsplit(as.character(twitter_corpus2$interactions.interaction.hashtags), ','))

b = strsplit(as.character(a), ',')

a = !is.null(twitter_corpus2$interactions.interaction.hashtags)
splitdat = do.call("rbind", strsplit(twitter_corpus2$interactions.interaction.hashtags), ","))
splitdat = data.frame(apply(splitdat, 2, as.numeric))

a = strsplit(as.character(twitter_corpus2$interactions.interaction.hashtags),',',fixed=TRUE)
c = data.frame(a)
b= data.frame(bah = unlist(a), blablah=1)
b = do.call(c, unlist(a, recursive=FALSE))

b = unlist(a, recursive=F)


