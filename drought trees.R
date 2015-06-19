

rm(list = ls())


#package management
#install.packages("jsonlite")
#install.packages("rjson")
#install.packages("ROCR")
#install.packages("gplots")
#install.packages("gmodels")
#install.packages("tm")
#install.packages("C50")
#install.packages("SparseM")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("e1071")
library(caret)
library(randomForest)
library(C50)
library(hmeasure)
library(rjson)
library(plyr)
library(jsonlite)
library(ROCR)
library(gmodels)
library(tm)
library(class)
library(xtable)


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
twitter_corpus2$outcome = ifelse(twitter_corpus2$tweet_length >= q[[4]], 1, 0 ) #indicator of being 75 percentile
twitter_corpus2$outcome = as.factor(twitter_corpus2$outcome)


#cant have logic or list structures 
twitter_corpus2$interactions.twitter.user.geo_enabled  = as.factor(twitter_corpus2$interactions.twitter.user.geo_enabled )

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



twitter_corpus3 = twitter_corpus2[,c(  "interactions.klout.score",                  
                                       "interactions.salience.content.sentiment",          

                                       "interactions.twitter.user.favourites_count",     
                 
                                       "gender",
                                       "interactions.twitter.user.followers_count",     
                                    
                                       "interactions.twitter.user.friends_count",       
                                   
                                       "interactions.twitter.user.statuses_count",     
                                       "interactions.twitter.user.geo_enabled",
                                       "source_platform",
                                       "links",
                                       "hashtag",
                                       "outcome")]

#clear out all the NAs for simplicty
#random forest handling of NAs is not great. 
twitter_corpus3 =na.omit(twitter_corpus3)

#make charecter vars explicit factors for random forest
twitter_corpus3$gender = as.factor(twitter_corpus3$gender)
twitter_corpus3$source_platform = as.factor(twitter_corpus3$source_platform)

###split into training and test data sets###
set.seed(123) #for consistent splits

#sample observations
smp_size_r <- floor(0.75 * nrow(twitter_corpus3))
train_ind_r <- sample(seq_len(nrow(twitter_corpus3)), size = smp_size_r)

train_r <- twitter_corpus3[train_ind_r, ]
test_r <- twitter_corpus3[-train_ind_r, ]

#sample users (authors of tweets)
users = unique(twitter_corpus3$interactions.interaction.author.username)
smp_size_u = floor(0.75 * length(users))
user_sample = sample(users, smp_size_u)

train_u = twitter_corpus3[twitter_corpus3$interactions.interaction.author.username %in% user_sample, ]
test_u = twitter_corpus3[!twitter_corpus3$interactions.interaction.author.username %in% user_sample, ]

#comparison a naked logistic
###example logit###
options(na.action = "na.exclude")
fit <- glm(outcome~interactions.klout.score+                   
             interactions.salience.content.sentiment+          
             interactions.twitter.user.favourites_count+     
             gender+
             interactions.twitter.user.followers_count+     
             interactions.twitter.user.friends_count+        
             interactions.twitter.user.statuses_count+      
             interactions.twitter.user.geo_enabled+
             source_platform+
             links+
             hashtag
           ,
           data=train_r,family=binomial())
summary(fit)

#get predictions

results_logistic = predict(fit, newdata=test_r, type ="response") #logistic predictions
results_logistic= ifelse(results_logistic > .5, T, F) #get outcome values
CrossTable(test_r$outcome, results_logistic, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )



#Trees
require(C50)

#create model
outcome_ref = which(colnames(train_r) %in% "outcome") #locate the outcome column
model = C5.0(train_r[-outcome_ref], train_r$outcome) 

#cream prediction
outcome_ref2 = which(colnames(test_r) %in% "outcome")
tree_predictions = predict(model, test_r[-outcome_ref2])

#look at results table
CrossTable(test_r$outcome, tree_predictions, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )

#boosted
model10 = C5.0(train_r[-outcome_ref], train_r$outcome, trials = 10)

btree_predictions = predict(model10, test_r[-outcome_ref2])

CrossTable(test_r$outcome, btree_predictions, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )

#adjusted costs 
#minimize false negatives
error_costs = matrix(c(0,1,3,0), nrow=2)

model_costs = C5.0(train_r[-outcome_ref], train_r$outcome, costs = error_costs)

cost_predictions = predict(model_costs, test_r[-outcome_ref2])

CrossTable(test_r$outcome, cost_predictions, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )


#forests
require(randomForest)
#make sure to make everything not numeric a factor, random forest does not make that assumption on it's own
#also rF does not play nice for NAs, I just dropped them for expediancy

model_forest = randomForest( outcome ~ . , data = train_r)

forest_prediction = predict(model_forest, test_r[-outcome_ref2])
CrossTable(test_r$outcome, forest_prediction, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )

#checking caret
m = train(outcome ~., data = train_r, method = "C5.0")
precition_caret = predict(m, test_r[-outcome_ref2])
CrossTable(test_r$outcome, precition_caret, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual length', 'predicted length') )
