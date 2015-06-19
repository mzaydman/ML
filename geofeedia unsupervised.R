#########################################################################################
##working with data pulled from GeoFeedia 
##project analyzing the coordination of agencies post hurricane Sandy
##this work is an exploration of using social media data to an organizational project
#########################################################################################


rm(list = ls())

library(plyr)
library(gmodels)


###creating a dataset###

#bronx
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Bronx") #source directory
files = list.files() #files in directory

Bronx = data.frame() #dataframe to store data

for(file in files){  #loop over all files, skip the file description up top, slow but skip doesnt work as non standard column numnbers
  temp = read.csv(text=readLines(file)[-(1:5)], stringsAsFactors = F)
  Bronx = rbind.fill(Bronx,temp)
}


#Brooklyn
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Brooklyn") #source directory
files = list.files()    #files in directory

Brooklyn = data.frame() #dataframe to store data

for(file in files){     #loop over all files, skip the file description up top, slow but skip doesnt work as non standard column numnbers
  temp = read.csv(text=readLines(file)[-(1:5)], stringsAsFactors = F)
  Brooklyn = rbind.fill(Brooklyn,temp)
}

#Manhattan
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Manhattan") #source directory
files = list.files()     #files in directory

Manhattan = data.frame() #dataframe to store data

for(file in files){      #loop over all files, skip the file description up top, slow but skip doesnt work as non standard column numnbers
  temp = read.csv(text=readLines(file)[-(1:5)], stringsAsFactors = F)
  Manhattan = rbind.fill(Manhattan,temp)
}

#Queens
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Queens") #source directory
files = list.files()  #files in directory

Queens = data.frame() #dataframe to store data

for(file in files){   #loop over all files, skip the file description up top, slow but skip doesnt work as non standard column numnbers
  temp = read.csv(text=readLines(file)[-(1:5)], stringsAsFactors = F)
  Queens = rbind.fill(Queens,temp)
}

#Staten Island
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Staten Island") #source directory
files = list.files()        #files in directory

StatenIsland = data.frame() #dataframe to store data

for(file in files){         #loop over all files, skip the file description up top, slow but skip doesnt work as non standard column numnbers
  temp = read.csv(text=readLines(file)[-(1:5)], stringsAsFactors = F)
  StatenIsland = rbind.fill(StatenIsland,temp)
}

setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data")
save.image() 

Bronx$Borough = 'Bronx'
Brooklyn$Borough = 'Brooklyn'
Manhattan$Borough = 'Manhattan'
Queens$Borough = 'Queens'
StatenIsland$Borough = 'StatenIsland'

#create master file
GeoFeediaData = data.frame()
GeoFeediaData = rbind.fill(GeoFeediaData, Bronx)
GeoFeediaData = rbind.fill(GeoFeediaData, Brooklyn)
GeoFeediaData = rbind.fill(GeoFeediaData, Manhattan)
GeoFeediaData = rbind.fill(GeoFeediaData, Queens)
GeoFeediaData = rbind.fill(GeoFeediaData, StatenIsland)

GeoFeediaData$Title <- sapply(GeoFeediaData$Title,function(row) iconv(row, "UTF-8", "ASCII", sub="")) #dealing with ecoding issues
GeoFeediaData$Title = gsub("&amp","",GeoFeediaData$Title) #changing the encoding still leaves artifacts
GeoFeediaData$Title = gsub("&gt;","",GeoFeediaData$Title) #ibid. 
GeoFeediaData$Title = tolower(GeoFeediaData$Title) #lower case for searches


GeoFeediaData$PublishDateUTC.Hour = as.Date(GeoFeediaData$PublishDateUTC.Hour)


#get a date variable
GeoFeediaData$PublishDateUTC.Day2 = substr(GeoFeediaData$PublishDateUTC.Day, 3 , nchar(GeoFeediaData$PublishDateUTC.Day)-1  )
GeoFeediaData$PublishDateUTC.Day3 = as.Date(GeoFeediaData$PublishDateUTC.Day2, "%m/%d/%Y")


#creating data sets for specific mention categories
toMatch = "sandy|hurricane|storm"
newdata_Sandy <- GeoFeediaData[ which(
                                  GeoFeediaData$Source == 'twitter' &
                                  grepl(toMatch, GeoFeediaData$Title) == T ), ]
                          
toMatch = "recovery|repair|restore|assistance|rebuild|resilience"
newdata_Relief <- GeoFeediaData[ which(
                                  GeoFeediaData$Source == 'twitter' &
                                  grepl(toMatch, GeoFeediaData$Title) == T ), ]

toMatch = "closure|surge|flood|destruction|damage|outage|aftermath|injury|catastrophe"
newdata_Damage <- GeoFeediaData[ which(
                                         GeoFeediaData$Source == 'twitter' &
                                         grepl(toMatch, GeoFeediaData$Title) == T ), ]



#create data for the overall peak storm time
newdata_Peak_Storm <- subset(GeoFeediaData, PublishDateUTC.Day3 > as.Date("2012-10-28") & PublishDateUTC.Day3 < as.Date("2012-11-1") )




setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data")
save.image() #data cleaning complete

############################################################################
##exploratory analytics
############################################################################



#a view of how the mentions appear over time
plot(count(newdata_Relief$Title), newdata_Sandy$PublishDateUTC.Day3)
hist(newdata_Sandy$PublishDateUTC.Day3, breaks = 22, main= "Distribution of Sandy Mentions Over Time", xlab = "Date (storm hits 10-29)")
hist(newdata_Relief$PublishDateUTC.Day3, breaks = 22, main= "Distribution of Relief Mentions Over Time", xlab = "Date (storm hits 10-29)")
hist(newdata_Damage$PublishDateUTC.Day3, breaks = 22, main= "Distribution of Damage Mentions Over Time", xlab = "Date (storm hits 10-29)")



#summary metrics
CrossTable(x = newdata_Sandy$Borough, y = newdata_Sandy$Source)
CrossTable(x = newdata_Relief$Borough, y = newdata_Relief$Source)
CrossTable(x = newdata_Damage$Borough, y = newdata_Damage$Source)




#sentiment analysis
#install.packages("qdap")
library(qdap)
library(data.table)
#install.packages("wordcloud")
#install.packages("SnowballC")
#install.packages("slam")
library(wordcloud)
library(SnowballC)
library(slam)

#wordclouds
wordcloud(newdata_Relief$Title, max.words = 100)

require(tm)
############################################
##creating a corpi
############################################

#########the relief corpus##################
corpus = Corpus(VectorSource(newdata_Relief$Title))

#cleaning the corpus using built in tm functions
clean_corp = tm_map(corpus, removeWords, stopwords("english"))
clean_corp = tm_map(clean_corp, removeWords, c("recovery","repair","restore","assistance","rebuild","resilience"))
clean_corp = tm_map(clean_corp, stripWhitespace)
clean_corp = tm_map(clean_corp, removePunctuation)
clean_corp = tm_map(clean_corp, stemDocument)
clean_corp = tm_map(clean_corp, removeWords, "restor") #too many variations on restore in document so clean stemmed
clean_corp = tm_map(clean_corp, removeNumbers)

#custom function using tm() to strip URLs
removeURLs = function(x) gsub("http(.*)", "", x) #remove urls, custom function
clean_corp = tm_map(clean_corp, removeURLs)

#########the Sandy corpus##################
corpus_S = Corpus(VectorSource(newdata_Sandy$Title))

#cleaning the corpus using built in tm functions
clean_corp_S = tm_map(corpus_S, removeWords, stopwords("english"))
clean_corp_S = tm_map(clean_corp_S, removeWords, c("sandy","hurricane","storm"))
clean_corp_S = tm_map(clean_corp_S, stripWhitespace)
clean_corp_S = tm_map(clean_corp_S, removePunctuation)
clean_corp_S = tm_map(clean_corp_S, stemDocument)
#clean_corp_S = tm_map(clean_corp_S, removeWords, "restor") #too many variations on restore in document so clean stemmed
clean_corp_S = tm_map(clean_corp_S, removeNumbers)

#custom function using tm() to strip URLs
removeURLs = function(x) gsub("http(.*)", "", x) #remove urls, custom function
clean_corp_S = tm_map(clean_corp_S, removeURLs)






#########the Damage corpus##################
corpus_D = Corpus(VectorSource(newdata_Damage$Title))

#cleaning the corpus using built in tm functions
clean_corp_D = tm_map(corpus_D, removeWords, stopwords("english"))
clean_corp_D = tm_map(clean_corp_D, removeWords, c("closure","surge","flood","destruction","damage","outage","aftermath","injury","catastrophe"))
clean_corp_D = tm_map(clean_corp_D, stripWhitespace)
clean_corp_D = tm_map(clean_corp_D, removePunctuation)
clean_corp_D = tm_map(clean_corp_D, stemDocument)
clean_corp_D = tm_map(clean_corp_D, removeWords, "flood") #too many variations on restore in document so clean stemmed
clean_corp_D = tm_map(clean_corp_D, removeNumbers)

#custom function using tm() to strip URLs
removeURLs = function(x) gsub("http(.*)", "", x) #remove urls, custom function
clean_corp_D = tm_map(clean_corp_D, removeURLs)


############documents from the peak storm time###############################

corpus_P = Corpus(VectorSource(newdata_Peak_Storm$Title))

#cleaning the corpus using built in tm functions
clean_corp_P = tm_map(corpus_P, removeWords, stopwords("english"))

#clean_corp_D = tm_map(clean_corp_D, removeWords, c("closure","surge","flood","destruction","damage","outage","aftermath","injury","catastrophe"))
clean_corp_P = tm_map(clean_corp_P, stripWhitespace)
clean_corp_P = tm_map(clean_corp_P, removePunctuation)
clean_corp_P = tm_map(clean_corp_P, stemDocument)
#clean_corp_D = tm_map(clean_corp_D, removeWords, "flood") #too many variations on restore in document so clean stemmed
clean_corp_P = tm_map(clean_corp_P, removeNumbers)

#custom function using tm() to strip URLs
removeURLs = function(x) gsub("http(.*)", "", x) #remove urls, custom function
clean_corp_P = tm_map(clean_corp_P, removeURLs)



wordcloud(clean_corp, scale=c(5,0.5), max.words=75, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(clean_corp_S, scale=c(5,0.5), max.words=75, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(clean_corp_D, scale=c(5,0.5), max.words=75, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))



setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data")
save.image()


#create matrix of words



dtm = DocumentTermMatrix(clean_corp)
dtm2 = removeSparseTerms(dtm, sparse = .95)  #try to compare frequencies to baseline english corpus frequencies #bionj
#dtm_df_idf = weightTfIdf(dtm2) #inverse weighting, gives greater weight to a term if it appears frequently in the tweet
                               #adjusted for frequently occouring terms in the text overall
m = as.matrix(dtm2)
hist(colSums(m), main = "sums of my frequency matrix")




a = model.matrix( ~ Borough - 1, data=newdata_Relief )
m2 = cbind( m, as.numeric(newdata_Relief$X))
m2 = cbind( m2, as.numeric(newdata_Relief$Y))
m2 = cbind( m2, a)
m2 = cbind( m2, as.numeric(newdata_Relief$PublishDateUTC.Day3))

#m2 = scale(m2)

rownames(m2) = 1:nrow(m2)

#norm_eucl = function(m)
#  m/apply(m, 1, function(x) sum(x^2)^.5)  #maybe not necessary, could try to not to transform

#m_norm = norm_eucl(m)

#m_norm = m_norm[complete.cases(m_norm), ]
#m = m[complete.cases(m), ]
m2 = m2[complete.cases(m2), ]


#normalize
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
m2 = apply(m2, 2, range01)


results =  kmeans(m, 12, 30)

round(results$centers, digits = 3) # cluster centers

clusters <- 1:12
for (i in clusters) {
  cat("Cluster " , i, ":", findFreqTerms(dtm_df_idf[results[['cluster']]==i,], 50),"\n\n")
}

inspect(clean_corp[which(results$cluster==11)])


########pamk
#install.packages("fpc")
#install.packages("diptest")
library(diptest)
library(fpc)

pamk.ch2 = pamk(m2, criterion = "ch", metric="manhattan")


pam.ch2.result <- pamk.ch2$pamobject
export.pam.ch2 = as.data.frame(pam.ch2.result$medoid)


layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pam.ch2.result, col.p = pam.ch2.result$clustering)

#hclust
m_d = dist(m2, method = "manhattan")

types = c("ward", "single", "complete", "average", "mcquitty", "median", "centroid")
par(mfrow=c(3,3))
for(t in types){
  hc = hclust(m_d, method = t)
  plot(hc, hang = -1, labels = F)
}
m_d = dist(m2, method = "euclidean")
hc = hclust(m_d, method = "ward")
plot(hc, hang = -1, labels = F)
rect.hclust(hc,4)
plot(hc, hang = -1, labels = F)
rect.hclust(hc,6)
  
# define some clusters
mycl <- cutree(hc, h=max(hc$height/7))

# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(mycl)))

# create vector of colors for side bar
myClusterSideBar <- clusterCols[mycl]

#install.packages("gplots")
library(gplots)
# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))

# draw the heat map
heatmap.2(m2, main="Hierarchical Cluster", Rowv=as.dendrogram(hc), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)

# cutree returns a vector of cluster membership
# in the order of the original data rows
# examine it
#mycl

# examine the cluster membership by it's order
# in the heatmap
#mycl[hc$order]

# grab a cluster
#cluster1 <- m2[mycl == 1,]

# or simply add the cluster ID to your data
#foo <- cbind(d, clusterID=mycl)

# examine the data with cluster ids attached, and ordered like the heat map
#foo[hr$order,]



#devisive clustering
#install.packages("cluster")
library(cluster)
dv = diana(m2, metric = "manhattan", stand = F)
plot(dv, hang = -1, labels = F)




#topic modeling
#install.packages("topicmodels")
library(topicmodels)
rowTotals <- apply(dtm2 , 1, sum) #Find the sum of words in each Document
dtm3   <- dtm2[rowTotals> 0, ]           #remove all docs without words

k <-5
SEED <- 1234
sandy_tm<- list(VEM = LDA(dtm3, k = k, control = list(seed = SEED)),
                VEM_fixed = LDA(dtm3, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
                Gibbs = LDA(dtm3, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
                CTM = CTM(dtm3, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))))


sapply(sandy_tm[1:2], slot, "alpha")


sapply(sandy_tm, function(x)  mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))


Topic <- topics(sandy_tm[["CTM"]], 1)
Terms <- terms(sandy_tm[["CTM"]], 10)
Terms[,1:5]


#finding optimal number of topics




best.model <- lapply(seq(2,35, by=1), function(k){LDA(dtm3, k, control = list(seed = SEED) )})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
best.model.logLik.df <- data.frame(topics=c(2:35), LL=as.numeric(as.matrix(best.model.logLik)))

library(ggplot2)
ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.25, size = 14)) + 
  theme(axis.title.y = element_text(size = 14, angle=90))

 dev.copy(png,'myplot.png')
 dev.off()

best.model.logLik.df[which.max(best.model.logLik.df$LL),]
# which returns 18 which is very close to 15, so favoring parsimony build 15

VEM = LDA(dtm3, k = 3, control = list(seed = SEED))
get_terms(VEM, 10)                         # gets 5 keywords for each topic, just for a quick look
get_topics(VEM, 5)                        # gets 5 topic numbers per document








lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
require(data.table) #fore IDate

topic <- topics(lda, 1)
topics <- data.frame(date=newdata_Relief$PublishDateUTC.Day2, topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")



###playing with tm
findAssocs(dtm, "damag", 0.2)




write.table(newdata_Sandy, "C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Doug TXT\\sandy_mentions.txt", sep = "\n", col.names = F, row.names = F)

#write stuff for doug
setwd("C:\\Users\\mzaydman\\Documents\\OJT Work\\OJT Sandy\\GeoFeedia Data\\Doug TXT")

borough = c("Bronx", "Brookyln", "Manhattan", "Queens", "StatenIsland")
type = c("twitter", "not_twitter","all")

for(brg in borough){
  for(tpe in type){
    path = paste(paste(brg,tpe, sep = "_"),"_Sandy_Mentions.txt", sep = "")

    if(tpe=="all"){
      temp = newdata_Sandy[newdata_Sandy$Borough==brg ,]
    }  else if (tpe =="twitter") {
      temp = newdata_Sandy[newdata_Sandy$Borough==brg & newdata_Sandy$Source == "twitter",]
    } else {
      temp = newdata_Sandy[newdata_Sandy$Borough==brg & newdata_Sandy$Source != "twitter",]
    }

  temp2 = temp$Title
  write.table(temp2, path, sep = "\n", col.names = F, row.names = F)
  }
}

