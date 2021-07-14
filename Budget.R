install.packages(c("httr", "devtools", "twitteR", "base64enc", "topicmodels", "party", "tm", "wordcloud", "slam", "ggplot2", "RColorBrewer", "dplyr", "shinythemes"))

library(tm)
library(httr)
library(devtools)
library(twitteR)
library(base64enc)
library(party)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

#===========Authentication===================
api_key <- "sUPRcLicVLAGWhvrOTyNBmOM7"
api_secret <- "XEfYX9ME72YJEQcqdrOQUDS9jjGDl8kZe2sNOMutB6pRtbvxIC"
access_token <- "752143664965640192-lrBU6C3J3eMkBaTSEyIFkIDRGQEulZZ"
access_token_secret <- "3JKQzioK9xQ5TxWFTdyLjCGuNK9mbMxgNv1nCG3g73vai"
setup_twitter_oauth(consumer_key = api_key,consumer_secret =api_secret,access_token = access_token,access_secret = access_token_secret)
#============================================
#================Downloading the Text========
NBA=searchTwitter("#budget2018",n=1000,lang="en")
#=============================================

head(NBA)
tail(NBA)
length(NBA)
NBA_text = sapply(NBA, function(x) x$getText())


#============Preprocessing of the text==========
NBA_text = gsub("RT", "", NBA_text)
NBA_text = gsub("@\\w+", "", NBA_text)
NBA_text = gsub("[[:punct:]]", "", NBA_text)
NBA_text = gsub("http\\w+", "", NBA_text)
NBA_text = gsub("[ |\t]{2,}", "", NBA_text)
NBA_text = gsub("^ ", "", NBA_text)
NBA_text = gsub(" $", "", NBA_text)
#===============================================
#NBA_text = iconv(NBA_text,to="utf-8-mac") # Windows Do NOt DO this
#wordcloud(NBAcorpus,min.freq = 1,colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words =250)

#====WORD CLOUD================
NBAcorpus = Corpus(VectorSource(NBA_text))
NBAcorpus = tm_map(NBAcorpus, function(x)removeWords(x,stopwords()))
#Create a term-document matrix from a corpus
tdm = TermDocumentMatrix(NBAcorpus,control = list(removePunctuation = TRUE,stopwords = c("new", "year", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))
#Convert as matrix
m = as.matrix(tdm)
#Get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
#Create data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
#Plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
#=============================================

#====Hierarchical Clustering==============
myStopwords <- c(stopwords('english'), "http","and","yes","no")
NBAcorpus=tm_map(NBAcorpus,removeWords,myStopwords)
NBAdtm = TermDocumentMatrix(NBAcorpus)
NBAterms=findFreqTerms(NBAdtm, lowfreq=10)
NBAdtm1 = removeSparseTerms(NBAdtm, sparse=0.99)
NBAfit = hclust(dist(scale(NBAdtm1)), method="ward.D")
plot(NBAfit)
#===========================================

#=========Topic Modeling========================
NBAdtm2=scale(NBAdtm1)
#NBAdtm2
NBAdtm3=DocumentTermMatrix(NBAcorpus)
NBAdtm3=removeSparseTerms(NBAdtm3,0.999)
NBAdtm3=NBAdtm3[rowSums(as.matrix(NBAdtm3))>0,]
NBAlda=LDA(NBAdtm3,5)
terms(NBAlda,10)
NBAlda
#===============================================
setwd("C:/Users/Karan Dawar/Documents")
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")

#===============================================

#==============Sentiment Analysis Function=====
score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}
tweets=searchTwitter("#budget2018",n=1000,lang="en")
Tweets.text = laply(tweets,function(t)t$getText())
analysis = score.sentiment(Tweets.text, pos, neg)
hist(analysis$score)
