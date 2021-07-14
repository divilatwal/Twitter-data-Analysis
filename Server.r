
library(shiny)


shinyServer(function(input, output) {
  
  library(tm)
  library(httr)
  library(devtools)
  library(twitteR)
  library(base64enc)
  
  
  
  #====IMPORTING THE LIBRARIES================
  
  library(NLP)

  library(ggplot2)
  library(openNLP)
  library(plyr)
  library(SnowballC)
  library(topicmodels)
  library(RColorBrewer)
  library(RSentiment)
  library(wordcloud)
  library(bitops)
  library(RCurl)
  
  library(pacman)
  library(wordcloud)
  library(ggmap)
  library(maptools)
  library(maps)
  library(slam)
  library(shiny)
  library(shinyGlobe)
  
  api_key <- "sUPRcLicVLAGWhvrOTyNBmOM7"
  api_secret <- "XEfYX9ME72YJEQcqdrOQUDS9jjGDl8kZe2sNOMutB6pRtbvxIC"
  access_token <- "752143664965640192-lrBU6C3J3eMkBaTSEyIFkIDRGQEulZZ"
  access_token_secret <- "3JKQzioK9xQ5TxWFTdyLjCGuNK9mbMxgNv1nCG3g73vai"
  setup_twitter_oauth(consumer_key = api_key,consumer_secret =api_secret,access_token = access_token,access_secret = access_token_secret)
  
  
    output$plot<-renderPlot({ 
    
      if(input$pType=='a')
    {
      
      searchterm<-input$term
      num<-input$i
      
      list <- searchTwitter(searchterm, n= num, lang="en") 
      
      
      
      l <- sapply(list, function(x) x$getText())
      
      l <- iconv(l, "latin1", "ASCII//TRANSLIT")
      
      l <- iconv(l, to='ASCII//TRANSLIT')
      
      #create corpus
      lc <- Corpus(VectorSource(l))
      
      #clean up
      
      lc <- tm_map(lc, content_transformer(tolower)) 
      lc <- tm_map(lc, removePunctuation)
      lc <- tm_map(lc, function(x)removeWords(x,stopwords()))
      
      wordcloud(lc, max=250,random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    }
      
    else if(input$pType=='b')
    {
      
      searchterm<-input$term
      num<-input$i
      
      list <- searchTwitter(searchterm, n= num, lang="en") 
      
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
     
      Tweets.text = laply(list,function(t)t$getText())
      analysis = score.sentiment(Tweets.text, pos, neg)
      hist(analysis$score)
      
    }
      
      else if(input$pType=='d')
      {
        searchterm<-input$term
        num<-input$i
        
        #access tweets and create cumulative file
        NBA_text<- searchTwitter(searchterm,n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
        NBAcorpus = Corpus(VectorSource(NBA_text))
        myStopwords <- c(stopwords('english'), "http","and","yes","no")
        NBAcorpus=tm_map(NBAcorpus,removeWords,myStopwords)
        NBAdtm = TermDocumentMatrix(NBAcorpus)
        NBAterms=findFreqTerms(NBAdtm, lowfreq=10)
        NBAdtm1 = removeSparseTerms(NBAdtm, sparse=0.99)
        NBAdtm2=scale(NBAdtm1)
        #NBAdtm2
        NBAdtm3=DocumentTermMatrix(NBAcorpus)
        NBAdtm3=removeSparseTerms(NBAdtm3,0.999)
        NBAdtm3=NBAdtm3[rowSums(as.matrix(NBAdtm3))>0,]
        NBAlda=LDA(NBAdtm3,5)
        terms(NBAlda,10)
        NBAlda
        
      }
      
      else if(input$pType=='f')
      {
        
        searchterm<-input$term
        num<-input$i
        
        list <- searchTwitter(searchterm, n= num, lang="en") 
        
        try.error=function(x)
        {
          y=NA
          try_error=tryCatch(tolower(x),error=function(e) e)
          if(!inherits(try_error,"error"))
            y=tolower(x)
          return(y)
        }
        
        textdata <- sapply(list, function(x) x$getText())
        textdata=sapply(textdata,try.error)
        textdata=textdata[!is.na(textdata)]
        names(textdata)= NULL
        classify_sent=classify_emotion(textdata,algorithm="bayes",prior=1.0)
        emotion=classify_sent[,7]
        emotion[is.na(emotion)]="other"
        classify_polar=classify_polarity(textdata,algorithm="bayes")
        polarity=classify_polar[,4]
        sent_df=data.frame(text=textdata,emotion=emotion,polarity=polarity,stringsAsFactors = FALSE)
        sent_df=within(sent_df,emotion<-factor(emotion),levels=names(sort(table(emotion),decreasing = TRUE)))
        ggplot(sent_df,aes(x=emotion))+ geom_bar(aes(y=..count..,fill=emotion))+scale_fill_brewer(palette = "Dark2")+labs(x="emotion categories",y="")
        
        ggplot(sent_df,aes(x=emotion))+ geom_bar(aes(y=..count..,fill=polarity))+scale_fill_brewer(palette = "RdGy")+labs(x="Polarity categories",y="")
        
        
        
      }
      
     
      
      else if(input$pType=='e')
      {
        
        #access tweets and create cumulative file
        
        
        searchterm<-input$term
        num<-input$i    
        
        
        
        list <- searchTwitter(searchterm,n= num, lang="en", since=NULL, until=NULL, retryOnRateLimit=10) 
        df <- twListToDF(list)
        df <- df[, order(names(df))]
        df$created <- strftime(df$created, '%Y-%m-%d')
        if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
        #merge last access with cumulative file and remove duplicates
        stack <- read.csv(file=paste(searchterm, '_stack.csv'))
        stack <- rbind(stack, df)
        stack <- subset(stack, !duplicated(stack$text))
        write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
        
        
        #evaluation tweets function
        score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
        {
          library("plyr")
          library("stringr")
          scores <- laply(sentences, function(sentence, pos.words, neg.words){
            
            
            
            
            sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
            
            
            sentence <- iconv(sentence, to='ASCII//TRANSLIT')
            
            
            
            
            sentence <- gsub('[[:punct:]]', "", sentence)
            sentence <- gsub('[[:cntrl:]]', "", sentence)
            sentence <- gsub('\\d+', "", sentence)
            sentence <- tolower(sentence)
            word.list <- str_split(sentence, '\\s+')
            words <- unlist(word.list)
            pos.matches <- match(words, pos.words)
            neg.matches <- match(words, neg.words)
            pos.matches <- !is.na(pos.matches)
            neg.matches <- !is.na(neg.matches)
            score <- sum(pos.matches) - sum(neg.matches)
            return(score)
          }, pos.words, neg.words, .progress=.progress)
          scores.df <- data.frame(score=scores, text=sentences)
          return(scores.df)
          detach("package:plyr", unload = TRUE)
        }
        library("ggplot2")
        pos <- scan('http://ptrckprry.com/course/ssd/data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
        neg <- scan('http://ptrckprry.com/course/ssd/data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
        
        pos.words <- c(pos, 'upgrade')
        neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
        Dataset <- stack
        Dataset$text <- as.factor(Dataset$text)
        scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
        write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
        #total evaluation: positive / negative / neutral
        stat <- scores
        stat$created <- stack$created
        stat$created <- as.Date(stat$created)
        stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
        require("dplyr")
        by.tweet <- group_by(stat, tweet, created)
        by.tweet <- summarise(by.tweet, number=n())
        detach("package:dplyr", unload = TRUE)
        write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)
        #create chart
        
        
        (ggplot(by.tweet, aes(created,number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
            geom_point(aes(group=tweet, color=tweet), size=4) +
            theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
            stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'yellow', size=2, geom = 'line') + 
            ggtitle(searchterm) + 
            xlab("Date of Tweet") + 
            ylab("Sentiments")   
        )
        
        
        
        
        
      }
    else if(input$pType=='c')
    {
      
      searchterm<-input$term
      num<-input$i
      
      list <- searchTwitter(searchterm, n= num, lang="en") 
      
      
      l <- sapply(list, function(x) x$getText())
      
      l <- iconv(l, "latin1", "ASCII//TRANSLIT")
      
       
      l <- iconv(l, to='ASCII//TRANSLIT')
      
      
      #create corpus
      lc <- Corpus(VectorSource(l))
      
      #clean up
      
      
      lc <- tm_map(lc, content_transformer(tolower)) 
      lc <- tm_map(lc, removePunctuation)
      lc <- tm_map(lc, function(x)removeWords(x,stopwords()))
      
      lcdtm = TermDocumentMatrix(lc)
      lcterms=findFreqTerms(lcdtm, lowfreq=10)
      lcdtm1 = removeSparseTerms(lcdtm, sparse=0.99)
      lcfit = hclust(dist(scale(lcdtm1)), method="ward.D")
      plot(lcfit)      
      
    }
    
  }
  )
 
}) 