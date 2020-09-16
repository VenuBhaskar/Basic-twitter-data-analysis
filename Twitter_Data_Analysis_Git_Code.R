rm(list=ls(all=TRUE))

#importing libraries
library(twitteR)
library(dplyr)
library(RCurl)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentimentr)
library(syuzhet)
library(tm)
library(SnowballC)

#initializing keys
api_key<-"your_api_key"
api_secret<-"your_api_secret"
access_token<-"your_access_token"
access_token_secret<-"your_access_token_secret"

#getting tweets
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1
get_tweets<-searchTwitter("#pulwamaattack",n=10000,lang = "en")
get_tweets
Tweets<-twListToDF(get_tweets)
View(Tweets)

#saving tweets into a csv
write.csv(Tweets,"pulwamatweets.csv")


#preprocessing the tweets
some_txt<- sapply(get_tweets, function(x) x$getText())
#remove rt
some_txt1<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)
#removelink
some_txt2<- gsub("http[^[:blank:]]+","",some_txt1)
#remove name
some_txt3<-gsub("@\\u+","",some_txt2)
#remove punctuations
some_txt4<-gsub("[[:punct:]]"," ",some_txt3)
#remove punctuations
some_txt5<-gsub("[^[:alnum:]]"," ",some_txt4)

head(get_tweets)
head(some_txt5)

#saving the preprocessed tweets
write.csv(some_txt5,"tweets1.csv")


#2--------------------------------------------------------------------------------------------------------------


#creating worcorpus and cleaning
some_txt6<- Corpus(VectorSource(some_txt5))
some_txt6<- tm_map(some_txt6, removePunctuation)
some_txt6<- tm_map(some_txt6, content_transformer(tolower))
some_txt6<- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6<- tm_map(some_txt6, stripWhitespace)

#building wordcloud
pal<- brewer.pal(8,"Dark2")

wordcloud(some_txt6, min.freq = 20, max.words = Inf, width=1000,
          height=1000, random.order = FALSE, color= pal )

#sentiment analysis
mysentiment<- get_nrc_sentiment(some_txt5) 
sentimentscores<- data.frame(colSums(mysentiment[,]))
#getting sntiment scores
names(sentimentscores)<-"score"
sentimentscores<-cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores)<-NULL

#plotting sentiment scores
ggplot(data=sentimentscores,aes(x=sentiment,y=score))+
  geom_bar(aes(fill=sentiment),stat="identity")+ 
  theme(legend.position = "none")+
  xlab("sentiment") +ylab("score")+ ggtitle("total sentiment score based on tweets")



#Extracting all the hashtags in the tweets
hash<-str_extract_all(Tweets[,1],"#\\S+")
hash<-data.table::transpose(hash)                      
df<-sapply(hash, "length<-",max(lengths(hash)))
View(df)
write.csv(df,"dptweets.csv")