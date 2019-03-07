#liraries used for connecting R and Twitter to scrap the data

library("RCurl")
library("httr")
library("openssl")
library("httpuv")
library("twitteR")

#library used for text mining in R
library("tm")

#library used for processing strings in R
library("stringr")

#library used for working with dataframes
library("dplyr")

#library for creating wordcloud
library("wordcloud")
library("syuzhet")

#library for visualisation
library("plotly")

#give you api keys and acces keys here
#the keys given are duplicate not real

api_key <- "abcd123"
api_secret <- "abcd123"
access_token <- "abcd123"
access_secret <- "abcd123"

#seting connection with twitter
setup_twitter_oauth(api_key,api_secret,access_token,access_secret)

#extractig the tweets based on hashtags and number of tweets can also be mentioned
gbm_tweet <- searchTwitter("#youhashtaghere", n=100)

#converting them into dataframe so that they can be processed easily
gbm_tweet.df<-twListToDF(gbm_tweet)
head(gbm_tweet.df)


#cleaning the tweets such as removing the spaces and special characters to make them ready for aanalysis
gbm_tweet.df$text=gsub("&amp", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("&amp", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("@\\w+", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("[[:punct:]]", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("[[:digit:]]", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("http\\w+", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("[ \t]{2,}", "", gbm_tweet.df$text)
gbm_tweet.df$text = gsub("^\\s+|\\s+$", "", gbm_tweet.df$text)
gbm_tweet.df$text <- iconv(gbm_tweet.df$text, "UTF-8", "ASCII", sub="")


#getting the emotions in the tweets
emotions <- get_nrc_sentiment(gbm_tweet.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])


#plot that shows a barchar for emotions
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #yourhashtag")

#to seperate the tweets based on emotions
wordcloud_tweet = c(
  paste(gbm_tweet.df$text[emotions$anger > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$anticipation > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$disgust > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$fear > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$joy > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$sadness > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$surprise > 0], collapse=" "),
  paste(gbm_tweet.df$text[emotions$trust > 0], collapse=" ")
)


#store the seperate emotion tweets in seperate documents
corpus = Corpus(VectorSource(wordcloud_tweet))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, tolower)


#creating the text document matrix for wordcloud
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

#plotting the wordcloud
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250,scale=c(2, 0.5),rot.per=0.2)
