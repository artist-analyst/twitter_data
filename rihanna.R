#Title: A Lesson From Rihanna...Change the Game, Build Your Brand
#Author: Robb King
#Date: June 20, 2018
#Close: June 21, 2018
#Desctiption: A close look at Rihanna's twitter word usage

#upload the data setting that was saved using twitter API
rihanna_df <- read.csv("C:/Users/rking/Documents/rihanna_tweets.csv")

#reformat the dates for the data setting. the code must be converted from an integer to a character to a date
rihanna_df$date <- format(as.Date(as.character(rihanna_df$created, format = "%Y%m%d%M%H"), "%Y-%m-%d"))
rihanna_df$year <- format(as.Date(rihanna_df$date), "%Y")
rihanna_df$month <- format(as.Date(rihanna_df$date), "%m")

#only keep the years 2015, 2016 and 2017. I have all the tweets for these years
rihanna_df <- subset(rihanna_df, year %in% c('2015', '2016', '2017'))

#convert favorites and retweets from integers to numerics
rihanna_df$favoriteCount <- as.numeric(rihanna_df$favoriteCount)
rihanna_df$retweetCount <- as.numeric(rihanna_df$retweetCount)

#graph the number of retweets by year and month
library(ggplot2)
retweets <- ggplot(data = rihanna_df, 
                   mapping = aes(x = rihanna_df$year,
                                 y = rihanna_df$retweetCount,
                                 fill = rihanna_df$month)) +
  geom_bar(stat = "identity", position = position_stack(reverse = T)) +
  theme_minimal() +
  labs(title = "Rihanna's Retweeted Tweets") +
  labs(xlab("year")) +
  labs(ylab("retweet count")) +
  scale_fill_discrete(name = "month",
                      labels = c("January", "February",
                                "March", "April", "May",
                                "June", "July", "August",
                                "September", "October",
                                "November", "December")) +
  coord_flip()


#graph the number of favorites by year and month
favorites <- ggplot(data = rihanna_df, 
                   mapping = aes(x = rihanna_df$year,
                                 y = rihanna_df$favoriteCount,
                                 fill = rihanna_df$month)) +
  geom_bar(stat = "identity", position = position_stack(reverse = T)) +
  theme_minimal() +
  labs(title = "Rihanna's Favorited Tweets") +
  labs(xlab("year")) +
  labs(ylab("retweet count")) +
  scale_fill_discrete(name = "month",
                      labels = c("January", "February",
                                 "March", "April", "May",
                                 "June", "July", "August",
                                 "September", "October",
                                 "November", "December")) +
  coord_flip()

#note: Rihanna had a surge in favorites and retweets in 2017, most notably in September of 2017

#create data setting for 2017 only
rihanna_2017 <- subset(rihanna_df, year %in% c('2017'))

#create data setting for September of 2017 only
rihanna_sept17 <- subset(rihanna_2017, month %in% c('09'))

#let's analyze the words Rihanna used during this time

#build a corpus
library(tm)
rihanna_corpus1 <- Corpus(VectorSource(rihanna_sept17$text))

#use tm to remove unwatned data (https://stackoverflow.com/questions/41109773/gsub-function-in-tm-package-to-remove-urls-does-not-remove-the-entire-string)
rihanna_corpus1 <- VCorpus(VectorSource(rihanna_sept17$text))
rihanna_corpus1 <- tm_map(rihanna_corpus1, tolower)
rihanna_corpus1 <- tm_map(rihanna_corpus1, removePunctuation)
rihanna_corpus1 <- tm_map(rihanna_corpus1, removeNumbers)
myStopwords <- c(stopwords('english'), "avaiable", "via")
rihanna_corpus1 <- tm_map(rihanna_corpus1, removeWords, myStopwords)
rihanna_corpus1 <- tm_map(rihanna_corpus1, PlainTextDocument)

#build text document matrix
rihanna_tdm <- TermDocumentMatrix(rihanna_corpus1, control = list(wordLengths = c(1,Inf)))

#find the most frequent terms used by Rihanna in September of 2017
findFreqTerms(rihanna_tdm, lowfreq = 10)

#creating word cloud
#install.packages("wordcloud")
library(wordcloud)

m <- as.matrix(rihanna_tdm)
wordFreq <- sort(rowSums(m), decreasing = TRUE)
set.seed(375)
grayLevels <- gray((wordFreq+10)/(max(wordFreq)+10))
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 3, random.order = F, colors = grayLevels)




#References:
#(https://www.youtube.com/watch?v=I0VCGCnquTQ)
