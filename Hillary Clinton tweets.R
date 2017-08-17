setwd("C:/Users/buri/Desktop/R practice")

# Install package called twitteR to provide an interface to Twitter web API
# Install packages devtools, rjson, bit64 & httr as it would be used during authentication

install.packages(c("devtools", "rjson", "bit64", "httr"))
install.packages("twitteR")
library("twitteR")
library("devtools")
library("rjson")
library("bit64")
library("httr")

# Create a new twitter app from https://apps.twitter.com/ to generate the consumer key, 
# consumer secret, access token and access secret to set up the necessary connection

api_key <- "BRZjWZxKHidxCcrSfUN29z3U3"
api_secret <- "Xhv2fHLFxhx5Nx8Noec6k9zqffrEQqyerGOVsAEckVp0EFgMk6"
access_token <- "112152905-ClNioJLBoYCGPadYC26ZalEsnGetyVVpUNZi1Ooq"
access_token_secret <- "sGN1b0ghYzKFYPdFH25mKwNMaE0HF15ur6S5F9AmGiaE7"
setup_twitter_oauth(api_key, api_secret, access_token = NULL, access_secret = NULL)

# Search twitter with keyword Hillary Clinton
Hillarytweet <- userTimeline("HillaryClinton", n = 3200)
# Converting tweets into a dataframa
tweetsc.df <- twListToDF(Hillarytweet)
dim(tweetsc.df)
View(tweetsc.df)
# Writing it to a csv file
write.csv(tweetsc.df,"Hilltweet.csv")

# Formatting the tweet data table
names(tweetsc.df)
tweetsc_df <- tweetsc.df[,1]

# Installing tm (text mining)package in R and calling the library
install.packages(tm)
library(tm)

# Creating corpus data and cleaning it
tweetscorpus <- Corpus(VectorSource(tweetsc_df))
summary(tweetscorpus)
inspect(tweetscorpus[1:3])
tweetscorpus <- tm_map(tweetscorpus, tolower)
tweetscorpus <- tm_map(tweetscorpus, stripWhitespace)
tweetscorpus <- tm_map(tweetscorpus, removeNumbers)
tweetscorpus <- tm_map(tweetscorpus, removeWords,c(stopwords('english'),'https*'))

# Creating a TDM
tweetstdm <- TermDocumentMatrix(tweetscorpus)
summary(tweetstdm)
inspect(tweetstdm)

# FInding words with common frequency of 15
?rowSums
termfreq <- rowSums(as.matrix(tweetstdm))
?subset
termfreq <- subset(termfreq, termfreq >= 15)
df <- data.frame(term = names(termfreq), freq = termfreq)
library(ggplot2)
?ggplot
ggplot(df, aes(x = term, y = freq))+ geom_bar(stat = "Identity")+ xlab("Terms")+ ylab("Count")+ coord_flip()

# Finding word associations with election
?findAssocs
findAssocs(tweetstdm, c("election","america"), c(.20, 0.3))

# Word Cloud
library(wordcloud)
# Calculate the frequency of words and sort
wordfreq <- sort(rowSums(as.matrix(tweetstdm)), decreasing = TRUE)
wordcloud(words = names(wordfreq), freq = wordfreq, min.freq = 3, random.order = F)
warnings()







