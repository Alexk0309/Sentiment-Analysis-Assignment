library(wordcloud)
library(rstudioapi)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(lubridate)
library(ggplot2)
# library(scales)
# library(reshape2)
library(dplyr)

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

Airline <- read.csv('Airline-Sentiment-2-w-AA.csv', nrows=100)
Airline['text']

write.table(Airline, file='Airline_tweets.txt', sep="\t")

Airline.text <- scan("Airline_tweets.txt", what="char", sep="\t")

Airline.text.preword.vector <- unlist(strsplit(Airline.text, "\\W"))

Airline.text.vector <-
  Airline.text.preword.vector[which(nchar(Airline.text.preword.vector) >0)]

Airline.text.vector <- gsub('[0-9]', '', Airline.text.vector)
Airline.text.vector <-gsub("https\\S*", "", Airline.text.vector) 
Airline.text.vector <-gsub("@\\S*", "", Airline.text.vector)
Airline.text.vector <-gsub("@\\w+", "", Airline.text.vector) 
Airline.text.vector <-gsub("amp", "", Airline.text.vector) 
Airline.text.vector <-gsub("[\r\n]", "", Airline.text.vector)
Airline.text.vector <-gsub("[[:punct:]]", "", Airline.text.vector)
Airline.text.vector <- unique(Airline.text.vector[Airline.text.vector != ''])

print(Airline.text.vector)

#extracting the tweets
tweets <- Airline$text
tweets <- iconv(tweets,"WINDOWS-1252","UTF-8")

d <- get_nrc_sentiment(tweets)

#combine tweets and sentiment columns
review_sentiment <- cbind(Airline$text, d)

# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
#print(head (d))
print(review_sentiment)

#barplot for sentiments
barplot(colSums(d), col = rainbow(10), ylab = 'count', main = 'Sentiment Scores for Airline Reviews')


# pdf(file = "Airline_text_analysis.pdf", width=20, height=20)
# set.seed(1234)
# wordcloud(Airline.text.vector, min.freq = 5,
#           max.words = 150,
#           random.order = FALSE,
#           random.color = FALSE,
#           rot.per = 0.0,
#           colors="black",
#           ordered.colors = FALSE,
#           use.r.layout = FALSE,
#           fixed.asp=TRUE)
# 
# 
dev.off()

