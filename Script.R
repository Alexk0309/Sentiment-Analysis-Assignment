library(SentimentAnalysis)
library(syuzhet)
library(ggplot2)
library(dplyr)


#reading dataset
AirlineDataset <- read.csv('Airline-Sentiment-2-w-AA.csv')

#Extracting the main data that will be used for analysis 
Airline <- data.frame(airline_name = AirlineDataset$airline, airline_sentiment = AirlineDataset$airline_sentiment, negative_reason = AirlineDataset$negativereason, text = AirlineDataset$text)



#========= Finding out all the airlines in the dataset ======

AllAirlines <- data.frame(table(airline_name = Airline$airline_name))

barplot(AllAirlines$Freq, names.arg=AllAirlines$airline_name, xlab="Airlines",
        ylab="Frequency", ylim = c(0,4000), col="green", border="black", main = "Top 6 Major Airlines Discussed")



#========== Original sentiment data provided =============

ValuesOriginal <- c("Negative","Neutral","Positive")

OriginalSentiment <- data.frame(ValuesOriginal, table(Airline$airline_sentiment))


# Pie chart for original data
piepercentoriginal<- round(100*OriginalSentiment$Freq/sum(OriginalSentiment$Freq), 1)
pie(OriginalSentiment$Freq, labels = paste0(piepercentoriginal, "%"), main="Tweet Sentiment Percentages for original dataset", col = rainbow(length(OriginalSentiment$Var1)))
legend("topright", OriginalSentiment$ValuesOriginal , fill = rainbow(length(OriginalSentiment$Var1)))



#========== negative reasons data provided =============

NegativeReasons <- data.frame(table(Airline$negative_reason))



#============== Data cleaning for Analysis ===============

#Removes the @fightname
Airline$text <- gsub("@\\w+", "", Airline$text)
#Removes Links
Airline$text <- gsub("https?://.+", "", Airline$text)
#Removes digits
Airline$text <- gsub("\\d+\\w*\\d*", "", Airline$text)
#Removes non-ASCII characters
Airline$text <- gsub("[^\x01-\x7F]", "", Airline$text)
#Removes punctuation 
Airline$text <- gsub("[[:punct:]]", " ", Airline$text)
#Removes amp
Airline$text = gsub("&amp", "", Airline$text)

# Remove spaces and newlines
Airline$text <- gsub("\n", " ", Airline$text)
Airline$text <- gsub("^\\s+", "", Airline$text)
Airline$text <- gsub("\\s+$", "", Airline$text)
Airline$text <- gsub("[ |\t]+", " ", Airline$text)

#extracting the tweets for analysis
tweets <- Airline$text
tweets <- iconv(tweets,"WINDOWS-1252","UTF-8")



#=========Splitting data based on airline==========

VirginAmerica <- Airline[grep("Virgin America", Airline$airline_name), ]
United <- Airline[grep("United", Airline$airline_name), ]
Delta <- Airline[grep("Delta", Airline$airline_name), ]
Southwest <- Airline[grep("Southwest", Airline$airline_name), ]
USAirways <- Airline[grep("US Airways", Airline$airline_name), ]
American<- Airline[grep("American", Airline$airline_name), ]




#==================== Sentiment analysis using sentiment analysis package==========================

sentiment <- analyzeSentiment(tweets)
sentiment_vector <- sentiment$SentimentQDAP
SentimentDirection <- convertToDirection(sentiment$SentimentQDAP)


#=================== Using Syuzhet Library ============================
# Syuzhet method
syuzhet_vector <- get_sentiment(tweets, method="syuzhet")

# bing method
bing_vector <- get_sentiment(tweets, method="bing")

#affin method
afinn_vector <- get_sentiment(tweets, method="afinn")


#Appending everything to a dataframe to see the values
calculation <- data.frame(tweets, afinn_vector, bing_vector, syuzhet_vector, sentiment_vector)


#Grouping up data for comparison

neutral_bing <- length(which(calculation$bing_vector == 0))
positive_bing <- length(which(calculation$bing_vector > 0))
negative_bing <- length(which(calculation$bing_vector < 0))
Count_bing <- c(positive_bing,neutral_bing,negative_bing)


neutral_afinn <- length(which(calculation$afinn_vector == 0))
positive_afinn <- length(which(calculation$afinn_vector > 0))
negative_afinn <- length(which(calculation$afinn_vector < 0))
Count_afinn <- c(positive_afinn,neutral_afinn,negative_afinn)


neutral_syuzhet <- length(which(calculation$syuzhet_vector == 0))
positive_syuzhet <- length(which(calculation$syuzhet_vector > 0))
negative_syuzhet <- length(which(calculation$syuzhet_vector < 0))
Count_syuzhet <- c(positive_syuzhet,neutral_syuzhet,negative_syuzhet)

neutral_sent <- length(which(calculation$sentiment_vector == 0))
positive_sent <- length(which(calculation$sentiment_vector > 0))
negative_sent <- length(which(calculation$sentiment_vector < 0))
Count_sent <- c(positive_sent,neutral_sent,negative_sent)


Values <- c("Positive","Neutral","Negative")
output <- data.frame(Values,Count_syuzhet, Count_afinn, Count_bing, Count_sent)




#=========Plotting pie charts for the sum of the sentiment analysis for comparisons

# Syuzhet method pie chart
piepercentsyu<- round(100*output$Count_syuzhet/sum(output$Count_syuzhet), 1)
pie(output$Count_syuzhet, labels = paste0(piepercentsyu, "%"), main="Tweet Sentiment Percentages using Syuzhet method", col = rainbow(length(output$Values)))
legend("topright", output$Values, fill = rainbow(length(output$Values)))


# Affin method pie chart
piepercentafinn<- round(100*output$Count_afinn/sum(output$Count_afinn), 1)
pie(output$Count_afinn, labels = paste0(piepercentafinn, "%"), main="Tweet Sentiment Percentages using Afinn method", col = rainbow(length(output$Values)))
legend("topright", output$Values, fill = rainbow(length(output$Values)))


# Bing method pie chart
piepercentbing<- round(100*output$Count_bing/sum(output$Count_bing), 1)
pie(output$Count_bing, labels = paste0(piepercentbing, "%"), main="Tweet Sentiment Percentages using Bing method", col = rainbow(length(output$Values)))
legend("topright", output$Values, fill = rainbow(length(output$Values)))


# Sentiment analysis library method
piepercentsent<- round(100*output$Count_sent/sum(output$Count_sent), 1)
pie(output$Count_sent, labels = paste0(piepercentsent, "%"), main="Tweet Sentiment Percentages using Sentiment Analysis library", col = rainbow(length(output$Values)))
legend("topright", output$Values, fill = rainbow(length(output$Values)))



# # =============================== NRC analysis ================
# Code gets really laggy so its commented uncomment, wait and see result
# 
# # d <- get_nrc_sentiment(tweets)
# # 
# # #transpose
# # td <-data.frame(t(d))
# # 
# # #The function rowSums computes column sums across rows for each level of a grouping variable.
# # nrc <- data.frame(rowSums(td))
# # 
# # #Transformation and cleaning the table
# # names(nrc)[1] <- "count"
# # nrc <- cbind("sentiment" = rownames(nrc), nrc)
# # rownames(nrc) <- NULL
# # 
# # 
# # #Plot One - count of words associated with each sentiment
# # qp <- quickplot(sentiment, data=nrc, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
# # print(qp)
# # 
# # 
# # #Plot 2 - count of words associated with each sentiment, expressed as a percentage
# # bp <- barplot(
# #   sort(colSums(prop.table(d))),
# #   xlim = c(0,0.25),
# #   horiz = TRUE,
# #   las = 1,
# #   main = "Emotions in Text", xlab="Percentage"
# # )
# # 
# # print(bp)




syuzhet_American <- get_sentiment(American$text, method="syuzhet")

syuzhet_Delta <- get_sentiment(Delta$text, method="syuzhet")

syuzhet_Southwest <- get_sentiment(Southwest$text, method="syuzhet")

syuzhet_United <- get_sentiment(United$text, method="syuzhet")

syuzhet_UsAirways <- get_sentiment(USAirways$text, method="syuzhet")

syuzhet_VirginAmerica <- get_sentiment(VirginAmerica$text, method="syuzhet")


neutral_American <- length(which(syuzhet_American == 0))
positive_American <- length(which(syuzhet_American > 0))
negative_American <- length(which(syuzhet_American < 0))
Count_American <- c(positive_American,neutral_American,negative_American)

neutral_Delta <- length(which(syuzhet_Delta == 0))
positive_Delta <- length(which(syuzhet_Delta > 0))
negative_Delta <- length(which(syuzhet_Delta < 0))
Count_Delta <- c(positive_Delta,neutral_Delta,negative_Delta)

neutral_Southwest <- length(which(syuzhet_Southwest == 0))
positive_Southwest <- length(which(syuzhet_Southwest > 0))
negative_Southwest <- length(which(syuzhet_Southwest < 0))
Count_Southwest <- c(positive_Southwest,neutral_Southwest,negative_Southwest)

neutral_United <- length(which(syuzhet_United == 0))
positive_United <- length(which(syuzhet_United > 0))
negative_United <- length(which(syuzhet_United < 0))
Count_United <- c(positive_United,neutral_United,negative_United)

neutral_UsAirways <- length(which(syuzhet_UsAirways == 0))
positive_UsAirways <- length(which(syuzhet_UsAirways > 0))
negative_UsAirways <- length(which(syuzhet_UsAirways < 0))
Count_UsAirways <- c(positive_UsAirways,neutral_UsAirways,negative_UsAirways)


neutral_VirginAmerica <- length(which(syuzhet_VirginAmerica == 0))
positive_VirginAmerica <- length(which(syuzhet_VirginAmerica > 0))
negative_VirginAmerica <- length(which(syuzhet_VirginAmerica < 0))
Count_VirginAmerica <- c(positive_VirginAmerica,neutral_VirginAmerica,negative_VirginAmerica)


values_airlines <- c("Positive","Neutral","Negative")
output_airlines <- data.frame(values_airlines,Count_American, Count_Delta, Count_Southwest, Count_United, Count_UsAirways, Count_VirginAmerica)


piepercentAmerican<- round(100*output_airlines$Count_American/sum(output_airlines$Count_American), 1)
pie(output_airlines$Count_American, labels = paste0(piepercentAmerican, "%"), main="Sentiment percentages for American Airlines", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))

piepercentDelta<- round(100*output_airlines$Count_Delta/sum(output_airlines$Count_Delta), 1)
pie(output_airlines$Count_Delta, labels = paste0(piepercentDelta, "%"), main="Sentiment percentages for Delta Airlines", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))

piepercentSouthwest<- round(100*output_airlines$Count_Southwest/sum(output_airlines$Count_Southwest), 1)
pie(output_airlines$Count_Southwest, labels = paste0(piepercentSouthwest, "%"), main="Sentiment percentages for SouthWest Airlines", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))

piepercentUnited<- round(100*output_airlines$Count_United/sum(output_airlines$Count_United), 1)
pie(output_airlines$Count_United, labels = paste0(piepercentUnited, "%"), main="Sentiment percentages for United Airlines", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))

piepercentUsAirways<- round(100*output_airlines$Count_UsAirways/sum(output_airlines$Count_UsAirways), 1)
pie(output_airlines$Count_UsAirways, labels = paste0(piepercentUsAirways, "%"), main="Sentiment percentages for UsAirways", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))

piepercentVirginAmerica<- round(100*output_airlines$Count_VirginAmerica/sum(output_airlines$Count_VirginAmerica), 1)
pie(output_airlines$Count_VirginAmerica, labels = paste0(piepercentVirginAmerica, "%"), main="Sentiment percentages for VirginAmerica Airlines", col = rainbow(length(output_airlines$values_airlines)))
legend("topright", output_airlines$values_airlines, fill = rainbow(length(output_airlines$values_airlines)))


syuzhet_NegativeReason <- get_sentiment(American$negative_reason, method="syuzhet")

print(summary(syuzhet_NegativeReason))









# 
# 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
# 
# 
# #========================== Text Analysis ======================
# 
# 
# 
# #Replace all uppercase with lowercase letters
# Airline$text <- tolower(Airline$text)
# 
# # Note : Already using cleaned data
# 
# #
# WordC <- removeWords(Airline$text, stopwords("english"))
# 
# 
# pdf(file = "testing.pdf", width = 8.5, height = 8.5)
# set.seed(1234)
# wordcloud(WordC, min.freq = 5,
#           max.words = 150,
#           random.order = FALSE,
#           random.color = FALSE,
#           rot.per = 0.0,
#           colors = "black",
#           ordered.colors = FALSE,
#           use.r.layout = FALSE,
#           fixed.asp = TRUE)
# dev.off()


#Testing with corpus for general text analysis for all airlines

airlines_wordcloud <- Corpus(VectorSource(Airline$text))

#Remove stop words
airlines_wordcloud <- tm_map(airlines_wordcloud, removeWords, stopwords("english"))
#Remove Custom stop words
airlines_wordcloud <- tm_map(airlines_wordcloud, removeWords, c("flight", "flights", "can", "now", "will", "get"))

# Build a term-document matrix
airlines_wordcloud_dtm <- TermDocumentMatrix(airlines_wordcloud)
airlines_wordcloud_m <- as.matrix(airlines_wordcloud_dtm)

# Sort by descearing value of frequency
airlines_wordcloud_dtm_v <- sort(rowSums(airlines_wordcloud_m),decreasing=TRUE)
airlines_wordcloud_dtm_d <- data.frame(word = names(airlines_wordcloud_dtm_v),freq=airlines_wordcloud_dtm_v)


barplot(airlines_wordcloud_dtm_d[1:10,]$freq, names.arg = airlines_wordcloud_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about all airlines",
        ylab = "Word frequencies",  ylim = c(0,1400))

#generate word cloud
set.seed(1234)
wordcloud(words = airlines_wordcloud_dtm_d$word, freq = airlines_wordcloud_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))




#Wordcloud for United airlines

united_wc <- Corpus(VectorSource(United$text))

#Remove stop words
united_wc <- tm_map(united_wc, removeWords, stopwords("english"))
#Remove Custom stop words
united_wc <- tm_map(united_wc, removeWords, c("flight", "flights", "can", "now", "will", "amp", "get"))

# Build a term-document matrix
united_wc_dtm <- TermDocumentMatrix(united_wc)
united_wc_m <- as.matrix(united_wc_dtm)

# Sort by descearing value of frequency
united_wc_dtm_v <- sort(rowSums(united_wc_m),decreasing=TRUE)
united_wc_dtm_d <- data.frame(word = names(united_wc_dtm_v),freq=united_wc_dtm_v)


barplot(united_wc_dtm_d[1:10,]$freq, names.arg = united_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about Uunited Airlines",
        ylab = "Word frequencies",  ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = united_wc_dtm_d$word, freq = united_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))


#Wordcloud for USAirwyas airlines

USAirways_wc <- Corpus(VectorSource(USAirways$text))

#Remove stop words
USAirways_wc <- tm_map(USAirways_wc, removeWords, stopwords("english"))
#Remove Custom stop words
USAirways_wc <- tm_map(USAirways_wc, removeWords, c("flight", "flights", "can", "now", "will", "amp", "get"))

# Build a term-document matrix
USAirways_wc_dtm <- TermDocumentMatrix(USAirways_wc)
USAirways_wc_m <- as.matrix(USAirways_wc_dtm)

# Sort by descearing value of frequency
USAirways_wc_dtm_v <- sort(rowSums(USAirways_wc_m),decreasing=TRUE)
USAirways_wc_dtm_d <- data.frame(word = names(USAirways_wc_dtm_v),freq=USAirways_wc_dtm_v)


barplot(USAirways_wc_dtm_d[1:10,]$freq, names.arg = USAirways_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about USAirways Airlines",
        ylab = "Word frequencies", ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = USAirways_wc_dtm_d$word, freq = USAirways_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))




#Wordcloud for American airlines

American_wc <- Corpus(VectorSource(American$text))

#Remove stop words
American_wc <- tm_map(American_wc, removeWords, stopwords("english"))
#Remove Custom stop words
American_wc <- tm_map(American_wc, removeWords, c("flight", "flights", "can", "now", "will", "amp", "get"))

# Build a term-document matrix
American_wc_dtm <- TermDocumentMatrix(American_wc)
American_wc_m <- as.matrix(American_wc_dtm)

# Sort by descearing value of frequency
American_wc_dtm_v <- sort(rowSums(American_wc_m),decreasing=TRUE)
American_wc_dtm_d <- data.frame(word = names(American_wc_dtm_v),freq=American_wc_dtm_v)


barplot(American_wc_dtm_d[1:10,]$freq, names.arg = American_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about American Airlines",
        ylab = "Word frequencies", ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = American_wc_dtm_d$word, freq = American_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))






















# 
# #======== Extra ignore for now ============
# 
# # 
# # 
# # #---------------------- Checking if there is a difference for cleaned data -----------------#
# # 
# # # using sentiment analysis package
# # 
# # sentiment2 <- analyzeSentiment(tweetsCleaned)
# # sentiment_vector2 <- sentiment2$SentimentQDAP
# # SentimentDirection2 <- convertToDirection(sentiment2$SentimentQDAP)
# # 
# # 
# # # Syuzhet
# # syuzhet_vector2 <- get_sentiment(tweetsCleaned, method="syuzhet")
# # 
# # # bing
# # bing_vector2 <- get_sentiment(tweetsCleaned, method="bing")
# # 
# # #affin
# # afinn_vector2 <- get_sentiment(tweetsCleaned, method="afinn")
# # 
# # 
# # #Appending everything to a dataframe to see the values
# # calculation2 <- data.frame(tweetsCleaned, afinn_vector2, bing_vector2, syuzhet_vector2, sentiment_vector2)
# # 
# # 
# # #Grouping up data for comparison
# # 
# # neutral_bing2 <- length(which(calculation2$bing_vector2 == 0))
# # positive_bing2 <- length(which(calculation2$bing_vector2 > 0))
# # negative_bing2 <- length(which(calculation2$bing_vector2 < 0))
# # Count_bing2 <- c(positive_bing2,neutral_bing2,negative_bing2)
# # 
# # 
# # neutral_afinn2 <- length(which(calculation2$afinn_vector2 == 0))
# # positive_afinn2 <- length(which(calculation2$afinn_vector2 > 0))
# # negative_afinn2 <- length(which(calculation2$afinn_vector2 < 0))
# # Count_afinn2 <- c(positive_afinn2,neutral_afinn2,negative_afinn2)
# # 
# # 
# # neutral_syuzhet2 <- length(which(calculation2$syuzhet_vector2 == 0))
# # positive_syuzhet2 <- length(which(calculation2$syuzhet_vector2 > 0))
# # negative_syuzhet2 <- length(which(calculation2$syuzhet_vector2 < 0))
# # Count_syuzhet2 <- c(positive_syuzhet2,neutral_syuzhet2,negative_syuzhet2)
# # 
# # neutral_sent2 <- length(which(calculation2$sentiment_vector2 == 0))
# # positive_sent2 <- length(which(calculation2$sentiment_vector2 > 0))
# # negative_sent2 <- length(which(calculation2$sentiment_vector2 < 0))
# # Count_sent2 <- c(positive_sent2,neutral_sent2,negative_sent2)
# # 
# # 
# # Values2 <- c("Positive","Neutral","Negative")
# # output2 <- data.frame(Values2,Count_syuzhet2, Count_afinn2, Count_bing2, Count_sent2)
# # 
# 








# Try when got more time 
# http://www.tabvizexplorer.com/sentiment-analysis-using-r-and-twitter/


# Create comparison word cloud data
# wordcloud_forsentiment = c(
#   paste(tweets.df$text[emotions$anger > 0], collapse=" "),
#   paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
#   paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
#   paste(tweets.df$text[emotions$fear > 0], collapse=" "),
#   paste(tweets.df$text[emotions$joy > 0], collapse=" "),
#   paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
#   paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
#   paste(tweets.df$text[emotions$trust > 0], collapse=" ")
# )
# 
# # create corpus
# corpus = Corpus(VectorSource(wordcloud_tweet))
# 
# # remove punctuation, convert every word in lower case and remove stop words
# 
# corpus = tm_map(corpus, tolower)
# corpus = tm_map(corpus, removePunctuation)
# corpus = tm_map(corpus, removeWords, c(stopwords("english")))
# corpus = tm_map(corpus, stemDocument)
# 
# # create document term matrix
# 
# tdm = TermDocumentMatrix(corpus)
# 
# # convert as matrix
# tdm = as.matrix(tdm)
# tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
# 
# # column name binding
# colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
# colnames(tdmnew) <- colnames(tdm)
# comparison.cloud(tdmnew, random.order=FALSE,
#                  colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
#                  title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

