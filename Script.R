library(SentimentAnalysis)
library(syuzhet)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)



#reading dataset
AirlineDataset <- read.csv('Airline-Sentiment-2-w-AA.csv')

#Extracting the main data that will be used for analysis 
Airline <- data.frame(airline_name = AirlineDataset$airline, airline_sentiment = AirlineDataset$airline_sentiment, negative_reason = AirlineDataset$negativereason, text = AirlineDataset$text, tweets_created = AirlineDataset$tweet_created)


#========== Visualising Original sentiment data provided =============

ValuesOriginal <- c("Negative","Neutral","Positive")

OriginalSentiment <- data.frame(ValuesOriginal, table(Airline$airline_sentiment))

# Pie chart for original data
piepercentoriginal<- round(100*OriginalSentiment$Freq/sum(OriginalSentiment$Freq), 1)
pie(OriginalSentiment$Freq, labels = paste0(piepercentoriginal, "%"), main="Tweet Sentiment Percentages for original dataset", col = rainbow(length(OriginalSentiment$Var1)))
legend("topright", OriginalSentiment$ValuesOriginal , fill = rainbow(length(OriginalSentiment$Var1)))



#========= Finding out all the airlines in the dataset ======

AllAirlines <- data.frame(table(airline_name = Airline$airline_name))

barplot(AllAirlines$Freq, names.arg=AllAirlines$airline_name, xlab="Airlines",
        ylab="Frequency", ylim = c(0,4000), col="green", border="black", main = "Top 6 Major Airlines Discussed")


#========== negative reasons data provided =============

NegativeReasons <- data.frame(table(Airline$negative_reason))

#Removing empty responses
NegativeReasons = NegativeReasons[-1,]

# Plotting top 5 negative Reasons

print(top_n(NegativeReasons, n=5, Freq) %>%
        ggplot(., aes(x=Var1, y=Freq))+
        geom_bar(stat='identity', color = "black", fill = "lightgreen") +
        ylab("Frequency") +
        xlab("Negative Reason") +
        ggtitle("Top 5 Negative Reasons for Airlines"))


#============== Data cleaning the tweets for Analysis ===============

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



# #==================== Sentiment analysis using sentiment analysis package==========================

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



# Grouping up data based on negative/neutral/positive for comparison

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



# =============================== NRC analysis for emotions ================
#Reference Used from lecture
#Code gets really laggy so its commented 
# d <- get_nrc_sentiment(tweets)
#
# #transpose
# td <-data.frame(t(d))
#
# #The function rowSums computes column sums across rows for each level of a grouping variable.
# nrc <- data.frame(rowSums(td))
#
# #Transformation and cleaning the table
# names(nrc)[1] <- "count"
# nrc <- cbind("sentiment" = rownames(nrc), nrc)
# rownames(nrc) <- NULL
#
#
# #Plot One - count of words associated with each sentiment
# qp <- quickplot(sentiment, data=nrc, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
# print(qp)
#
#
# #Plot 2 - count of words associated with each sentiment, expressed as a percentage
# bp <- barplot(
#   sort(colSums(prop.table(d))),
#   xlim = c(0,0.25),
#   horiz = TRUE,
#   las = 1,
#   main = "Emotions in Text", xlab="Percentage"
# )
#
# print(bp)

#Appending the Syuzhet method data into the data frame 
Airline$calculated_sentiment <- syuzhet_vector


#Histogram for syuzhet sentiment score
print(Airline %>%
        ggplot(aes(x=calculated_sentiment)) +
        geom_histogram(binwidth = 1, color = "black", fill = "green")+
        ylab("Frequency") +
        xlab("sentiment score") +
        ggtitle("Distribution of Sentiment scores of the tweets"))



#=========Splitting data based on airline==========

VirginAmerica <- Airline[grep("Virgin America", Airline$airline_name), ]
United <- Airline[grep("United", Airline$airline_name), ]
Delta <- Airline[grep("Delta", Airline$airline_name), ]
Southwest <- Airline[grep("Southwest", Airline$airline_name), ]
USAirways <- Airline[grep("US Airways", Airline$airline_name), ]
American<- Airline[grep("American", Airline$airline_name), ]


neutral_American <- length(which(American$calculated_sentiment == 0))
positive_American <- length(which(American$calculated_sentiment > 0))
negative_American <- length(which(American$calculated_sentiment < 0))
Count_American <- c(positive_American,neutral_American,negative_American)

neutral_Delta <- length(which(Delta$calculated_sentiment == 0))
positive_Delta <- length(which(Delta$calculated_sentiment > 0))
negative_Delta <- length(which(Delta$calculated_sentiment < 0))
Count_Delta <- c(positive_Delta,neutral_Delta,negative_Delta)

neutral_Southwest <- length(which(Southwest$calculated_sentiment == 0))
positive_Southwest <- length(which(Southwest$calculated_sentiment > 0))
negative_Southwest <- length(which(Southwest$calculated_sentiment < 0))
Count_Southwest <- c(positive_Southwest,neutral_Southwest,negative_Southwest)

neutral_United <- length(which(United$calculated_sentiment == 0))
positive_United <- length(which(United$calculated_sentiment > 0))
negative_United <- length(which(United$calculated_sentiment < 0))
Count_United <- c(positive_United,neutral_United,negative_United)

neutral_UsAirways <- length(which(USAirways$calculated_sentiment == 0))
positive_UsAirways <- length(which(USAirways$calculated_sentiment > 0))
negative_UsAirways <- length(which(USAirways$calculated_sentiment < 0))
Count_UsAirways <- c(positive_UsAirways,neutral_UsAirways,negative_UsAirways)

neutral_VirginAmerica <- length(which(VirginAmerica$calculated_sentiment == 0))
positive_VirginAmerica <- length(which(VirginAmerica$calculated_sentiment > 0))
negative_VirginAmerica <- length(which(VirginAmerica$calculated_sentiment < 0))
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



#========================== Text Analysis ======================

# Note : We are already using Cleaned Data
#Testing with corpus for general text analysis for all airlines

#Creating a corpus for tweets cleaned
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

#Plotting bar graph to see top 10 most used words in tweets
barplot(airlines_wordcloud_dtm_d[1:10,]$freq, names.arg = airlines_wordcloud_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about all airlines",
        ylab = "Word frequencies",  ylim = c(0,1400))

#generate word cloud
set.seed(1234)
wordcloud(words = airlines_wordcloud_dtm_d$word, freq = airlines_wordcloud_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))




#Wordcloud for tweets about United airlines

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

#Plotting bar graph for top 10 most used words in tweets towards United Airlines
barplot(united_wc_dtm_d[1:10,]$freq, names.arg = united_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about Uunited Airlines",
        ylab = "Word frequencies",  ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = united_wc_dtm_d$word, freq = united_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))



#Wordcloud for tweets about USAirways


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

#Plotting bar graph for top 10 most used words in tweets towards USAirways
barplot(USAirways_wc_dtm_d[1:10,]$freq, names.arg = USAirways_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about USAirways Airlines",
        ylab = "Word frequencies", ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = USAirways_wc_dtm_d$word, freq = USAirways_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))




#Wordcloud for tweets about American Airlines


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

#Plotting bar graph for top 10 most used words in tweets towards American Airlines
barplot(American_wc_dtm_d[1:10,]$freq, names.arg = American_wc_dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words within tweets about American Airlines",
        ylab = "Word frequencies", ylim = c(0,400))

#generate word cloud
set.seed(1234)
wordcloud(words = American_wc_dtm_d$word, freq = American_wc_dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))
