# ------------------------------------------------------
#** Coffee review data analysis by Thaher Deen**
# ------------------------------------------------------
# Data sources.
# Data downloaded from: https://www.kaggle.com/datasets/schmoyote/coffee-reviews-dataset
# Data originally scraped from www.coffeereview.com and uploaded here: https://www.kaggle.com/datasets/hanifalirsyad/coffee-scrap-coffeereview/versions/2
# ------------------------------------------------------

#**Attaching packages.**
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

#** Importing data.**
Coffee_analysis <- read.csv("coffee_analysis.csv")

#** Selecting review columns for text analysis and creating a Corpus.**
# For simplicity I will only be using the first review for each roast.
Text1 <- Corpus(VectorSource(Coffee_analysis$desc_1))

#** Cleaning text.**
#Replacing "/", "@" and "|" with space
toSpace <-
  content_transformer(function (x , pattern)
    gsub(pattern, " ", x))
Text1 <- tm_map(Text1, toSpace, "/")
Text1 <- tm_map(Text1, toSpace, "@")
Text1 <- tm_map(Text1, toSpace, "\\|")
# Converting the text to lower case
Text1 <- tm_map(Text1, content_transformer(tolower))
# Removing numbers
Text1 <- tm_map(Text1, removeNumbers)
# Removing english stopwords
Text1 <- tm_map(Text1, removeWords, stopwords("english"))
# Removing punctuation
Text1 <- tm_map(Text1, removePunctuation)
# Removing extra white spaces
Text1 <- tm_map(Text1, stripWhitespace)

#** Building a term document matrix to see what the most common words are.**
Text1_dtm <- TermDocumentMatrix(Text1)
dtm_m <- as.matrix(Text1_dtm)
# Sorting by descending value of frequency
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

#** generating a word cloud**
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

#** getting a sentiment score using the syuzhet method**
syuzhet_vector <- get_sentiment(Text1, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)
sign (syuzhet_vector)
