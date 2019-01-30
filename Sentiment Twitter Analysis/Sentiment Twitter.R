library("twitteR")
library("ROAuth")

consumerKey <- "rtXXXXX"
consumerSecret <- "XXX"
accessToken <- "9XX"
accessTokenSecret <- "XXG"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

library(tm); library(SnowballC)

tweets1 <- searchTwitter('Yankees', n = 150)
tweets2 <- searchTwitter('RedSox', n = 150)

getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

data.corpus1 <- getCorpus(tweets1)
data.corpus2 <- getCorpus(tweets2)


removeURL <- function(x) {
  gsub("(http[^ ]*)", " ", x) # Remove the URLs from the tweets
}

removeNumberWords <- function(x) {
  gsub("([[:digit:]]+)([[:alnum:]])*", " ", x)
}
  
getTransCorpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus, content_transformer(removeNumberWords))
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  return (data.corpus)
}

data.Trans.corpus1 <- getTransCorpus(data.corpus1)
data.Trans.corpus2 <- getTransCorpus(data.corpus2)

tdm1 <- TermDocumentMatrix(data.Trans.corpus1) # Build the term document matrix
tdm2 <- TermDocumentMatrix(data.Trans.corpus2) # Build the term document


FFT1 <- findFreqTerms(tdm1, lowfreq=3) # frequent terms
FFT2 <- findFreqTerms(tdm2, lowfreq=3) # frequent terms
wordFreq1 <- rowSums(as.matrix(tdm1))
wordFreq2 <- rowSums(as.matrix(tdm2))

library(wordcloud)
palette <- brewer.pal(8,"Dark2")
set.seed(137)
FFT1 # Display Words in Console
wordcloud(words=names(wordFreq1), freq=wordFreq1, min.freq=3, random.order=F,colors=palette)
FFT2 # Display Words in Console
wordcloud(words=names(wordFreq2), freq=wordFreq2, min.freq=3, random.order=F,colors=palette)


# Lexicons
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

head(pos.words)


sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  score <- sum(pos.matches) - sum(neg.matches)
  cat (" Positive: ", words[pos.matches], "\n")
  cat (" Negative: ", words[neg.matches], "\n")
  return (score)
}

score <- sapply(data.Trans.corpus2$content, 
                 sentiment, 
                 pos.words, neg.words)

barplot(table(score), 
        xlab="Score", ylab="Count",
        col="cyan", main = 'Redsox Neg vs Pos')

score <- sapply(data.Trans.corpus1$content, 
                sentiment, 
                pos.words, neg.words)

barplot(table(score), 
        xlab="Score", ylab="Count",
        col="cyan", main = 'Yankees Neg vs Pos')



#For Both yankees and Redsox tweet both have a high numbe rof tweets that are neither positve or negative. In the Yankee 
# had a few more tweets that were were  categorized with having more positive context.  At the same time there was a slight
# increase of tweets that were rating with a -1 . Overall By analyzing the latest 150 tweets we see that there isn't much 
# negative or extremely positve twetets. 







