library(tm)
library(NLP)
library(RWeka)
library(plyr)

path <- "C:/Users/m.hernandez.moreno/Desktop/Curso Data Science/10. Data Science Capstone/data/en_US/"
setwd(path)
blogs <- readLines(con <- file("en_US.blogs.txt"), encoding = "UTF-8", skipNul = TRUE); close(con)
news <- readLines(con <- file("en_US.news.txt", open="rb"), encoding = "UTF-8", skipNul = TRUE); close(con)
tweets <- readLines(con <- file("en_US.twitter.txt"), encoding = "UTF-8", skipNul = TRUE); close(con)

length(blogs)
length(news)
length(tweets)
dataset <- c(blogs,tweets,news)

# 1- Get a sample of dataset
set.seed(1111)
sample_data <-  sample(dataset, length(dataset)*0.0075)


# 2- Cleaning data
build_corpus <- function (lines){
    lines <- iconv(lines, 'UTF-8', 'ASCII', sub="")
    corpus <- VCorpus(VectorSource(lines)) 
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, PlainTextDocument)
}

mycorpus <- build_corpus (sample_data)

# 3- Create corpus where low frequecy is 2, 5 or 8
ngram_corpus2 <- function (corpus,n){
    ngram <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngram))
    corpus_freq <- findFreqTerms(tdm,lowfreq = 2)
    corpus_freq <- rowSums(as.matrix(tdm[corpus_freq,]))
    corpus_freq <- data.frame(word=names(corpus_freq), frequency=corpus_freq)
    corpus_freq <- arrange(corpus_freq, -frequency)
}

ngram_corpus5 <- function (corpus,n){
    ngram <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngram))
    corpus_freq <- findFreqTerms(tdm,lowfreq = 5)
    corpus_freq <- rowSums(as.matrix(tdm[corpus_freq,]))
    corpus_freq <- data.frame(word=names(corpus_freq), frequency=corpus_freq)
    corpus_freq <- arrange(corpus_freq, -frequency)
}

ngram_corpus8 <- function (corpus,n){
    ngram <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngram))
    corpus_freq <- findFreqTerms(tdm,lowfreq = 8)
    corpus_freq <- rowSums(as.matrix(tdm[corpus_freq,]))
    corpus_freq <- data.frame(word=names(corpus_freq), frequency=corpus_freq)
    corpus_freq <- arrange(corpus_freq, -frequency)
}

# Create bigram, trigram and quadram

# Splitting words in bigram
bigram <- ngram_corpus8(mycorpus,2)
bigram$word <- as.character(bigram$word)
str2 <- strsplit(bigram$word,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)


# Splitting words in trigram
trigram <- ngram_corpus5(mycorpus,3)
trigram$word <- as.character(trigram$word)
str3 <- strsplit(trigram$word,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)



# Splitting words in quagram
quagram <- ngram_corpus2(mycorpus,4)
quagram$word <- as.character(quagram$word)
str4 <- strsplit(quagram$word,split=" ")
quagram <- transform(quagram,
                     one = sapply(str4,"[[",1),
                     two = sapply(str4,"[[",2),
                     three = sapply(str4,"[[",3),
                     four = sapply(str4,"[[",4))
quagram <- data.frame(word1 = quagram$one,word2 = quagram$two, word3 = quagram$three,
                      word4 = quagram$four, freq = quagram$freq,stringsAsFactors=FALSE)



# 4- Save ngram
path2 <- "C:/Users/m.hernandez.moreno/Desktop/Curso Data Science/10. Data Science Capstone/Project/myApp/Next_Word_Predictor"
setwd(path2)

saveRDS(bigram, "bigram.RDS")
saveRDS(trigram, "trigram.RDS")
saveRDS(quagram, "quagram.RDS")