#Data set generated by "Getting_and_Cleaning_Data.R"

bigram <- readRDS("bigram.RDS")
trigram <- readRDS("trigram.RDS")
quagram <- readRDS("quagram.RDS")

library(tm)
library(NLP)
library(plyr)


next_word<-function(sentence)  {
    
    sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence),
                                                                preserve_intra_word_dashes = TRUE)))
    sentence_split<- strsplit(sentence_c," ")[[1]]
    top_words <- data.frame(word=is.null,freq=is.null)
    
    for (i in c(2,3,4)) {
        weight_i<-i/10
        last_words<-tail(sentence_split,i)
        if(i==2) {
            word <- as.character(bigram[bigram$word1==last_words[2],]$word2[1:5])
            freq<-as.integer(bigram[bigram$word1==last_words[2],]$freq[1:5])*weight_i
            
        } else if(i==3) {
            word <- as.character(trigram[trigram$word1==last_words[2] 
                                         & trigram$word2 == last_words[3],]$word3[1:5])
            freq<-as.integer(trigram[trigram$word1==last_words[2]
                                     & trigram$word2 == last_words[3],]$freq[1:5])*weight_i
        } else if(i==4) {
            word <- as.character(quagram[quagram$word1==last_words[2]
                                         & quagram$word2 == last_words[3]
                                         & quagram$word3 == last_words[4],]$word4[1:5])
            freq<-as.integer(quagram[quagram$word1==last_words[2] 
                                     & quagram$word2 == last_words[3]
                                     & quagram$word3 == last_words[4],]$freq[1:5])*weight_i
        } 
        top5 <- cbind(word,freq)
        top_words <- rbind(top_words,top5)
        
    }
    top_words <- top_words[!duplicated(top_words$word), ]
    top_words <- top_words[!is.na(top_words$word), ]
    top_words <- top_words[1:5,]
    return(top_words$word)
}


shinyServer(function(input, output) {
    ans1 <- reactive({
        next_word(input$user_input)[1]})
    ans2 <- reactive({
        next_word(input$user_input)[2]})
    ans3 <- reactive({
        next_word(input$user_input)[3]})
    ans4 <- reactive({
        next_word(input$user_input)[4]})
    ans5 <- reactive({
        next_word(input$user_input)[5]})
    
    output$guess1 <- ans1
    output$guess2 <- ans2
    output$guess3 <- ans3
    output$guess4 <- ans4
    output$guess5 <- ans5
    
    
})

    