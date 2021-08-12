## Server.R file for my Shiny App
## My github repo: https://github.com/circulaic/coursera-datasci-capstone
## Created by Genevieve Beart, 12 August 2021

suppressWarnings(library(tm))
suppressWarnings(library(shiny))

#=============LOADING=========================#
## Load in the saved n-gram data files:
#load("onegrams.RData"); ##loads in data.frame sub_onegrams
load("twograms.RData"); ##loads in data.frame sub_twograms
load("threegrams.RData"); ##loads in data.frame sub_threegrams
load("fourgrams.RData"); ##loads in data.frame sub_fourgrams
##load("fivegrams.RData"); ##loads in data.frame sub_fivegrams

#===========CLEANING=====================#
#Simplfying n-gram data for easier matching
#Fivegrams
##dd<-transform(sub_fivegrams, word=do.call(rbind, strsplit(fivegram, " ", fixed=TRUE)), stringsAsFactors=F) #split into words
##dd<- within(dd,  fourgram <- paste(word.1, word.2, word.3, word.4, sep=" ")) # join together first four words
##fivegram<-dd[,c(1,2,8,7)] #final data-frame for testing with full n-gram and last word separated
#Fourgrams
cc<-transform(sub_fourgrams, word=do.call(rbind, strsplit(fourgram, " ", fixed=TRUE)), stringsAsFactors=F) #split into words
cc<- within(cc,  threegram <- paste(word.1, word.2, word.3, sep=" ")) # join together first three words
fourgram<-cc[,c(1,2,7,6)] #final data-frame for testing with full n-gram and last word separated
#Three
bb<-transform(sub_threegrams, word=do.call(rbind, strsplit(trigram, " ", fixed=TRUE)), stringsAsFactors=F) #split into words
bb<- within(bb,  bigram <- paste(word.1, word.2, sep=" ")) # join together first three words
trigram<-bb[,c(1,2,6,5)] #final data-frame for testing with full n-gram and last word separated
#Two
aa<-transform(sub_twograms, word=do.call(rbind, strsplit(bigram, " ", fixed=TRUE)), stringsAsFactors=F) #split into words
aa<- within(aa,  unigram <- paste(word.1, sep=" ")) # join together first three words
bigram<-aa[,c(1,2,5,4)] #final data-frame for testing with full n-gram and last word separated

#=========FUNCTION====================#
###Create a function that can be used repeatedly to test input phrases
## Word Prediction: Back Off Algorithm
# Predict the next term of the user input sentence:
# 1. For the greatest accuracy prediction is done by first trying to match to the most words
## a. The "fivegrams" are checked first looking for a match - the first 4 words of the "fivegram" are the last 4 user inputted words
## b. If no match in a., The "fourgrams"/quadgrams are checked first looking for a match - first 3 words of quadgram are last 3 user inputted words
## c. If no match in b., The "threegrams"/trigrams are checked first looking for a match - first 2 words of the trigram are the last 2 user inputted words
## d. If no match in c., The "twograms"/bigrams are checked first looking for a match - first 1 word of the bigram is the last user inputted word
## e. Finally, if no matched in d., the most common "onegrams"/unigram is returned, i.e., "the"


next_word<-function(x){
    #Clean the input data
    x <- removePunctuation(x) #take out punctuation
    x <- tolower(x) #convert to lowercase
    x <- removeNumbers(x) #remove numbers
    x <- stripWhitespace(x) #remove extra whitespace
    x_words <- strsplit(x, " ")[[1]] # split string into unique words
    
    
   # message<-" " ## blank message to be written to to tell user which approach was used
    
    if (length(x_words)>=4) {
        x_w<-tail(x_words,4) #if the input is 4 or more words, take the last 4 words as input
        #matches5<-head(fivegram[grep(paste(x_w[1],x_w[2],x_w[3],x_w[4],sep=" "), fivegram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches4<-head(fourgram[grep(paste(x_w[2],x_w[3],x_w[4],sep=" ") , fourgram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches3<-head(trigram[grep(paste(x_w[3],x_w[4],sep=" ") , trigram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches2<-head(bigram[grep(paste(x_w[4],sep=" ") , bigram[,3]),], 5) ## return the 5 most likely matching n-grams
        
       if (nrow(matches4)>0) {
            word<-matches4[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 4-gram."
            return(word)
            #print(message)
            #print(matches4) 
            }    
        else if (nrow(matches3)>0) {
            word<-matches3[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 3-gram."
            return(word)
            #print(message)
           # print(matches3)
            }        
        else if (nrow(matches2)>0) {
            word<-matches2[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 2-gram."
            return(word)
            #print(message)
           # print(matches2) 
            }
        else {
            word<-"the"
            #message<-"No common match found in corpus - most common unigram 'the' returned."
            return(word)
            #print(message)
            }
    }
    else if (length(x_words)==3) {
        x_w<-tail(x_words,3) #if the input is 3 or more words, take the last 3 words as input
        matches4<-head(fourgram[grep(paste(x_w[1],x_w[2],x_w[3],sep=" ") , fourgram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches3<-head(trigram[grep(paste(x_w[2],x_w[3],sep=" ") , trigram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches2<-head(bigram[grep(paste(x_w[3],sep=" ") , bigram[,3]),], 5) ## return the 5 most likely matching n-grams
        if (nrow(matches4)>0) {
            word<-matches4[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 4-gram."
            return(word)
            #print(message)
            #print(matches4) 
            }    
        else if (nrow(matches3)>0) {
            word<-matches3[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 3-gram."
            return(word)
            #print(message)
            #print(matches3) 
            }        
        else if (nrow(matches2)>0) {
            word<-matches2[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
           # message<-"Most likely next word predicted using 2-gram."
            return(word)
            #print(message)
            #print(matches2) 
            }
        else {
            word<-"the"
           # message<-"No common match found in corpus - most common unigram 'the' returned."
            return(word)
            #print(message)
            }
        
    }
    else if (length(x_words)==2) {
        x_w<-tail(x_words,2) #if the input is 3 or more words, take the last 3 words as input
        matches3<-head(trigram[grep(paste(x_w[1],x_w[2],sep=" ") , trigram[,3]),], 5) ## return the 5 most likely matching n-grams
        matches2<-head(bigram[grep(paste(x_w[2],sep=" ") , bigram[,3]),], 5) ## return the 5 most likely matching n-grams
        if (nrow(matches3)>0) {
            word<-matches3[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 3-gram."
            return(word)
            #print(message)
            #print(matches3) 
            }        
        else if (nrow(matches2)>0) {
            word<-matches2[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 2-gram."
            return(word)
            #print(message)
            #print(matches2) 
            }
        else {
            word<-"the"
            #message<-"No common match found in corpus - most common unigram 'the' returned."
            return(word)
            #print(message)
            }
    } 
    
    else if (length(x_words)==1) {
        x_w<-tail(x_words,1) #if the input is 3 or more words, take the last 3 words as input
        matches2<-head(bigram[grep(paste(x_w[1],sep=" ") , bigram[,3]),], 5) ## return the 5 most likely matching n-grams
        if (nrow(matches2)>0) {
            word<-matches2[1,4] ##when there is an exact match (i.e., more than 0 matches) go for the first one (most likely n-gram)
            #message<-"Most likely next word predicted using 2-gram."
            return(word)
            #print(message)
            #print(matches2) 
            }
        else {
            word<-"the"
            #message<-"No common match found in corpus - most common unigram 'the' returned."
            return(word)
            #print(message)
            }
    } 
    else {
        word<-"the"
        #message<-"No common match found in corpus - most common unigram 'the' returned"
        return(word)
        #print(message)
        }
}

shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- next_word(input$inputString)
        #output$text2 <- renderText({message()})
        result
    });
    
    output$text1 <- renderText({
        input$inputString});
})