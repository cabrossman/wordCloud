require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
require(tidyverse)




#wordcloud function --------------------------------------------
wordCloud <- function(df, fname, stemming = FALSE, tfidf =FALSE){
  #create a corpus
  corpus <- Corpus(DataframeSource(df))
  #convert corpus to plain text doc
  corpus <- tm_map(corpus, PlainTextDocument)
  #remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Convert the text to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Eliminate extra white spaces
  corpus <- tm_map(corpus, stripWhitespace)
  #remove stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #ap.corpus <- tm_map(ap.corpus, tolower)
  
  #remove special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "@")
  corpus <- tm_map(corpus, toSpace, "\\|")
  
  if(stemming == TRUE){
    #stemming
    corpus <- tm_map(corpus, stemDocument)
  }
  
  if(tfidf == TRUE){
    ap.tdm <- TermDocumentMatrix(corpus)
    ap.m <- as.matrix(ap.tdm)
    ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
    ap.d <- data.frame(word = names(ap.v),freq=ap.v)
    table(ap.d$freq)
    pal2 <- brewer.pal(8,"Dark2")
    png(fname, width=640,height=400)
    wordcloud(ap.d$word,
              ap.d$freq, 
              scale=c(8,.2),
              min.freq=3,
              #max.words=Inf,
              max.words=200,
              random.order=FALSE, 
              rot.per=.15, 
              colors=pal2)
    dev.off()
  } else {
    #word cloud without stemming
    pal2 <- brewer.pal(8,"Dark2")
    png(fname, width=640,height=400)
    wordcloud(corpus, 
              scale=c(8,.2),
              min.freq=3,
              max.words=200, 
              random.order=FALSE, 
              rot.per=.15, 
              colors=pal2)
    dev.off()
  }
  
  
  
}



#Read in text file---------------------------------------------------
t = readLines('CorporateNarrativeInterviews-forwordchart.txt')
#Split on new lines---------------------------------------------------
t1 = unlist(strsplit(t,'/n'))

#Remove non UTF chars---------------------------------------------------
Encoding(t1) <- "UTF-8"
t2 <- iconv(t1, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''


questions <- c("Why are we here?","What can we accomplish?","How should we connect with each other in order to accomplish this?",
               "Why am I proud of what I do here?","What is the reason for our creation? Why did our business even start?",
               "What would you name our company and why?","Who are the founders and what are their stories?",
               "What's a day like in the office?","What are one or two things that most people probably don't know about the company?",
               "If your company were a pizza topping what would it be and why?","What animal best describes our company?",
               "What other things do you want to add?")

Encoding(questions) <- "UTF-8"
questions <- iconv(questions, "UTF-8", "UTF-8",sub='') ## replace any non UTF-8 by ''

question_string <- paste(questions, collapse = "|")


t3 <- as.data.frame(t2,stringsAsFactors = FALSE) %>% select(responses = t2) %>% filter(!grepl(question_string,responses,TRUE))

wordCloud(df = t3, fname = 'wc.png', stemming = FALSE, tfidf = FALSE)
wordCloud(df = t3, fname = 'wc_tfidf.png', stemming = FALSE, tfidf = TRUE)
wordCloud(df = t3, fname = 'wc_stem.png', stemming = TRUE, tfidf = FALSE)
wordCloud(df = t3, fname = 'wc_stem_tfidf.png', stemming = TRUE, tfidf = TRUE)