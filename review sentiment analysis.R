library(stringi)
library(tidytext) #sentilexicon
library(tidyr)
library(mgsub)
library(scales) 
library(stringr)
library(textdata)
library(widyr)
library(igraph)
library(pacman)
library(readxl)
library(rlist)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(wordcloud)
library(dplyr)

options(warn=-1)


review<- read_excel('hotel_reviews.xls')


text_mining<-function(hotel){
  hotel_subset<- review[review$`Hotel/Restaurant name`== hotel,]
  text<- hotel_subset$Review
  sentence<- gsub("http\\S+\\s*", "",text)
  sentence<- gsub("wasn[\u2019']t", "was not",sentence)
  sentence<- gsub("won[\u2019']t", "will not", sentence)
  sentence <- gsub("can[\u2019']t", "can not", sentence)
  sentence <- gsub("didn[\u2019']t", "did not", sentence)
  sentence <- gsub("don[\u2019']t", "do not", sentence)
  sentence <- gsub("I[\u2019']m", "I am", sentence)
  sentence <- gsub("[\u2019']ve", " have", sentence) 
  sentence <- gsub("[\u2019|']s", "", sentence)
  sentence <- gsub("[\u2019']re", " are", sentence)
  sentence <- gsub("[\u2019']ll", " will", sentence)
  sentence<- gsub("[[:punct:]]", "",sentence)
  sentence<- gsub("[[:punct:]]", "", sentence)
  sentence<- gsub("[[:digit:]]", "", sentence)
  sentence<- gsub("Flip Side", "", sentence) #hotel name
  sentence<- gsub("thai", "", sentence)
  sentence<- gsub("^ ", "", sentence)
  sentence<- gsub(" $", "", sentence)
  sentence<- tolower(sentence)
  corpus<- Corpus(VectorSource(sentence))
  stopwords_default<- stopwords("english")
  stopWordsNotDeleted<- c("not") 
  stopwords_new<- stopwords_default[! stopwords_default
                                    %in% stopWordsNotDeleted]
  corpus<- tm_map(corpus, removeWords, stopwords_new)
  corpus<- tm_map(corpus,stripWhitespace)
  clean_corpus<<- tm_map(corpus, stemDocument)
  inspect(clean_corpus[1])
  return(clean_corpus)
  
} 


multiplot<- function(x){
  corpus_dtm <- TermDocumentMatrix(x,control = list(minWordLength=c(1,Inf)))
  dtm_m <- as.matrix(corpus_dtm)
  dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
  dtm_freq <- data.frame(word = names(dtm_v),freq=dtm_v)
  #word_cor<-findAssocs(corpus_dtm, terms = findFreqTerms(corpus_dtm, lowfreq = 10), 
                       #corlimit = 0.3)
  barplot(dtm_freq[1:15,]$freq, las = 2, names.arg = dtm_freq[1:15,]$word,
          col ="red3", main =paste(hotel, "Top 15 frequent words", collapse = " "),
          ylab = "Word frequencies")
  set.seed(1234)
  wordcloud(words = dtm_freq$word, freq = dtm_freq$freq,min.freq = 1,
            max.words=60, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

  
  
 


sentiment_analysis<- function(x){
  bing_dict<-get_sentiments("bing")
  p_lex_bing<- bing_dict[bing_dict$sentiment== "positive", "word"]
  n_lex_bing<- bing_dict[bing_dict$sentiment=="negative", "word"]
  #Calculating the count of total positive and negative words in each review
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(x)
  for(i in 1:size){
    #All the words in current review
    corpus_words<- list(strsplit(x[[i]]$content, split = " "))
    #positive words in current review
    pos_count <-length(intersect(unlist(corpus_words), unlist(p_lex_bing)))
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_words), unlist(n_lex_bing)))
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  #Create a dataframe with all the positive and negative reviews
  #df<-data.frame(Review_Type= c("Postive","Negitive"),
                 #Count= c(total_pos_count ,total_neg_count ),
                # Percentage= c(overall_positive_percentage, overall_negative_percentage))
  Review<- c("polarity")
  df<- rbind(data.frame( Review, "count"=overall_positive_percentage, "type"="Positive"),
             data.frame( Review, "count"=overall_negative_percentage, "type"="Negative" ))
  
  p <- ggplot(df, aes(y=Review, x=count, fill=type))+ 
    geom_bar(position="stack", stat="identity", width = 0.3)+ 
    geom_text(data = df, aes(label = paste(round(count,2), "%", sep="")))+ 
    ggtitle(paste(hotel, "Polarity Score", collapse = " "))+
    scale_x_continuous(position = "top")
  P <- p + theme_bw()+
    theme(axis.title.y =element_blank(), axis.line.x.top = element_line("grey50"),panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    scale_fill_manual(values = c("springgreen4","red3"))
  
  return(P)
  
}

hotel_lst<- unique(review$`Hotel/Restaurant name`)
length(hotel_lst)
twenty_hotel_lst<- sample(hotel_lst, size=20, replace=F)
twenty_hotel_lst

for (hotel in twenty_hotel_lst){
  text_mining(hotel)
  print(sentiment_analysis(clean_corpus))
  multiplot(clean_corpus)
}

