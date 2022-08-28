# This project represents the exploratory analysis of politically affiliated.motivated
# tweets during a period of time. the latter file in this directory explores the
# use of a naiveBayes classification model to predict poltically motivated/affiliated 
# tweets in real time

library(tm)
library(ggplot2)
library(stringr)
library(wordcloud)
library(RColorBrewer)

#Exploring "student_tm_case_score_data.csv"
test_tweets<- read.csv("student_tm_case_score_data.csv")

Test_exp<- test_tweets$rawText
Test_exp<- Corpus(VectorSource(Test_exp))

#TEXT TRANSFORMATION/CLEANING
Test_exp <- tm_map(Test_exp, content_transformer(tolower)) #tranforms document to lower case
#Remove numbers 
Test_exp<- tm_map(Test_exp, removeNumbers)
#Remove english common stopwords (and 'utf8towcs')
Test_exp <- tm_map(Test_exp, removeWords,c("http", stopwords("english")))
#Remove punctuations
Test_exp<- tm_map( Test_exp, removePunctuation)
#Eliminate  extra white spaces
Test_exp<- tm_map(Test_exp, stripWhitespace)

#convert corpus into TermDocumentMatrix
dtm_exp <- TermDocumentMatrix(Test_exp)

dtm_m <- as.matrix(dtm_exp)

dtm_v<- sort(rowSums(dtm_m), decreasing = TRUE)

dtm_d<- data.frame(word = names(dtm_v), freq= dtm_v) 

##Generate Word Cloud
library(wordcloud)
set.seed(1234)

wordcloud(words= dtm_d$word, freq= dtm_d$freq, 
          min.freq = 1, max.words = 200, 
          random.order =FALSE, rot.per = 0.30, 
          colors = brewer.pal(8, "Dark2"), scale = c(3.5,0.25))

###############
#find frequent terms with the findFreqTerms function
frq_terms<-findFreqTerms(dtm_exp, lowfreq = 4)

#findAssocs helps us to observe terms and association with a particular topic 

political_flow<-findAssocs(dtm_exp, terms = "politics", corlimit = 0.2)

#plot a bar graph to give an idea of word frequency and count with ggplot

subset(dtm_d, freq> 50)  %>% ggplot(aes(word, freq)) + 
  geom_bar(stat= "identity",fill = "black", colour = "darkgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Exploring "student_tm_case_traning_data.csv"
training_tweets_exp <- read.csv("student_tm_case_training_data.csv")

#Explore labeled data
df<- training_tweets_exp

df1<-df %>% group_by(docID) %>% summarise(Total_political_discuss = sum(label))

df$label<-ifelse(df$label > 0, "Political tweet","Non-Political tweets")

ggplot(df, aes(label)) + geom_bar((aes(fill ="orange"))) + 
  ggtitle("Political Discourse") + labs( y = "Tweet count", x = "type of tweet")


#converting raw text to corpus
tt_exp <- training_tweets_exp$rawText

tt_exp<- Corpus(VectorSource(tt_exp))

tt_exp = tm_map(tt_exp, PlainTextDocument)
tt_exp =  tm_map(tt_exp, tolower)
tt_exp = tm_map(tt_exp, removePunctuation)
tt_exp =  tm_map(tt_exp, removeNumbers)
tt_exp = tm_map(tt_exp, removeWords, stopwords("english"))
tt_exp =  tm_map(tt_exp, stemDocument)


#converting corpus to termdocmatrix
Dtm_train_data = TermDocumentMatrix(tt_exp)

g <- as.matrix(Dtm_train_data)

n<- sort(rowSums(g), decreasing = TRUE)

j<- data.frame(word = names(n), freq= n) 

#Generate Word Cloud
#install.packages ("wordcloud2") makes data more uniques and easily manipulated
library(wordcloud2)
set.seed(1234)

wordcloud2(data= j, size = 0.7, 
          color = "random-dark")

#plot a bar graph to give an idea of word frequency and count with ggplot

head(j, 10)  %>% ggplot(aes(word, freq)) + 
  geom_bar(stat= "identity",fill = "darkred", colour = "darkgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Count")


#Exploring both data files together
#binding datasets with the rbind function
total_docs<- rbind(test_tweets, training_tweets[,-3])

text<- total_docs$rawText
text<- iconv(text, from="UTF-8", to = "")
docs<- Corpus(VectorSource(text))

inspect(docs)


#TEXT TRANSFORMATION/CLEANING
docs <- tm_map(docs, content_transformer(tolower)) #tranforms document to lower case
#Remove numbers 
docs<- tm_map(docs, removeNumbers)
#Remove english common stopwords (and 'utf8towcs')
docs <- tm_map(docs, removeWords,c("http", stopwords("english")))
#Remove punctuations
docs<- tm_map( docs, removePunctuation)
#Eliminate  extra white spaces
docs<- tm_map(docs, stripWhitespace)


##Build a TermDocumentMatrix
dtm<- TermDocumentMatrix(docs)

m<- as.matrix(dtm)

v<- sort(rowSums(m), decreasing = TRUE)

d<- data.frame(word = names(v), freq= v)

##Generate Word Cloud
set.seed(1234)
wordcloud(words= d$word, freq= d$freq, 
          min.freq = 1, max.words = 200, 
          random.order =FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

###############find frequent terms

frq_terms<-findFreqTerms(dtm, lowfreq = 4)

findAssocs(dtm, terms = "politics", corlimit = 0.2)

head(d, 10)  %>% ggplot(aes(word, freq)) + 
  geom_bar(stat= "identity",fill = "blue", colour = "darkgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Word")


