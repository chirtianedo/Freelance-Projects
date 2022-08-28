# This file follows up from the explorative phase in "lastName_TM_case_exploration.R file" to
# the construction of a naive bayes classification model.

library(tm)
library(dplyr)
########Loading data into Global Environment
testing_tweets<- read.csv("student_tm_case_score_data.csv")

training_tweets <- read.csv("student_tm_case_training_data.csv")

#SPlitting student_tm_case_training_data.csv into test and training data

library(caTools)
set.seed(100)

split = sample.split(training_tweets, SplitRatio =  0.7)
train_tweets = subset(training_tweets, split == TRUE)
test_tweets = subset(training_tweets , split == FALSE)
train_label = subset(training_tweets$label, split == T)
test_label= subset(training_tweets$label, split == F)

#DATA PREPROCESSING STEPS
#check for NA values
sum(is.na(test_tweets))
sum(is.na(train_tweets))
#Creating a training and test corpus data
corpus_docs_train <- Corpus(VectorSource(train_tweets$rawText))
corpus_docs_train[[1]][1]
train_tweets$label[1]

corpus_docs_test <- Corpus(VectorSource(test_tweets$rawText))

#create a function to get rid of special characters


#Effecting morecleaning procedures with tm() function
corpus_docs_train = tm_map(corpus_docs_train, PlainTextDocument)
corpus_docs_test =  tm_map(corpus_docs_test, PlainTextDocument)
corpus_docs_train = tm_map(corpus_docs_train, tolower)
corpus_docs_test =  tm_map(corpus_docs_test, tolower)
corpus_docs_train = tm_map(corpus_docs_train, removePunctuation)
corpus_docs_test =  tm_map(corpus_docs_test, removePunctuation)
corpus_docs_train = tm_map(corpus_docs_train, removeNumbers)
corpus_docs_test =  tm_map(corpus_docs_test, removeNumbers)
corpus_docs_train = tm_map(corpus_docs_train, removeWords, stopwords("english"))
######### brief visualization
wordcloud(corpus_docs_train, min.freq = 40, random.order = FALSE)

#Creating a documentmatrixdata
Dtm_training = DocumentTermMatrix(corpus_docs_train)
Dtm_test = DocumentTermMatrix(corpus_docs_test)


#Sparsingdata
Dtm_training = removeSparseTerms(Dtm_training, 0.995)
Dtm_test = removeSparseTerms(Dtm_test, 0.995)

######PREPARING DATA FOR THE NAIVE BAYES CLASSIFICATION
#APPLYING FREQUENCY TO TERMDOCMAT
Dtm_training = Dtm_training %>% findFreqTerms(5) %>% Dtm_training [ , .]
Dtm_test = Dtm_test %>% findFreqTerms(5) %>% Dtm_test [ , .]



#CONVERTING LABELS TO FACTORS
convert_counts<- function(x) {
  x<- ifelse(x>0, "Yes", "No")
}

Dtm_training <- Dtm_training %>% apply(convert_counts, MARGIN = 2)


Dtm_test <- Dtm_test %>% apply(convert_counts, MARGIN = 2)
#APPLYING THE NAIVE BAYES METHOD 
library(e1071)
test.tweet =naiveBayes(Dtm_training, train_label, laplace = 1)

test.predict = predict(test.tweet, Dtm_test)

#calculate accuracy of model using crosstable function from install.packages("gmodels")
library(gmodels)
CrossTable(test.predict, test_label, 
           prop.chisq = FALSE, chisq = FALSE,
           prop.t = FALSE, 
           dnn = c("predicted", "actual"))


#PREDICTING AND CLASSIFYING TEST SET OF "Student_tm_case_score_data.csv"
#testing_tweets<- read.csv("student_tm_case_score_data.csv")


score_data_pred<- Corpus(VectorSource(testing_tweets$rawText))


#PREPROCESS NEW DATA 
corpus_docs_mint = tm_map(score_data_pred, PlainTextDocument)
corpus_docs_mint =  tm_map(corpus_docs_mint, tolower)
corpus_docs_mint = tm_map(corpus_docs_mint, removePunctuation)
corpus_docs_mint =  tm_map(corpus_docs_mint, removeNumbers)
corpus_docs_mint = tm_map(corpus_docs_mint, removeWords,c( "@","rt","n/", "/", stopwords("english")))
corpus_docs_mint =  tm_map(corpus_docs_mint, stemDocument)




Dtm_score_data = DocumentTermMatrix(corpus_docs_mint)

inspect(Dtm_score_data)

######PREPARING DATA FOR THE NAIVE BAYES PREDICTION
#APPLYING FREQUENCY TO TERMDOCMAT

Dtm_score_data = Dtm_score_data %>% findFreqTerms(8) %>% Dtm_score_data[ , .]

#CONVERT
convert_counts<- function(x) {
  x<- ifelse(x>0, "Yes", "No")
}

Dtm_score_data <- Dtm_score_data %>% apply(convert_counts, MARGIN = 2)

language.predict<- predict(test.tweet, Dtm_score_data)

testing_tweets$label<- language.predict

head(testing_tweets)
#writing the predicted label into csv.file
write.csv( testing_tweets, file = "lastName_TM_scores.csv", row.names = TRUE, col.names = TRUE)   
