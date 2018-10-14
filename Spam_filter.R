## loading required NLP package

library(NLP)
library(tm)         ## for text mining
library(SnowballC)  ## provides wordstem() function
library(wordcloud)  ## for visualizing text data in the form of cloud
library(e1071)      ## for naive bayes implementation
library(gmodels)

# read file

sms_raw <- read.csv("sms_spam.csv", header = T, stringsAsFactors = F)

str(sms_raw)

## since it is the character vector (spam or ham) it is better to make a class as a factor

sms_raw$type <- factor(sms_raw$type)

## examine the structure and table for type colummn
str(sms_raw$type)

table(sms_raw$type)

## data cleaning and data preparation as it contain many uninterested word such and,but, or
## this can be done using tm (text mining package)

## accepting the text to spam detection
## VCorpus is the function that stores all the text data
## PCorpus is to get the access of permanently stored on harddisk
## vectorSource - to read the data from the file
## VCorpus function is in tm package


sms_corpus <- VCorpus(VectorSource(sms_raw$text))

print(sms_corpus)

## to view first 2 text sms

inspect(sms_corpus[1:2])

## To view the actual msg

as.character(sms_corpus[[1]])
as.character(sms_corpus[[2]])


## Applying lapply

lapply(sms_corpus[1:2], as.character)

## to count Hello, HELLO, HELlo to be counted as one word - Data cleansing for performing analysis
## for this we use tm_map() function

## content_transformer() to treat tolower() as a transformation function that can be used to access the corpus

## The content_transformer() function can be used to apply more sophisticated text processing and cleanup processes, such as grep
## pattern matching and replacement


sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

sms_corpus_clean

## checking for first text whether it has converted to lower case

as.character(sms_corpus[[1]])

as.character(sms_corpus_clean[[1]])

## now it has converted all to lower character

## lets remove numbers from msg as most of them will not provide any good insights

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

## now remove filler word such as to, and , but
## becoz they appear frequently and do not provide any useful insights
## for this we will use stopwords() function provided by tm package.

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

## remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

## to remove suffix ---- leaning, learns, learned -- converted to learn
## for this install snowballC

# install.packages("SnowballC")

wordStem()

wordStem(c("learn", "learned", "learning", "learns"))


sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

## Data preparation --- splitting text documents into words

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
#   tolower = TRUE, removeNumbers = TRUE,
#   stopwords = TRUE,
#   removePunctuation = TRUE,
#   stemming = TRUE
# ))

sms_dtm

# sms_dtm2

## Data preparation --- creating training and test datasets

sms_dtm_train <- sms_dtm[1:4169, ]

sms_dtm_test <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type

sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels))

prop.table(table(sms_test_labels))

## Visualizing text data --- word clouds

wordcloud(sms_corpus_clean, min.freq = 50, random.order = T)

spam <- subset(sms_raw, type == "spam")

ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))

wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


## creating indicator features for frequent words

findFreqTerms(sms_dtm_train, 5)

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

str(sms_freq_words)

sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]

sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                    convert_counts)


## training the model

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_pred <- predict(sms_classifier, sms_test)


CrossTable(sms_test_pred, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))


## improving model performance

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

## Finally, we'll compare the predicted classes to the actual classifications using a cross tabulation

CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


