SMS Spam Detection using Naive Bayes Algorithm

You can download dataset from https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection or Dataset is provided in this folder itself.

This problem comes under classification problem.To solve this problem, there are many algorithms, but I specifically concentrated on Naive Bayes and SVM.

First you need to convert message text into tokens and then apply Naive Bayes algorithm

Following Libraries used while developing algorithm in R:

library(NLP)

library(tm) ## for text mining

library(SnowballC) ## provides wordstem() function

library(wordcloud) ## for visualizing text data in the form of cloud

library(e1071)      

library(gmodels)


