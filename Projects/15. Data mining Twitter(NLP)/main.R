library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1017)
library(class)

#conect twitter with your own 4 inputs
setup_twitter_oauth(ckey, skey, token, sectoken)

money_tweets <- searchTwitter('money', n = 1000, lang = 'en')
#help(searchTwitter) for more options

#grab text data
money_text <- sapply(money_tweets, function(x) x$getText())

#clean text data
money_text <- iconv(money_text, 'UTF-8', 'ASCII')

money_corpus <- Corpus(VectorSource(money_text))

#document term matrix
term_doc_matrix <- TermDocumentMatrix(money_corpus, control = list(removePunctuation = TRUE, stopwords = c('money', stopwords('english')),
                                                                   removeNumbers = TRUE, tolower = TRUE))

#convert object into a matrix
term_doc_matrix <- as.matrix(term_doc_matrix)

#get word counts
word_freq <- sort(rowSums(term_doc_matrix), decreasing = T)
dm <- data.frame(word = names(word_freq), freq = word_freq)

#create wordcloud
wordcloud(dm$word, dm$freq, random.color = F, colors = brewer.pal(8, 'Dark2'))










