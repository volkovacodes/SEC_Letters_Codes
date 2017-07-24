require(tm)
require(topicmodels)
require(data.table)
load("./Projects/SEC Letter Project/Data After Review/Rdata/lda_20170710_8_topics.RData")
### word matrix
words <- as.matrix(dtm)
### word's frequency
p_w <- colSums(words)/sum(words)
### average topic frequency
p_T <- colMeans(as.data.frame(ldaOut@gamma))

### weight of word in each topic
require(tidytext)
tmp <- data.table(tidy(ldaOut, matrix = "beta"))

p_w_T <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
for(i in 1:k) p_w_T[,i] <- tmp[topic == i]$beta

p_T_w <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
for(i in 1:k) p_T_w[i] <- p_w_T[i]*p_T/(p_w)
items <- readRDS("./Projects/SEC Letter Project/Data After Review/Additional Data/Item Text/25. Critical Accounting Policies and Estimates.rds")
item_docs <- Corpus(VectorSource(items))
item_docs <- tm_map(item_docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
item_docs <- tm_map(item_docs, stripWhitespace)
require(SnowballC)
item_docs <- tm_map(item_docs,stemDocument)

item_dtm <- DocumentTermMatrix(item_docs)
item_post <- posterior(object = ldaOut, newdata = item_dtm)

df <- NULL
for(i in 1:k)
{
  print(i)
  print(mean(item_post$topics[,i]))
 df <- rbind(df, data.table(topic = i, weight = mean(item_post$topics[,i])))
}

test.topics <- apply(item_post$topics, 1, which.max)
table(test.topics)

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  ## Required packages
  require(topicmodels)
  require(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)
  
  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = doc_term$dimnames$Terms,
                            Freq = colSums(as.matrix(doc_term)))
  #freq_matrix <- data.frame(ST = colnames(temp_frequency), Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

lda_visual <- topicmodels_json_ldavis(ldaOut, docs, dtm)
serVis(lda_visual)



lda_visual
