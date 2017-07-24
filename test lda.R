require(tm)
require(topicmodels)
require(data.table)
#upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv",select = c("CIK","out_filename", "Clean_Body"))
#upload <- upload[!duplicated(CIK)]
#create corpus from vector
docs <- Corpus(VectorSource(upload$Clean_Body))
#remove stopwords
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Stem document
require(SnowballC)
docs <- tm_map(docs,stemDocument)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- upload$out_filename
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord[1:100]]

require(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 200
iter <- 1000
thin <- 1000
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 12

#Run LDA using Gibbs sampling
start <- Sys.time()
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
end <- Sys.time()

ldaOut.topics <- as.matrix(topics(ldaOut))
require(data.table)
topicProbabilities <- as.data.table(ldaOut@gamma)
#terms(ldaOut)
#ldaOut.terms <- as.data.frame(topicmodels::terms(ldaOut, 30), stringsAsFactors = FALSE)
print(end - start)

plot_topics <- function(folder)
{
  for(i in 1:k)
  {
    library(tidytext)
    ap_topics <- data.table(tidy(ldaOut, matrix = "beta"))
    topic1 <- ap_topics[topic == i]
    topic1 <- topic1[order(beta, decreasing = T)]
    topic1 <- topic1[1:100]
    library(wordcloud)
    
    png(filename=paste0(folder, i, ".png"))
    wordcloud(topic1$term, topic1$beta,  colors=brewer.pal(8, "Dark2"))
    dev.off()
  }
}

plot_topics(folder = "./Projects/SEC Letter Project/Data After Review/Additional Data/Topic Plots/20170629/")

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
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

lda_visual <- topicmodels_json_ldavis(ldaOut, docs, dtm)
print(end - start)

serVis(lda_visual)

View(topicProbabilities)
ldaOut@Dim

topicProbabilities[, mean(V1 > 0.05)]

for(i in 1:12)
{
  tmp <- topicProbabilities[,i, with = F]
  print(i)
  print(mean(tmp$V1, na.rm = T))
}

x <- 100*colMeans(topicProbabilities)
order(x)
save(list = ls(all.names = TRUE), file = "./Projects/SEC Letter Project/Data After Review/Rdata/lda_20170629.RData")

