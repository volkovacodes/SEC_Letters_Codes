### this is the version I use to determine number of topics
### Date: 20170725
require(tm)
require(topicmodels)
require(data.table)
require(tidytext)
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
docs <- Corpus(VectorSource(upload$Clean_Body))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- upload$out_filename

dir <- "./Projects/SEC Letter Project/Data After Review/Additional Data/N_LDA/"
files <- list.files(dir)

for(n in 2:15)
{
  k <- n
  
  name <- paste0(dir, "n_topics_", n, ".RData")
  load(name)
  ### word matrix
  words <- as.matrix(dtm)
  ### word's frequency
  p_w <- colSums(words)/sum(words)
  ### average topic frequency
  p_T <- colMeans(as.data.frame(ldaOut@gamma))
  
  tmp <- data.table(tidy(ldaOut, matrix = "beta"))
  p_w_T <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
  
  for(i in 1:k) p_w_T[,i] <- tmp[topic == i]$beta
  
  p_T_w <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
  for(i in 1:k) p_T_w[i] <- p_w_T[i]*p_T/(p_w)
  
  tmp <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
  for(i in 1:k) tmp[i] <- p_T_w[i]*log(p_T_w[i]/p_T[i])
  
  distinctive_w <- tmp
  saliency_w <- p_w*distinctive_w
  rownames(saliency_w) <- labels(p_w)
  
  ## LIMIT 5
  LIMIT <- 7
  weighted_saliency <- 0
  for(i in 1:k)
  {
    top_sal <- sort(saliency_w[,i], decreasing = T)
    weighted_saliency <- weighted_saliency + sum(top_sal[1:LIMIT])*(mean(p_T))*3957
  }

  x <- cor(p_w_T)
  x <- max(x[x != 1])
  print(sprintf("with %02d topic correlation is %g and saliency is %g",  n ,  x, weighted_saliency))
}

