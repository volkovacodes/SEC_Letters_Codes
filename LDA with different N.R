require(tm)
require(topicmodels)
require(data.table)
#upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")

require(DBI)
require(RSQLite)
db_name <- "./Projects/SEC Letter Project/Data After Review/ipo_letters.sqlite"
con = dbConnect(SQLite(), dbname = db_name)
files <- dbGetQuery(con, 'SELECT CIK, out_filename, Body FROM ipoletters')

#upload <- upload[!duplicated(CIK)]
#create corpus from vector
docs <- Corpus(VectorSource(files$Body))
#remove stopwords
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#Stem document
#require(SnowballC)
#docs <- tm_map(docs,stemDocument)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames
rownames(dtm) <- files$out_filename
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk

require(topicmodels)
#Set parameters for Gibbs sampling
burnin <- 100
iter <- 200
thin <- 200
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


for(k in 2:30)
{
  print(k)
  #Run LDA using Gibbs sampling
  start <- Sys.time()
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(burnin = burnin, iter = iter, thin=thin, keep = iter))
  end <- Sys.time()
  
  ldaOut.topics <- as.matrix(topics(ldaOut))
  require(data.table)
  topicProbabilities <- as.data.table(ldaOut@gamma)
  #terms(ldaOut)
  #ldaOut.terms <- as.data.frame(topicmodels::terms(ldaOut, 30), stringsAsFactors = FALSE)
  print(end - start)
  save(list = ls(all.names = TRUE), file = paste0("./Projects/SEC Letter Project/Data After Review/Additional Data/N_LDA/n_topics_no_stem", k,".RData"))
}